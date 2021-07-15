package codecs.xml

import codecs.internal.Producty
import codecs.xml.internal.{Attributes, Namespaces}
import scalaz.Semigroup

import scala.{xml => X}


sealed trait Xml {
  override def toString: String = {
    val pretty = new X.PrettyPrinter(100, 2)

    pretty.formatNodes(toElem)
  }

  def namespaces: Namespaces
  def attributes: Attributes
  def elements: List[Xml.Element]

  def withName(name: Name): Xml

  def toElem: X.Elem

  def +(other: Xml): Xml
}

object Xml {
  implicit val semigroup: Semigroup[Xml] = Semigroup.instance(_ + _)

  val Text: Producty[Xml, String] = Producty[Xml, String](text => Xml.Children(text = List(text))) {
    case Children(_, _, _, List(text))             => text
    case Element(_, Children(_, _, _, List(text))) => text
  }

  def apply(node: X.Node): Xml = node match {
    case X.Text(text)                       => Text(text)
    case e@X.Elem(_, _, _, _, X.Text(text)) => Text(text)
    case e: X.Elem                          => Element(e)
    case other => ???
  }

  object Children {
    def apply(xmls: List[Xml]): Children = xmls.foldLeft(Children()) {
      case (children, element: Element) => children.add(elements = List(element))
      case (children, kids: Children)   => kids.addTo(children)
    }

    def apply(xml: Xml): Children = xml match {
      case children: Children => children
      case element: Element   => Children(List(element))
    }
  }

  case class Children(
    namespaces: Namespaces = Namespaces(),
    elements: List[Element] = Nil,
    attributes: Attributes = Attributes(),
    text: List[String] = Nil
  ) extends Xml {
    def withName(name: Name): Children = Children(
      namespaces.withName(name),
      elements.map(_.withName(name)),
      attributes.withName(name)
    )

    def +(other: Xml): Xml = {
      val result = other match {
        case Element(name, children) => Element(name, addTo(children))
        case children: Children      => children.addTo(this)
      }

      result
    }

    def addTo(other: Children): Children =
      other.add(namespaces, elements, attributes, text)

    def add(namespaces: Namespaces = Namespaces(), elements: List[Element] = Nil, attributes: Attributes = Attributes(), text: List[String] = Nil): Children = {
      val result = Children(this.namespaces + namespaces, this.elements ++ elements, this.attributes + attributes, this.text ++ text)

      result
    }

    def element(name: Name): X.Elem = {
      val metaData: X.MetaData = attributes.toMetaData
      val childNodes: List[X.Node] = elements.flatMap(_.toElem) ++ text.map(X.Text(_))

      val binding: X.NamespaceBinding = namespaces.toBinding

      name match {
        case Name.Unprefixed(label)       => X.Elem(null,   label, metaData, binding, childNodes.isEmpty, childNodes: _*)
        case Name.Prefixed(prefix, label) => X.Elem(prefix, label, metaData, binding, childNodes.isEmpty, childNodes: _*)
      }
    }


    override def toString: String = {
      val pretty = new X.PrettyPrinter(100, 2)

      pretty.formatNodes(element("unknown"))
    }

    def toElem: X.Elem = {
      elements match {
        case List(single) => single.toElem
        case _ => sys.error("boom")
      }
    }
  }

  object Element {
    def apply(elem: X.Elem): Element = Element(Name(elem.prefix, elem.label), Children(
      Namespaces(elem.scope),
      elem.child.collect {
        case e: X.Elem => apply(e)
      }.toList,
      Attributes(elem.attributes),
      elem.child.collect {
        case X.Text(text) => text
      }.toList
    ))
  }

  case class Element(name: Name, children: Children) extends Xml {
    def namespaces: Namespaces = children.namespaces
    def attributes: Attributes = children.attributes
    def elements: List[Element] = children.elements

    def +(other: Xml): Xml = {
      val result = other match {
        case element:       Element  => Children(elements = List(this, element))
        case otherChildren: Children => copy(children = otherChildren.addTo(children))
      }

      result
    }


    def withName(newName: Name): Element = Element(newName, children)

    def toElem: X.Elem = children.element(name)
  }
}
