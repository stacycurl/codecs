package codecxml

import codecxml.internal.{Attributes, Namespaces}

import scala.{xml => X}


sealed trait Xml {
  override def toString: String = {
    val pretty = new X.PrettyPrinter(100, 2)

    pretty.formatNodes(toNode)
  }

  def toNode: X.Node

  def +(other: Xml): Xml
}

object Xml {
  object Children {
    def apply(xmls: List[Xml]): Children = {
      val res = xmls.foldLeft(Children()) {
        case (children, element: Element) => children.add(elements = List(element))
        case (children, kids: Children)   => kids.addTo(children)
      }

      res
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
      val childNodes: List[X.Node] = elements.flatMap(_.toNode) ++ text.map(X.Text(_))

      val binding: X.NamespaceBinding = namespaces.toBinding

      name match {
        case Name.Unprefixed(label)       => X.Elem(null,   label, metaData, binding, childNodes.isEmpty, childNodes: _*)
        case Name.Prefixed(prefix, label) => X.Elem(prefix, label, metaData, binding, childNodes.isEmpty, childNodes: _*)
      }
    }


    override def toString: String = {
      val pretty = new X.PrettyPrinter(100, 2)

      pretty.formatNodes(element("parent"))
    }

    def toNode: X.Node = {
      elements match {
        case List(single) => single.toNode
        case _ => X.Group(Seq.empty)
      }
    }
  }

  case class Element(name: Name, children: Children) extends Xml {
    def +(other: Xml): Xml = {
      val result = other match {
        case element:       Element  => Children(elements = List(this, element))
        case otherChildren: Children => copy(children = otherChildren.addTo(children))
      }

      result
    }

    def toNode: X.Node= children.element(name)
  }
}
