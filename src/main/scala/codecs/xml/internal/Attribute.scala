package codecs.xml.internal

import codecs.xml.Name
import codecs.{DecodeResult, Error}

import scala.annotation.tailrec
import scala.{xml => X}


case class Attribute(name: Name, value: String) {
  def toMeta(next: X.MetaData): X.MetaData = name match {
    case Name.Unprefixed(unprefixed)   => new X.UnprefixedAttribute(unprefixed, X.Text(value), next)
    case Name.Prefixed(prefix, suffix) => new X.PrefixedAttribute(prefix, suffix, X.Text(value), next)
  }
}

object Attributes {
  def apply(metaData: X.MetaData): Attributes =
    apply(metaData.foldAttributes[List[Attribute]](List.empty[Attribute])(key ⇒ value ⇒ list ⇒ Attribute(key, value) :: list))

  def apply(attributes: List[Attribute]): Attributes = Attributes(attributes.groupBy(_.name).map {
    case (name, attrs) => name -> attrs.map(_.value)
  })

  def fromList(attributes: List[Attribute]): X.MetaData = attributes.foldLeft(X.Null: X.MetaData) {
    case (acc, attribute@Attribute(key, value)) ⇒ attribute.toMeta(acc)
  }

  private implicit class MetaDataOps(val self: X.MetaData) extends AnyVal {
    def foldAttributes[A](zero: A)(f: String ⇒ String ⇒ A ⇒ A): A = {
      @tailrec def recurse(acc: A, current: X.MetaData): A = current match {
        case X.PrefixedAttribute(prefix, key, X.Text(value), next) ⇒ recurse(f(s"$prefix:$key")(value)(acc), next)
        case X.UnprefixedAttribute(key, X.Text(value), next)  ⇒ recurse(f(key)(value)(acc), next)
        case X.Null ⇒ acc
      }

      recurse(zero, self)
    }
  }
}

case class Attributes(values: Map[Name, List[String]] = Map()) {
  def withName(newName: Name): Attributes = Attributes(values.map {
    case (_, valuesForName) => newName -> valuesForName
  })

  def toMetaData: X.MetaData = values.foldLeft(X.Null: X.MetaData) {
    case (acc, (name, valuesForName)) => valuesForName.foldLeft(acc) {
      case (md, value) => Attribute(name, value).toMeta(md)
    }
  }

  def +(other: Attributes): Attributes = Attributes(
    (values.keySet union other.values.keySet).map(name => {
      name -> (values.getOrElse(name, Nil) ++ other.values.getOrElse(name, Nil))
    }).toMap
  )

  def get(name: Name): DecodeResult[List[String]] =
    DecodeResult.Ok(values.getOrElse(name, Nil))

  override def toString: String = values.map {
    case (key, value) => s"""$key = "$value""""
  }.mkString(", ")
}

case class Namespaces(values: Map[Name, String] = Map()) {
  def withName(newName: Name): Namespaces = Namespaces(values.map {
    case (_, value) => newName -> value
  })

  def toBinding: X.NamespaceBinding = sorted.reverse.foldLeft(X.TopScope: X.NamespaceBinding) {
    case (acc, (Name.Unprefixed("xmlns"), value))  => X.NamespaceBinding(null, value, acc)
    case (acc, (Name.Unprefixed(label), value))  => X.NamespaceBinding(label, value, acc)
    case (acc, (Name.Prefixed(_, label), value)) => X.NamespaceBinding(label, value, acc)
  }

  def +(other: Namespaces): Namespaces = Namespaces(values ++ other.values)

  def add(name: Name, value: String): Namespaces = {
    val prefixed = name match {
      case Name.Unprefixed("xmlns") => name
      case Name.Unprefixed(other) => Name.Prefixed("xmlns", other)
      case other => other
    }

    Namespaces(values + ((prefixed, value)))
  }

  def get(name: Name): DecodeResult[String] =
    DecodeResult.fromOption(values.get(name), Error(s"No such namespace: $name, available: ${names.mkString(", ")}"))

  def names: List[Name] = values.keySet.toList.sorted

  private def sorted: List[(Name, String)] =
    values.toList.sortBy(_._1)
}

object Namespaces {
  def apply(binding: X.NamespaceBinding): Namespaces =
    binding.fold(Namespaces())(prefix => uri => _.add(prefix, uri))

  private implicit class NamespaceBindingOps(val self: X.NamespaceBinding) extends AnyVal {
    def fold[A](zero: A)(f: Name ⇒ String ⇒ A ⇒ A): A = {
      @tailrec def recurse(acc: A, current: X.NamespaceBinding): A = current match {
        case X.TopScope                            ⇒ acc
        case X.NamespaceBinding(null, uri, next) ⇒ recurse(f("xmlns")(uri)(acc), next)
        case X.NamespaceBinding(prefix, uri, next) ⇒ recurse(f(prefix)(uri)(acc), next)
      }

      recurse(zero, self)
    }
  }

}