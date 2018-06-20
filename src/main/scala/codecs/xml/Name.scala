package codecs.xml

import scala.language.implicitConversions

sealed trait Name {
  def matches(prefix: String, label: String): Boolean

  def isPrefixed: Boolean
}

object Name {
  def apply(prefix: String, label: String): Name = if (prefix == null) Unprefixed(label) else Prefixed(prefix, label)

  implicit val ordering: Ordering[Name] =
    Ordering.Tuple2[Boolean, String].on[Name](name => (name.isPrefixed, name.toString))

  implicit def stringToName(name: String): Name = {
    if (name == null) {
      println("wtf")
    }

    name.split(":").toList match {
      case prefix :: value :: Nil => Prefixed(prefix, value)
      case _                      => Unprefixed(name)
    }
  }

  def Placeholder: Name = Unprefixed("placeholder")

  case class Unprefixed(value: String) extends Name {
    def isPrefixed: Boolean = false

    def matches(prefix: String, label: String): Boolean = label == value

    override def toString: String = value
  }

  case class Prefixed(prefix: String, value: String) extends Name {
    def isPrefixed: Boolean = true

    def matches(prefix: String, label: String): Boolean = this.prefix == prefix && value == label

    override def toString: String = s"$prefix:$value"
  }
}