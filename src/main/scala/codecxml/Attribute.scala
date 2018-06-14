package codecxml

import scala.annotation.tailrec
import scala.{xml => X}

case class Attribute(name: String, value: String)

object Attribute {
  private implicit class MetaDataOps(val self: X.MetaData) extends AnyVal {
    def foldAttributes[A](zero: A)(f: String ⇒ String ⇒ A ⇒ A): A = {
      @tailrec def recurse(acc: A, current: X.MetaData): A = current match {
        //              case PrefixedAttribute(_, key, Text(value), next) ⇒ recurse(f(key)(value)(acc), next)
        case X.UnprefixedAttribute(key, X.Text(value), next)  ⇒ recurse(f(key)(value)(acc), next)
        case X.Null ⇒ acc
      }

      recurse(zero, self)
    }
  }

  def toMap(attributes: X.MetaData): Map[String, String] =
    attributes.foldAttributes[Map[String, String]](Map())(key ⇒ value ⇒ map ⇒ map + ((key, value)))

  def fromList(attributes: List[Attribute]): X.MetaData = attributes.foldLeft(X.Null: X.MetaData) {
    case (acc, Attribute(key, value)) ⇒ new X.UnprefixedAttribute(key, X.Text(value), acc)
  }
}
