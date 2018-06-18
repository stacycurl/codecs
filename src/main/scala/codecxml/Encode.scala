package codecxml

import scala.reflect.ClassTag


trait Encode[A] {
  def encode(a: A)(implicit C: ClassTag[A]): Xml = encode(Encode.simpleClassNameOf[A], a)
  def encode(name: Name, a: A): Xml

  def contramap[B](g: B => A): Encode[B] = Encode.Contramapped(this, g)

  def &&[B](other: Encode[B]): Encode[(A, B)] = Encode.Pair(this, other)
}

object Encode extends EncodeElemInstances {
  def encode[A: Encode: ClassTag](a: A): Xml = of[A].encode(a)
  def encode[A: Encode](name: Name, a: A): Xml = of[A].encode(name, a)

  def of[A](implicit E: Encode[A]): Encode[A] = E

  def simpleClassNameOf[A: ClassTag]: String =
    implicitly[ClassTag[A]].runtimeClass.getSimpleName

  private case class Contramapped[A, B](from: Encode[A], g: B => A) extends Encode[B] {
    def encode(name: Name, b: B): Xml = from.encode(name, g(b))
  }

  private case class Pair[A, B](lhs: Encode[A], rhs: Encode[B]) extends Encode[(A, B)] {
    def encode(name: Name, ab: (A, B)): Xml = lhs.encode(name, ab._1) + rhs.encode(name, ab._2)
  }
}

trait EncodeElemInstances {
  implicit val stringEncodeElem: Encode[String] =
    (name: Name, value: String) => {
      Xml.Element(name, Xml.Children(text = List(value)))
    }

  implicit val booleanEncodeElem: Encode[Boolean] =
    stringEncodeElem.contramap[Boolean](_.toString)
}
