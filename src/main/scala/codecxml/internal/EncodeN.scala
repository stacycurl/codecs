package codecxml.internal

import codecxml.{Encode, Name, Xml}

trait EncodeN[A] {
  def encode(name: Name, a: A): Xml

  def contramap[B](g: B => A): EncodeN[B] = EncodeN.Contramapped(this, g)
}

object EncodeN extends EncodeNInstances {
  private case class Contramapped[A, B](from: EncodeN[A], g: B => A) extends EncodeN[B] {
    def encode(name: Name, b: B): Xml = from.encode(name, g(b))
  }
}

trait EncodeNInstances extends LowPriorityEncodeN {
  implicit def encodeList[A: Encode]: EncodeN[List[A]] =
    (name: Name, as: List[A]) => Xml.Children(as.map(Encode.of[A].encode(name, _)))

  implicit def encodeOption[A: Encode]: EncodeN[Option[A]] =
    (name: Name, oa: Option[A]) => Xml.Children(oa.map(Encode.of[A].encode(name, _)).toList)
}

trait LowPriorityEncodeN {
  implicit def encodeOne[A: Encode]: EncodeN[A] =
    (name: Name, a: A) => Xml.Children(Encode.of[A].encode(name, a))
}