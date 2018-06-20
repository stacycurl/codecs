package codecs.xml.internal

import codecs.xml.{EncodeXml, Xml}

trait EncodeN[A] {
  def encode(a: A): Xml

  def contramap[B](g: B => A): EncodeN[B] = EncodeN.Contramapped(this, g)
}

object EncodeN extends EncodeNInstances {
  private case class Contramapped[A, B](from: EncodeN[A], g: B => A) extends EncodeN[B] {
    def encode(b: B): Xml = from.encode(g(b))
  }
}

trait EncodeNInstances extends LowPriorityEncodeN {
  implicit def encodeList[A: EncodeXml]: EncodeN[List[A]] =
    (as: List[A]) => Xml.Children(as.map(EncodeXml.of[A].encode(_)))

  implicit def encodeOption[A: EncodeXml]: EncodeN[Option[A]] =
    (oa: Option[A]) => Xml.Children(oa.map(EncodeXml.of[A].encode(_)).toList)
}

trait LowPriorityEncodeN {
  implicit def encodeOne[A: EncodeXml]: EncodeN[A] =
    (a: A) => Xml.Children(EncodeXml.of[A].encode(a))
}
