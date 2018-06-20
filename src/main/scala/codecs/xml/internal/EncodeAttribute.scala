package codecs.xml.internal

trait EncodeAttribute[A] {
  def encode(a: A): String

  def contramap[B](g: B => A): EncodeAttribute[B] = EncodeAttribute.Contramappeed(this, g)
}

object EncodeAttribute extends EncodeAttributeInstances {
  private case class Contramappeed[A, B](from: EncodeAttribute[A], g: B => A) extends EncodeAttribute[B] {
    def encode(b: B): String = from.encode(g(b))
  }

  def encode[A: EncodeAttribute](a: A): String = of[A].encode(a)

  def of[A](implicit E: EncodeAttribute[A]): EncodeAttribute[A] = E
}

trait EncodeAttributeInstances {
  implicit val stringEncodeAttribute: EncodeAttribute[String] = value => value
}


trait EncodeNAttributes[A] {
  def encode(a: A): List[String]
}

object EncodeNAttributes extends LowPriorityEncodeNAttributes {
  implicit def encodeList[A: EncodeAttribute]: EncodeNAttributes[List[A]] =
    _.map(EncodeAttribute.of[A].encode)

  implicit def encodeOption[A: EncodeAttribute]: EncodeNAttributes[Option[A]] =
    _.map(EncodeAttribute.of[A].encode).toList
}

trait LowPriorityEncodeNAttributes {
  implicit def encodeOne[A: EncodeAttribute]: EncodeNAttributes[A] =
    a => List(EncodeAttribute.of[A].encode(a))
}