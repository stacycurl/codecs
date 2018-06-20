package codecs.xml.internal

import codecs.DecodeResult

trait DecodeAttribute[A] {
  def decode(attribute: String): DecodeResult[A]

  def map[B](f: A => B): DecodeAttribute[B] = DecodeAttribute.Mapped(this, f)
}

object DecodeAttribute extends DecodeAttributeInstances {
  def decode[A: DecodeAttribute](attribute: String): DecodeResult[A] = of[A].decode(attribute)

  def of[A](implicit D: DecodeAttribute[A]): DecodeAttribute[A] = D

  private case class Mapped[A, B](from: DecodeAttribute[A], f: A => B) extends DecodeAttribute[B] {
    def decode(attribute: String): DecodeResult[B] = from.decode(attribute).map(f)
  }
}

trait DecodeAttributeInstances {
  implicit val stringDecodeAttribute: DecodeAttribute[String] = value => DecodeResult.Ok(value)
}

trait DecodeNAttributes[A] {
  def decode(attributes: List[String]): DecodeResult[A]

  def map[B](f: A => B): DecodeNAttributes[B] = DecodeNAttributes.Mapped(this, f)
}

object DecodeNAttributes extends DecodeNAttributesInstances {
  private case class Mapped[A, B](from: DecodeNAttributes[A], f: A => B) extends DecodeNAttributes[B] {
    def decode(attributes: List[String]): DecodeResult[B] = from.decode(attributes).map(f)
  }
}

trait DecodeNAttributesInstances extends LowPriorityDecodeNAttributes {
  implicit def decodeList[A: DecodeAttribute]: DecodeNAttributes[List[A]] = attributes =>
    DecodeResult.concat(attributes map DecodeAttribute.of[A].decode)

  implicit def decodeOption[A: DecodeAttribute]: DecodeNAttributes[Option[A]] =
    decodeList[A].map(_.headOption)
}

trait LowPriorityDecodeNAttributes {
  implicit def decodeOne[A: DecodeAttribute]: DecodeNAttributes[A] = {
    case (attribute: String) :: _ => DecodeAttribute.of[A].decode(attribute)
    case attributes => DecodeResult.Ko("Expected one attribute", Some(attributes))
  }
}
