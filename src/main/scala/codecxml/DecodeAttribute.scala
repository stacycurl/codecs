package codecxml

trait DecodeAttribute[A] {
  def decode(attribute: Attribute): DecodeResult[A]
}

object DecodeAttribute extends DecodeAttributeInstances

trait DecodeAttributeInstances {
  implicit val stringDecodeAttribute: DecodeAttribute[String] = attribute => DecodeResult(Right(attribute.value))
}
