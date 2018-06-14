package codecxml

trait CodecAttribute[A] extends EncodeAttribute[A] with DecodeAttribute[A] {
  def encode(name: String, a: A): Attribute = Encoder.encode(name, a)
  def decode(elem: Attribute): DecodeResult[A] = Decoder.decode(elem)

  val Encoder: EncodeAttribute[A]
  val Decoder: DecodeAttribute[A]
}

object CodecAttribute {
  implicit def of[A](implicit E: EncodeAttribute[A], D: DecodeAttribute[A]): CodecAttribute[A] = {
    new CodecAttribute[A] {
      val Encoder: EncodeAttribute[A] = E
      val Decoder: DecodeAttribute[A] = D
    }
  }
}
