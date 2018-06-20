package codecs.xml.internal

import codecs.DecodeResult


trait CodecAttribute[A] extends EncodeAttribute[A] with DecodeAttribute[A] {
  def encode(a: A): String = Encoder.encode(a)
  def decode(attribute: String): DecodeResult[A] = Decoder.decode(attribute)

  def xmap[B](f: A => B)(g: B => A): CodecAttribute[B] = CodecAttribute.of(Encoder contramap g, Decoder map f)

  val Encoder: EncodeAttribute[A]
  val Decoder: DecodeAttribute[A]
}

object CodecAttribute {
  def of[A](implicit E: EncodeAttribute[A], D: DecodeAttribute[A]): CodecAttribute[A] = {
    new CodecAttribute[A] {
      val Encoder: EncodeAttribute[A] = E
      val Decoder: DecodeAttribute[A] = D
    }
  }
}
