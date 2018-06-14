package codecxml

import scala.{xml => X}

trait DecodeElem[A] {
  def decode(elem: X.Elem): DecodeResult[A]
}

object DecodeElem extends DecodeElemInstances

trait DecodeElemInstances {
  implicit val stringDecodeElem: DecodeElem[String] = {
    case X.Elem(_, _, _, _, X.Text(value)) => DecodeResult(Right(value))
    case elem                              => DecodeResult.error("Expected Text", Some(elem))
  }
}
