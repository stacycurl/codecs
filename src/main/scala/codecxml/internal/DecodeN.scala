package codecxml.internal

import codecxml.{Decode, DecodeResult}

import scala.{xml => X}

trait DecodeN[A] {
  def decode(nodes: X.NodeSeq): DecodeResult[A]

  def map[B](f: A => B): DecodeN[B] = DecodeN.Mapped(this, f)
}

object DecodeN extends DecodeNInstances {
  private case class Mapped[A, B](from: DecodeN[A], f: A => B) extends DecodeN[B] {
    def decode(nodes: X.NodeSeq): DecodeResult[B] = from.decode(nodes).map(f)
  }
}

trait DecodeNInstances extends LowPriorityDecodeN {
  implicit def decodeList[A: Decode]: DecodeN[List[A]] = nodes =>
    DecodeResult.concat(nodes.toList map Decode.of[A].decode)

  implicit def decodeOption[A: Decode]: DecodeN[Option[A]] =
    decodeList[A].map(_.headOption)
}

trait LowPriorityDecodeN {
  implicit def decodeOne[A: Decode]: DecodeN[A] = {
    case (node: X.Node) +: _ => Decode.of[A].decode(node)
    case nodes               => DecodeResult.Ko("Expected one node", Some(nodes))
  }
}

