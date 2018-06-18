package codecxml

import codecxml.DecodeResult.{Ok, Ko}

import scala.xml.Node
import scala.{xml => X}


trait Decode[A] {
  def decode(node: X.Node): DecodeResult[A]

  def map[B](f: A => B): Decode[B] = Decode.Mapped(this, f)

  def &&[B](other: Decode[B]): Decode[(A, B)] = Decode.Pair(this, other)
}

object Decode extends DecodeElemInstances {
  def decode[A: Decode](node: X.Node): DecodeResult[A] = of[A].decode(node)

  def of[A](implicit D: Decode[A]): Decode[A] = D

  private case class Mapped[A, B](from: Decode[A], f: A => B) extends Decode[B] {
    def decode(node: Node): DecodeResult[B] = from.decode(node).map(f)
  }

  private case class Pair[A, B](a: Decode[A], b: Decode[B]) extends Decode[(A, B)] {
    def decode(node: Node): DecodeResult[(A, B)] = a.decode(node) && b.decode(node)
  }
}

trait DecodeElemInstances {
  implicit val stringDecode: Decode[String] = {
    case X.Text(value)                     => Ok(value)
    case X.Elem(_, _, _, _, X.Text(value)) => Ok(value)
    case node                              => Ko("Expected Text", Some(node))
  }

  implicit val booleanDecode: Decode[Boolean] = stringDecode.map {
    case "true" => true
    case _      => false
  }
}
