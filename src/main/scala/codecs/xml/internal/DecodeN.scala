package codecs.xml.internal

import codecs.DecodeResult
import codecs.xml.{DecodeXml, Xml}

trait DecodeN[A] {
  def decodeNodes(nodes: scala.xml.Node*): DecodeResult[A] = decode(nodes.map(Xml(_)).toList)
  def decode(xmls: List[Xml]): DecodeResult[A]

  def map[B](f: A => B): DecodeN[B] = DecodeN.Mapped(this, f)
}

object DecodeN extends DecodeNInstances {
  private case class Mapped[A, B](from: DecodeN[A], f: A => B) extends DecodeN[B] {
    def decode(xmls: List[Xml]): DecodeResult[B] = from.decode(xmls).map(f)
  }
}

trait DecodeNInstances extends LowPriorityDecodeN {
  implicit def decodeList[A: DecodeXml]: DecodeN[List[A]] = xmls =>
    DecodeResult.concat(xmls map DecodeXml.of[A].decode)

  implicit def decodeOption[A: DecodeXml]: DecodeN[Option[A]] =
    decodeList[A].map(_.headOption)
}

trait LowPriorityDecodeN {
  implicit def decodeOne[A: DecodeXml]: DecodeN[A] = {
    case (xml: Xml) :: _ => DecodeXml.of[A].decode(xml)
    case xmls            => DecodeResult.Ko("Expected one node", Some(xmls))
  }
}

