package codecs

import codecs.DecodeResult.{Ko, Ok}
import codecs.xml.internal._
import scalaz.{Semigroup, Zip}

import scala.reflect.ClassTag
import scala.{xml => X}

package object xml {
  type EncodeNode[A] = Encode[A, X.Node]
  object EncodeNode {
    def encode[A: EncodeNode](a: A): X.Node = of[A].encode(a)
    def of[A](implicit E: EncodeNode[A]): EncodeNode[A] = E
  }

  type DecodeNode[A] = Decode[A, X.Node]
  object DecodeNode {
    def decode[A: DecodeNode](node: X.Node): DecodeResult[A] = of[A].decode(node)
    def of[A](implicit D: DecodeNode[A]): DecodeNode[A] = D
  }

  type CodecNode[A] = Codec[A, X.Node]
  object CodecNode {
    implicit def of[A](implicit C: CodecNode[A]): CodecNode[A] = C
  }

  implicit def deriveEncodeNode[A: EncodeXml]: EncodeNode[A] = EncodeXml.of[A].mapRep[X.Node](_.toElem)
  implicit def deriveDecodeNode[A: DecodeXml]: DecodeNode[A] = DecodeXml.of[A].contramapRep[X.Node](Xml(_))
  implicit def deriveCodecNode[A:  CodecXml]:  CodecNode[A]  = CodecXml.of[A].xmapRep[X.Node](_.toElem)(Xml(_))


  type EncodeXml[A] = Encode[A, Xml]
  object EncodeXml extends Encode.Companion[Xml]

  type DecodeXml[A] = Decode[A, Xml]
  object DecodeXml extends Decode.Companion[Xml]

  implicit val stringCodecXml: CodecXml[String] = CodecXml.of[String](
    string => Xml.Element(Name.Placeholder, Xml.Children(text = List(string))),
    {
      case Xml.Text(value) => Ok(value)
      case xml             => Ko("Expected Text", Some(xml))
    }
  )

  implicit val booleanCodecXml: CodecXml[Boolean] = stringCodecXml.xmap[Boolean] {
    case "true" => true
    case _      => false
  } {
    case true => "true"
    case _    => "false"
  }

  type CodecXml[A]  = Codec[A, Xml]

  object CodecXml extends Codec.Parameter[Xml, Q] {
    abstract class HasText[CC: ClassTag](apply: String => CC, unapply: CC => String) {
      implicit val codec: CodecXml[CC] = CodecXml[CC, String](apply, cc => Some(unapply(cc)))(_.text)
    }

    abstract class HasAttriubte[CC: ClassTag](apply: String => CC, unapply: CC => String, attribute: String) {
      implicit val codec: CodecXml[CC] = CodecXml[CC, String](apply, cc => Some(unapply(cc)))(_.attribute(attribute))
    }

    protected implicit val repSemigroup: Semigroup[Xml] = Xml.semigroup

    private val Z = Zip[CodecXml]

    protected implicit val paremeterZip: Zip[Q] = new Zip[Q] {
      def zip[A, B](a: => Q[A], b: => Q[B]): Q[(A, B)] = choices => Z.zip(a(choices.as[A]), b(choices.as[B]))
    }

    protected def caseClassCodec[CC: ClassTag, A](qa: Q[A])(toCC: A => CC, fromCC: CC => A): CodecXml[CC] = {
      val cqq: CodecXml[A] = qa.apply(new Choices[A])

      of[CC](
        (cc: CC) => {
          val children = cqq.encode(fromCC(cc))

          Xml.Children(List(Xml.Element(implicitly[ClassTag[CC]].runtimeClass.getSimpleName, Xml.Children(children))))
        },
        (xml: Xml) => cqq.decode(xml).map(toCC)
      )
    }
  }

  type Q[A] = Choices[A] => CodecXml[A]

  class Choices[A] {
    private[xml] def as[B]: Choices[B] = asInstanceOf[Choices[B]]

    def text(implicit encodeA: EncodeNAttributes[A], decodeA: DecodeNAttributes[A]): CodecXml[A] = CodecXml.of[A](
      a => Xml.Children(text = encodeA.encode(a)),
      {
        case Xml.Text(text) => decodeA.decode(List(text))
        case xml            => Ko("Expected text", Some(xml))
      }
    )

    def attribute(name: Name)(implicit encodeA: EncodeNAttributes[A], decodeA: DecodeNAttributes[A]): CodecXml[A] = CodecXml.of[A](
      a => Xml.Children(attributes = Attributes(Map(name -> encodeA.encode(a)))),
      xml => for {
        values <- xml.attributes.get(name)
        a      <- decodeA.decode(values)
      } yield a
    )

    def element(name: Name)(implicit encodeA: EncodeN[A], decodeA: DecodeN[A]): CodecXml[A] = CodecXml.of[A](
      a => encodeA.encode(a).withName(name),
      xml => decodeA.decode(xml.elements collect {
        case element@Xml.Element(`name`, _) => element
      }).appendContext(name)
    )

    def namespace(name: Name)(implicit encodeA: EncodeNAttributes[A], decodeA: DecodeNAttributes[A]): CodecXml[A] = CodecXml.of[A](
      a => Xml.Children(namespaces = encodeA.encode(a).foldLeft(Namespaces()) {
        case (namespaces, value) => namespaces.add(name, value)
      }),
      xml => for {
        namespace <- xml.namespaces.get(name)
        a         <- decodeA.decode(List(namespace))
      } yield a
    )
  }
}
