package codecxml

import codecxml.DecodeResult.Ko
import codecxml.internal._

import scala.xml.{Elem, Node}
import scala.{xml => X}


trait Codec[A] extends Encode[A] with Decode[A] {
  def encode(name: Name, a: A): Xml = Encoder.encode(name, a)
  def decode(node: X.Node): DecodeResult[A] = Decoder.decode(node)

  def xmap[B](f: A => B)(g: B => A): Codec[B] =
    Codec.of(Encoder contramap g, Decoder map f)

  def &&[B](other: Codec[B]): Codec[(A, B)] = Codec.Pair(this, other)

  val Encoder: Encode[A]
  val Decoder: Decode[A]
}

object Codec {
  private implicit class AnySyntax[A](val self: A) {
    def &&[B](other: B): (A, B) = (self, other)
  }

  implicit def of[A](implicit E: Encode[A], D: Decode[A]): Codec[A] = {
    new Codec[A] {
      val Encoder: Encode[A] = E
      val Decoder: Decode[A] = D
    }
  }

  private case class Pair[A, B](lhs: Codec[A], rhs: Codec[B]) extends Codec[(A, B)] {
    val Encoder: Encode[(A, B)] = lhs.Encoder && rhs.Encoder
    val Decoder: Decode[(A, B)] = lhs.Decoder && rhs.Decoder
  }

  def apply[CC, A](apply: A => CC, CC: Extractor[CC, A])(aq: Q[A]): Codec[CC] =
    createCodecN(aq).xmap(apply, { case CC(a) => a })

  def apply[CC, A,B](apply: (A,B) => CC, CC: Extractor[CC, (A,B)])(
    aq: Q[A], bq: Q[B]
  ): Codec[CC] = createCodecN(aq && bq).xmap(
    { case a && b => apply(a, b) },
    { case CC((a, b)) => a && b  }
  )

  def apply[CC, A,B,C](apply: (A,B,C) => CC, CC: Extractor[CC, (A,B,C)])(
    aq: Q[A], bq: Q[B], cq: Q[C]
  ): Codec[CC] = createCodecN(aq && bq && cq).xmap(
    { case a && b && c => apply(a, b, c) },
    { case CC((a, b, c)) => a && b && c  }
  )

  def apply[CC, A,B,C,D](apply: (A,B,C,D) => CC, CC: Extractor[CC, (A,B,C,D)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq).xmap(
    { case a && b && c && d => apply(a, b, c, d) },
    { case CC((a, b, c, d)) => a && b && c && d  }
  )

  def apply[CC, A,B,C,D,E](apply: (A,B,C,D,E) => CC, CC: Extractor[CC, (A,B,C,D,E)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D], eq: Q[E]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq && eq).xmap(
    { case a && b && c && d && e => apply(a, b, c, d, e) },
    { case CC((a, b, c, d, e)) => a && b && c && d && e }
  )

  def apply[CC, A,B,C,D,E,F](apply: (A,B,C,D,E,F) => CC, CC: Extractor[CC, (A,B,C,D,E,F)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D], eq: Q[E], fq: Q[F]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq && eq && fq).xmap(
    { case a && b && c && d && e && f => apply(a, b, c, d, e, f) },
    { case CC((a, b, c, d, e, f)) => a && b && c && d && e && f }
  )

  def apply[CC, A,B,C,D,E,F,G](apply: (A,B,C,D,E,F,G) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D], eq: Q[E], fq: Q[F], gq: Q[G]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq && eq && fq && gq).xmap(
    { case a && b && c && d && e && f && g => apply(a, b, c, d, e, f, g) },
    { case CC((a, b, c, d, e, f, g)) => a && b && c && d && e && f && g  }
  )

  def apply[CC, A,B,C,D,E,F,G,H](apply: (A,B,C,D,E,F,G,H) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D], eq: Q[E], fq: Q[F], gq: Q[G], hq: Q[H]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq && eq && fq && gq && hq).xmap(
    { case a && b && c && d && e && f && g && h => apply(a, b, c, d, e, f, g, h) },
    { case CC((a, b, c, d, e, f, g, h)) => a && b && c && d && e && f && g && h }
  )

  def apply[CC, A,B,C,D,E,F,G,H,I](apply: (A,B,C,D,E,F,G,H,I) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H,I)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D], eq: Q[E], fq: Q[F], gq: Q[G], hq: Q[H], iq: Q[I]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq && eq && fq && gq && hq && iq).xmap(
    { case a && b && c && d && e && f && g && h && i => apply(a, b, c, d, e, f, g, h, i) },
    { case CC((a, b, c, d, e, f, g, h, i)) => a && b && c && d && e && f && g && h && i  }
  )

  def apply[CC, A,B,C,D,E,F,G,H,I,J](apply: (A,B,C,D,E,F,G,H,I,J) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H,I,J)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D], eq: Q[E], fq: Q[F], gq: Q[G], hq: Q[H], iq: Q[I], jq: Q[J]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq && eq && fq && gq && hq && iq && jq).xmap(
    { case a && b && c && d && e && f && g && h && i && j => apply(a, b, c, d, e, f, g, h, i, j) },
    { case CC((a, b, c, d, e, f, g, h, i, j)) => a && b && c && d && e && f && g && h && i && j }
  )

  def apply[CC, A,B,C,D,E,F,G,H,I,J,K](apply: (A,B,C,D,E,F,G,H,I,J,K) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H,I,J,K)])(
    aq: Q[A], bq: Q[B], cq: Q[C], dq: Q[D], eq: Q[E], fq: Q[F], gq: Q[G], hq: Q[H], iq: Q[I], jq: Q[J], kq: Q[K]
  ): Codec[CC] = createCodecN(aq && bq && cq && dq && eq && fq && gq && hq && iq && jq && kq).xmap(
    { case a && b && c && d && e && f && g && h && i && j && k => apply(a, b, c, d, e, f, g, h, i, j, k) },
    { case CC((a, b, c, d, e, f, g, h, i, j, k)) => a && b && c && d && e && f && g && h && i && j && k }
  )

  private case class createCodecN[QQ](qqq: Q[QQ]) {
    def xmap[A](f: QQ => A, g: A => QQ): Codec[A] = {
      val cqq: Codec[QQ] = qqq.apply(new Choices)

      of[A](
        (name: Name, a: A) => {
          val children = cqq.encode(name, g(a))

          Xml.Children(List(Xml.Element(name, Xml.Children(children))))
        },
        (node: Node) => cqq.decode(node).map(f)
      )
    }
  }

  private object && {
    def unapply[A,B](ab: (A, B)): Option[(A, B)] = Some(ab)
  }

  abstract class HasText[CC](apply: String => CC, unapply: CC => String) {
    implicit val codec: Codec[CC] = Codec[CC, String](apply, cc => Some(unapply(cc)))(_.text)
  }

  abstract class HasAttriubte[CC](apply: String => CC, unapply: CC => String, attribute: String) {
    implicit val codec: Codec[CC] = Codec[CC, String](apply, cc => Some(unapply(cc)))(_.attribute(attribute))
  }

  abstract class HasElement[A, CC](apply: A => CC, unapply: CC => A, element: String)(implicit codecA: Codec[A]) {
    implicit val codec: Codec[CC] = Codec[CC, A](apply, cc => Some(unapply(cc)))(_.element(element))
  }

  private def partition(mixed: AttributeOrNodes*): (X.MetaData, X.NodeSeq) = {
    val attributes = List.newBuilder[Attribute]
    val nodeSeq    = List.newBuilder[X.Node]

    mixed foreach {
      case Left(attribute) => attributes += attribute
      case Right(nodes)    => nodeSeq    ++= nodes
    }

    (Attributes.fromList(attributes.result()), nodeSeq.result())
  }

  type AttributeOrNodes = Either[Attribute, X.NodeSeq]

  type Q[A] = Choices[A] => Codec[A]

  private implicit class QSyntax[A](val self: Q[A]) {
    def &&[B](other: Q[B]): Q[(A, B)] = choices => self(choices.as[A]) && other(choices.as[B])
  }

  class Choices[A] {
    private[codecxml] def as[B]: Choices[B] = asInstanceOf[Choices[B]]

    def text(implicit encodeA: EncodeNAttributes[A], decodeA: DecodeNAttributes[A]): Codec[A] = Codec.of[A](
      (_, a) => Xml.Children(text = encodeA.encode(a)),
      {
        case X.Elem(_, _, _, _, X.Text(text)) => decodeA.decode(List(text))
        case node => Ko("Expected text", Some(node))
      }
    )

    def attribute(name: Name)(implicit encodeA: EncodeNAttributes[A], decodeA: DecodeNAttributes[A]): Codec[A] = Codec.of[A](
      (_, a) => Xml.Children(attributes = Attributes(Map(name -> encodeA.encode(a)))),
      node => for {
        values <- Attributes(node.attributes).get(name)
        a <- decodeA.decode(values)
      } yield a
    )

    def element(name: Name)(implicit encodeA: EncodeN[A], decodeA: DecodeN[A]): Codec[A] = Codec.of[A](
      (_, a) => encodeA.encode(name, a),
      node => {
        val kids: Seq[Node] = node.child collect {
          case elem: Elem if name.matches(elem.prefix, elem.label) => elem
        }

        val result: DecodeResult[A] = decodeA.decode(kids)

        result.appendContext(Context.Element(name))
      }
    )

    def namespace(name: Name)(implicit encodeA: EncodeNAttributes[A], decodeA: DecodeNAttributes[A]): Codec[A] = Codec.of[A](
      (_, a) => Xml.Children(namespaces = encodeA.encode(a).foldLeft(Namespaces()) {
        case (namespaces, value) => namespaces.add(name, value)
      }),
      node => for {
        namespace <- Namespaces(node.scope).get(name)
        a <- decodeA.decode(List(namespace))
      } yield a
    )
  }
}