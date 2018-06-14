package codecxml

import scala.reflect.ClassTag
import scala.{xml => X}

trait CodecElem[A] extends EncodeElem[A] with DecodeElem[A] {
  def encode(name: String, a: A): X.Elem = Encoder.encode(name, a)
  def decode(elem: X.Elem): DecodeResult[A] = Decoder.decode(elem)

  val Encoder: EncodeElem[A]
  val Decoder: DecodeElem[A]
}

object CodecElem {
  implicit def of[A](implicit E: EncodeElem[A], D: DecodeElem[A]): CodecElem[A] = {
    new CodecElem[A] {
      val Encoder: EncodeElem[A] = E
      val Decoder: DecodeElem[A] = D
    }
  }

  def codec1[CC: ClassTag, A](apply: A => CC, unapply: CC => Option[A])(
    ac: X[A]
  ): CodecElem[CC] = of(
    (name: String, cc: CC) => elem(
      name, ac(new Dunno[A]).encode(unapply(cc).get)
    ),
    (elem: X.Elem) => for {
      a <- ac(new Dunno[A]).decode(elem)
    } yield apply(a)
  )

  def codec2[CC: ClassTag, A, B](apply: (A, B) => CC, unapply: CC => Option[(A, B)])(
    an: X[A], bn: X[B]
  ): CodecElem[CC] = of(
    (name: String, cc: CC) => {
      val (a, b) = unapply(cc).get

      elem(
        name,
        an(new Dunno[A]).encode(a),
        bn(new Dunno[B]).encode(b)
      )
    },
    (elem: X.Elem) => DecodeResult.applyN(
      an(new Dunno[A]).decode(elem),
      bn(new Dunno[B]).decode(elem)
    )(apply)
  )
  def codec4[CC: ClassTag, A, B, C, D](apply: (A, B, C, D) => CC, unapply: CC => Option[(A, B, C, D)])(
    an: X[A], bn: X[B], cn: X[C], dn: X[D]
  ): CodecElem[CC] = of(
    (name: String, cc: CC) => {
      val (a, b, c, d) = unapply(cc).get

      elem(
        name,
        an(new Dunno[A]).encode(a),
        bn(new Dunno[B]).encode(b),
        cn(new Dunno[C]).encode(c),
        dn(new Dunno[D]).encode(d)
      )
    },
    (elem: X.Elem) => DecodeResult.applyN(
      an(new Dunno[A]).decode(elem),
      bn(new Dunno[B]).decode(elem),
      cn(new Dunno[C]).decode(elem),
      dn(new Dunno[D]).decode(elem)
    )(apply)
  )

  private def elem(name: String, mixed: Either[Attribute, X.Elem]*): X.Elem = {
    val (attributes, children) = partition(mixed: _*)

    X.Elem(
      null, name,
      attributes, X.TopScope, children.isEmpty, children: _*
    )
  }

  private def partition(mixed: Either[Attribute, X.Elem]*): (X.MetaData, List[X.Elem]) = {
    val attributes = List.newBuilder[Attribute]
    val elems      = List.newBuilder[X.Elem]

    mixed foreach {
      case Left(attribute) => attributes += attribute
      case Right(elem)     => elems      += elem
    }

    (Attribute.fromList(attributes.result()), elems.result())
  }

  type X[A] = Dunno[A] => Next[A]

  class Dunno[A] {
    def attribute(name: String)(implicit codecA: CodecAttribute[A]): Next[A] = new Next[A](name) {
      def decode(elem: X.Elem): DecodeResult[A] = for {
        value <- DecodeResult.fromOption(Attribute.toMap(elem.attributes).get(name), Error(s"No such attribute: $name"))
        a     <- codecA.decode(Attribute(name, value))
      } yield a

      def encode(a: A): Either[Attribute, X.Elem] = Left(codecA.encode(name, a))
    }

    def element(name: String)(implicit codecA: CodecElem[A]): Next[A] = new Next[A](name) {
      def decode(elem: X.Elem): DecodeResult[A] = {
        val names = elem.child.collect {
          case X.Elem(_, n, _, _, _) => n
        }

        elem.child.collectFirst {
          case kid: X.Elem if kid.label == name => codecA.decode(kid)
        }.getOrElse({
          DecodeResult.error(s"Expected '$name' element, available: ${names.mkString(", ")}", Some(elem))
        })
      }
      def encode(a: A): Either[Attribute, X.Elem] = Right(codecA.encode(name, a))
    }
  }

  abstract class Next[A](name: String) {
    def decode(elem: X.Elem): DecodeResult[A]
    def encode(a: A): Either[Attribute, X.Elem]
  }
}








