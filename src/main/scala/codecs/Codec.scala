package codecs

import scala.language.higherKinds

import codecs.internal.Extractor
import codecs.internal.ZipSyntax._
import scalaz.{Category, InvariantFunctor, Semigroup, Zip}

import scala.reflect.ClassTag


trait Codec[A, Rep] extends Encode[A, Rep] with Decode[A, Rep] {
  val Encoder: Encode[A, Rep]
  val Decoder: Decode[A, Rep]

  def encode(a: A): Rep = Encoder.encode(a)
  def decode(rep: Rep): DecodeResult[A] = Decoder.decode(rep)

  def xmapRep[Sym](f: Rep => Sym)(g: Sym => Rep): Codec[A, Sym] =
    Codec.of[A, Sym](Encoder mapRep f, Decoder contramapRep g)

  def xmap[B](f: A => B)(g: B => A): Codec[B, Rep] =
    Codec.of[B, Rep](Encoder contramap g, Decoder map f)
}

object Codec {
  implicit val codecCategory: Category[Codec] = new Category[Codec] {
    def id[A]: Codec[A, A] = new Codec[A, A] {
      val Encoder: Encode[A, A] = (a: A) => a
      val Decoder: Decode[A, A] = (rep: A) => DecodeResult.Ok(rep)
    }

    def compose[A, B, C](f: Codec[B, C], g: Codec[A, B]): Codec[A, C] = new Codec[A, C] {
      val Encoder: Encode[A, C] = (a: A) => f.encode(g.encode(a))
      val Decoder: Decode[A, C] = (rep: C) => for {
        b ← f.decode(rep)
        a ← g.decode(b)
      } yield a
    }
  }

  implicit def codecInvariant[Rep]: InvariantFunctor[Codec[?, Rep]] = new InvariantFunctor[Codec[?, Rep]] {
    def xmap[A, B](codecA: Codec[A, Rep], f: A => B, g: B => A): Codec[B, Rep] = codecA.xmap(f)(g)
  }

  implicit def codecZip[Rep](implicit EZ: Zip[Encode[?, Rep]], DZ: Zip[Decode[?, Rep]]): Zip[Codec[?, Rep]] = new Zip[Codec[?, Rep]] {
    def zip[A, B](codecA: => Codec[A, Rep], codecB: => Codec[B, Rep]): Codec[(A, B), Rep] = of[(A, B), Rep](
      EZ.zip(codecA.Encoder, codecB.Encoder), DZ.zip(codecA.Decoder, codecB.Decoder)
    )
  }

  def of[A, Rep](implicit E: Encode[A, Rep], D: Decode[A, Rep]): Codec[A, Rep] = {
    new Codec[A, Rep] {
      val Encoder: Encode[A, Rep] = E
      val Decoder: Decode[A, Rep] = D
    }
  }


  trait Parameterless[Rep] {
    protected implicit def repSemigroup: Semigroup[Rep]

    type CR[A] = Codec[A, Rep]

    def of[A](implicit E: Encode[A, Rep], D: Decode[A, Rep]): Codec[A, Rep] = Codec.of[A, Rep](E, D)

    def apply[CC, A:CR](apply: A => CC, CC: Extractor[CC, A]): Codec[CC, Rep] =
      of[A].xmap[CC](apply) { case CC(a) => a }

    def apply[CC, A:CR, B:CR](apply: (A,B) => CC, CC: Extractor[CC, (A, B)]): Codec[CC, Rep] = {
      (of[A] &: of[B]).xmap[CC] {
        case (a, b) => apply(a, b)
      } {
        case CC((a, b)) => (a, b)
      }
    }

    def apply[CC, A:CR, B:CR, C:CR](apply: (A,B,C) => CC, CC: Extractor[CC, (A,B,C)]): Codec[CC, Rep] = {
      (of[A] &: of[B] &: of[C]).xmap[CC] {
        case (a, (b, c)) => apply(a, b, c)
      } {
        case CC((a, b, c)) => (a, (b, c))
      }
    }
  }

  trait Parameter[Rep, P[_]] {
    def of[A](implicit E: Encode[A, Rep], D: Decode[A, Rep]): Codec[A, Rep] = Codec.of[A, Rep](E, D)

    def apply[CC: ClassTag, A](apply: A => CC, CC: Extractor[CC, A])(pa: P[A]): Codec[CC, Rep] =
      caseClassCodec(pa)(apply, { case CC(a) => a })

    def apply[CC: ClassTag, A,B](apply: (A,B) => CC, CC: Extractor[CC, (A,B)])(
      pa: P[A], pb: P[B]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb)(
      { case (a, b) => apply(a, b) },
      { case CC((a, b)) => (a, b) }
    )

    def apply[CC: ClassTag, A,B,C](apply: (A,B,C) => CC, CC: Extractor[CC, (A,B,C)])(
      pa:P[A], pb:P[B], pc:P[C]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc)(
      { case (a, (b, c)) => apply(a, b, c) },
      { case CC((a, b, c)) => (a, (b, c)) }
    )

    def apply[CC: ClassTag, A,B,C,D](apply: (A,B,C,D) => CC, CC: Extractor[CC, (A,B,C,D)])(
      aq: P[A], bq: P[B], cq: P[C], dq: P[D]
    ): Codec[CC, Rep] = caseClassCodec(aq &: bq &: cq &: dq)(
      { case (a, (b, (c, d))) => apply(a, b, c, d) },
      { case CC((a, b, c, d)) => (a, (b, (c, d))) }
    )

    def apply[CC: ClassTag, A,B,C,D,E](apply: (A,B,C,D,E) => CC, CC: Extractor[CC, (A,B,C,D,E)])(
      pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc &: pd &: pe)(
      { case (a, (b, (c, (d, e)))) => apply(a, b, c, d, e) },
      { case CC((a, b, c, d, e)) => (a, (b, (c, (d, e)))) }
    )

    def apply[CC: ClassTag, A,B,C,D,E,F](apply: (A,B,C,D,E,F) => CC, CC: Extractor[CC, (A,B,C,D,E,F)])(
      pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc &: pd &: pe &: pf)(
      { case (a, (b, (c, (d, (e, f))))) => apply(a, b, c, d, e, f) },
      { case CC((a, b, c, d, e, f)) => (a, (b, (c, (d, (e, f))))) }
    )

    def apply[CC: ClassTag, A,B,C,D,E,F,G](apply: (A,B,C,D,E,F,G) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G)])(
      pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc &: pd &: pe &: pf &: pg)(
      { case (a, (b, (c, (d, (e, (f, g)))))) => apply(a, b, c, d, e, f, g) },
      { case CC((a, b, c, d, e, f, g)) => (a, (b, (c, (d, (e, (f, g)))))) }
    )

    def apply[CC: ClassTag, A,B,C,D,E,F,G,H](apply: (A,B,C,D,E,F,G,H) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H)])(
      pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc &: pd &: pe &: pf &: pg &: ph)(
      { case (a, (b, (c, (d, (e, (f, (g, h))))))) => apply(a, b, c, d, e, f, g, h) },
      { case CC((a, b, c, d, e, f, g, h)) => (a, (b, (c, (d, (e, (f, (g, h)))))))  }
    )

    def apply[CC: ClassTag, A,B,C,D,E,F,G,H,I](apply: (A,B,C,D,E,F,G,H,I) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H,I)])(
      pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc &: pd &: pe &: pf &: pg &: ph &: pi)(
      { case (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) => apply(a, b, c, d, e, f, g, h, i) },
      { case CC((a, b, c, d, e, f, g, h, i)) => (a, (b, (c, (d, (e, (f, (g, (h, i)))))))) }
    )

    def apply[CC: ClassTag, A,B,C,D,E,F,G,H,I,J](apply: (A,B,C,D,E,F,G,H,I,J) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H,I,J)])(
      pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc &: pd &: pe &: pf &: pg &: ph &: pi &: pj)(
      { case (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) => apply(a, b, c, d, e, f, g, h, i, j) },
      { case CC((a, b, c, d, e, f, g, h, i, j)) => (a, (b, (c, (d, (e, (f, (g, (h, (i, j))))))))) }
    )

    def apply[CC: ClassTag, A,B,C,D,E,F,G,H,I,J,K](apply: (A,B,C,D,E,F,G,H,I,J,K) => CC, CC: Extractor[CC, (A,B,C,D,E,F,G,H,I,J,K)])(
      pa: P[A], pb: P[B], pc: P[C], pd: P[D], pe: P[E], pf: P[F], pg: P[G], ph: P[H], pi: P[I], pj: P[J], pk: P[K]
    ): Codec[CC, Rep] = caseClassCodec(pa &: pb &: pc &: pd &: pe &: pf &: pg &: ph &: pi &: pj &: pk)(
      { case (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, k)))))))))) => apply(a, b, c, d, e, f, g, h, i, j, k) },
      { case CC((a, b, c, d, e, f, g, h, i, j, k)) => (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, k)))))))))) }
    )

    protected implicit def repSemigroup: Semigroup[Rep]
    protected implicit def paremeterZip: Zip[P]

    protected def caseClassCodec[CC: ClassTag, A](pa: P[A])(toCC: A => CC, fromCC: CC => A): Codec[CC, Rep]
  }
}
