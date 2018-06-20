package codecs

import scalaz.{Profunctor, Semigroup, Zip}

trait Encode[A, Rep] {
  def encode(a: A): Rep

  def mapRep[Sym](f: Rep => Sym): Encode[A, Sym] = Encode.MappedRep(this, f)
  def contramap[B](g: B => A): Encode[B, Rep] = Encode.Contramapped(this, g)
}

object Encode {
  implicit val encodeProfunctor: Profunctor[Encode] = new Profunctor[Encode] {
    def mapfst[A, Rep, B](fab: Encode[A, Rep])(f: B => A): Encode[B, Rep] = fab.contramap(f)
    def mapsnd[A, Rep, Sym](fab: Encode[A, Rep])(f: Rep => Sym): Encode[A, Sym] = fab.mapRep(f)
  }

  implicit def encodeZip[Rep: Semigroup]: Zip[Encode[?, Rep]] = new Zip[Encode[?, Rep]] {
    def zip[A, B](encodeA: => Encode[A, Rep], encodeB: => Encode[B, Rep]): Encode[(A, B), Rep] =
      Encode.Pair(encodeA, encodeB, Semigroup[Rep])
  }

  def of[A, Rep](implicit E: Encode[A, Rep]): Encode[A, Rep] = E

  trait Companion[Rep] {
    def of[A](implicit E: Encode[A, Rep]): Encode[A, Rep] = E
  }

  private case class MappedRep[A, Rep, Sym](from: Encode[A, Rep], f: Rep => Sym) extends Encode[A, Sym] {
    def encode(a: A): Sym = f(from.encode(a))
  }

  private case class Contramapped[A, B, Rep](from: Encode[A, Rep], g: B => A) extends Encode[B, Rep] {
    def encode(b: B): Rep = from.encode(g(b))
  }

  private case class Pair[A, B, Rep](encodeA: Encode[A, Rep], encodeB: Encode[B, Rep], S: Semigroup[Rep]) extends Encode[(A, B), Rep] {
    def encode(ab: (A, B)): Rep = S.append(encodeA.encode(ab._1), encodeB.encode(ab._2))
  }
}