package codecs

import scalaz.{Profunctor, Zip}

import scala.language.higherKinds


trait Decode[A, Rep] {
  def decode(rep: Rep): DecodeResult[A]

  def contramapRep[Sym](g: Sym => Rep): Decode[A, Sym] = Decode.ContramappedRep(this, g)
  def map[B](f: A => B): Decode[B, Rep] = Decode.Mapped(this, f)
}

object Decode {
  implicit val decodeProfunctor: Profunctor[Lambda[(Rep, A) => Decode[A, Rep]]] = new Profunctor[Lambda[(Rep, A) => Decode[A, Rep]]] {
    def mapfst[Rep, A, Sym](fab: Decode[A, Rep])(f: Sym => Rep): Decode[A, Sym] = fab.contramapRep(f)
    def mapsnd[Rep, A, B](fab: Decode[A, Rep])(f: A => B): Decode[B, Rep] = fab.map(f)
  }

  implicit def decodeZip[Rep]: Zip[Decode[?, Rep]] = new Zip[Decode[?, Rep]] {
    def zip[A, B](decodeA: => Decode[A, Rep], decodeB: => Decode[B, Rep]): Decode[(A, B), Rep] = Decode.Pair(decodeA, decodeB)
  }

  def of[A, Rep](implicit D: Decode[A, Rep]): Decode[A, Rep] = D

  trait Companion[Rep] {
    def of[A](implicit D: Decode[A, Rep]): Decode[A, Rep] = D
  }

  private case class ContramappedRep[A, Rep, Sym](from: Decode[A, Rep], g: Sym => Rep) extends Decode[A, Sym] {
    def decode(sym: Sym): DecodeResult[A] = from.decode(g(sym))
  }

  private case class Mapped[A, B, Rep](from: Decode[A, Rep], f: A => B) extends Decode[B, Rep] {
    def decode(rep: Rep): DecodeResult[B] = from.decode(rep).map(f)
  }

  private case class Pair[A, B, Rep](decodeA: Decode[A, Rep], decodeB: Decode[B, Rep]) extends Decode[(A, B), Rep] {
    def decode(rep: Rep): DecodeResult[(A, B)] = decodeA.decode(rep) && decodeB.decode(rep)
  }
}
