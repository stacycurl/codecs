package codecs.internal

import scala.language.implicitConversions


trait Extractor[A, B] {
  def unapply(a: A): Option[B]
}

object Extractor {
  implicit def apply[A, B](f: A => Option[B]): Extractor[A, B] = (a: A) => f(a)

  def pf[A, B](pf: PartialFunction[A, B]): Extractor[A, B] = apply(pf.lift)
}

trait Producty[A, B] extends Extractor[A, B] {
  def unapply(a: A): Option[B]
  def apply(b: B): A
}

object Producty {
  def apply[A, B](ba: B => A)(ab: PartialFunction[A, B]): Producty[A, B] = new Producty[A, B] {
    def unapply(a: A): Option[B] = ab.lift(a)
    def apply(b: B): A = ba.apply(b)
  }
}