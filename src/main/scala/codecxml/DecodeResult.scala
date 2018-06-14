package codecxml

import scala.{xml => X}

case class DecodeResult[+A](value: Either[Errors, A]) {
  def errors: Errors = value.fold(identity, _ => Errors(Nil))

  def toOption: Option[A] = value.toOption

  def map[B](f: A => B): DecodeResult[B] = DecodeResult(value.map(f))

  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] = DecodeResult(value.flatMap(a => f(a).value))
}

object DecodeResult {
  def applyN[A, B, R](da: DecodeResult[A], db: DecodeResult[B])(f: (A, B) => R): DecodeResult[R] = {
    (da, db) match {
      case (Ok(a), Ok(b)) => ok(f(a, b))
      case _              => errors(da.errors + db.errors)
    }
  }

  def applyN[A, B, C, D, R](da: DecodeResult[A], db: DecodeResult[B], dc: DecodeResult[C], dd: DecodeResult[D])(f: (A, B, C, D) => R): DecodeResult[R] = {
    (da, db, dc, dd) match {
      case (Ok(a), Ok(b), Ok(c), Ok(d)) => ok(f(a, b, c, d))
      case _                            => errors(da.errors + db.errors + dc.errors + dd.errors)
    }
  }


  def ok[A](value: A): DecodeResult[A] = DecodeResult(Right(value))
  def error(description: String, optElem: Option[X.Elem] = None): DecodeResult[Nothing] = error(Error(description, optElem))
  def error(error: Error): DecodeResult[Nothing] = errors(Errors(List(error)))
  def errors(errors: Errors) = DecodeResult(Left(errors))

  def fromOption[A](optA: Option[A], ifNone: => Error): DecodeResult[A] =
    DecodeResult(optA.fold(Left(Errors(List(ifNone))): Either[Errors, A])(Right(_)))

  private object Ok {
    def unapply[A](result: DecodeResult[A]): Option[A] = result.toOption
  }
}

case class Errors(errors: List[Error]) {
  def +(other: Errors) = Errors(errors ++ other.errors)
}

case class Error(description: String, optElem: Option[X.Elem] = None)