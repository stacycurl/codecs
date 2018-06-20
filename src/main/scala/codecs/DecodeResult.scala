package codecs

sealed trait DecodeResult[+A] {
  import DecodeResult._

  def toOption: Option[A] = fold(Some(_), _ => None)
  def errors: List[Error] = fold(_ => Nil, identity)
  def appendContext(context: AnyRef): DecodeResult[A] = leftMap(_.map(_.appendContext(context)))

  def leftMap(f: List[Error] => List[Error]): DecodeResult[A] = fold(Ok.apply, f andThen Ko.apply)

  def map[B](f: A => B): DecodeResult[B] = fold(f andThen Ok.apply, Ko.apply)

  def flatMap[B](f: A => DecodeResult[B]): DecodeResult[B] = fold(f, Ko.apply)

  def fold[B](ok: A => B, ko: List[Error] => B): B

  def &:[B](other: DecodeResult[B]): DecodeResult[(B, A)] = other && this

  def &&[B](other: DecodeResult[B]): DecodeResult[(A,B)] = (this, other) match {
    case (Ok(a),       Ok(b))       => Ok((a, b))
    case (Ko(aErrors), Ko(bErrors)) => Ko(aErrors ++ bErrors)
    case (Ok(a),       Ko(bErrors)) => Ko(bErrors)
    case (Ko(aErrors), Ok(b))       => Ko(aErrors)
  }
}

object DecodeResult {
  case class Ok[A](value: A) extends DecodeResult[A] {
    override def fold[B](ok: A => B, ko: List[Error] => B): B = ok(value)
  }

  object Ko {
    def apply(description: String, optContext: Option[AnyRef] = None): DecodeResult[Nothing] =
      apply(Error(description, optContext.toList))

    def apply(error: Error): DecodeResult[Nothing] = Ko(List(error))
  }

  case class Ko(value: List[Error]) extends DecodeResult[Nothing] {
    override def fold[B](ok: Nothing => B, ko: List[Error] => B): B = ko(value)

    override def toString: String = value.mkString("\n")
  }

  type DR[A] = DecodeResult[A]

  def concat[A](results: List[DecodeResult[A]]): DecodeResult[List[A]] = {
    results.reverse.foldLeft(Ok(List.empty[A]): DecodeResult[List[A]]) {
      case (Ok(as),  Ok(a))      => Ok(a :: as)
      case (Ko(acc), Ko(errors)) => Ko(acc ++ errors)
      case (Ok(as),  Ko(errors)) => Ko(errors)
      case (Ko(acc), Ok(_))      => Ko(acc)
    }
  }

  def fromOption[A](optA: Option[A], ifNone: => Error): DecodeResult[A] =
    optA.fold(Ko(List(ifNone)): DecodeResult[A])(Ok(_))


  private def errorsOf(drs: DR[_]*): DR[Nothing] = Ko(drs.flatMap(_.errors).toList)
}

case class Error(description: String, context: List[AnyRef] = Nil) {
  def appendContext(context: AnyRef): Error = copy(context = this.context :+ context)
}