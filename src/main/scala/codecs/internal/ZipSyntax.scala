package codecs.internal

import scala.language.higherKinds

import scalaz.Zip


object ZipSyntax {
  implicit class ZipSyntax[F[_], A](val self: F[A]) extends AnyVal {
    def &:[B](other: F[B])(implicit Z: Zip[F]): F[(B, A)] = Z.zip(other, self)
  }

  implicit class Zip2Syntax[F[_, _], A, R](val self: F[A, R]) extends AnyVal {
    def &:[B](other: F[B, R])(implicit Z: Zip[F[?, R]]): F[(B, A), R] = Z.zip(other, self)
  }
}
