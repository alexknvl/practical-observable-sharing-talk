package scalaz.base

import cats.Invariant

trait IsoFunctor[F[_]] {
  def isoMap[A, B](fa: F[A])(f: Iso[A, B]): F[B]
}
object IsoFunctor {
  implicit def isoFunctor[F[_]](implicit I: Invariant[F]): IsoFunctor[F] =
    new IsoFunctor[F] {
      def isoMap[A, B](fa: F[A])(f: Iso[A, B]): F[B] =
        I.imap(fa)(f.to)(f.from)
    }
}