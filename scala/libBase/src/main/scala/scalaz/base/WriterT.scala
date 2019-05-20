package scalaz.base

import cats.{Contravariant, Functor, Invariant}

final case class WriterT[F[_], S, A](run: F[S /\ A])
trait WriterTLowerPriority {
  implicit def invariant[F[_], S](implicit F: Invariant[F]): Invariant[WriterT[F, S, ?]] =
    new Invariant[WriterT[F, S, ?]] {
      def imap[A, B](fa: WriterT[F, S, A])(to: A => B)(from: B => A): WriterT[F, S, B] =
        WriterT(F.imap(fa.run)
        { case s /\ a => s /\ to(a) }
        { case s /\ b => s /\ from(b) })
    }
}
object WriterT {
  implicit def functor[F[_], S](implicit F: Functor[F]): Functor[WriterT[F, S, ?]] =
    new Functor[WriterT[F, S, ?]] {
      def map[A, B](fa: WriterT[F, S, A])(to: A => B): WriterT[F, S, B] =
        WriterT(F.map(fa.run)
        { case s /\ a => s /\ to(a) })
    }
  implicit def contravariant[F[_], S](implicit F: Contravariant[F]): Contravariant[WriterT[F, S, ?]] =
    new Contravariant[WriterT[F, S, ?]] {
      def contramap[A, B](fa: WriterT[F, S, A])(from: B => A): WriterT[F, S, B] =
        WriterT(F.contramap(fa.run)
        { case s /\ b => s /\ from(b) })
    }
}