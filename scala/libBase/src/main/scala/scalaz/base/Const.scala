package scalaz.base

import cats.{Applicative, Contravariant, Eval, Functor, Invariant, Traverse}

final case class Const[A, B](getConst: A) {

}
object Const {
  implicit def instance[Z]: Functor[Const[Z, ?]] with Contravariant[Const[Z, ?]] with Invariant[Const[Z, ?]] with Traverse[Const[Z, ?]] =
    new Functor[Const[Z, ?]] with Contravariant[Const[Z, ?]] with Invariant[Const[Z, ?]] with Traverse[Const[Z, ?]] {
      override def contramap[A, B](fa: Const[Z, A])(f: B => A): Const[Z, B] = Const(fa.getConst)

      override def traverse[G[_], A, B](fa: Const[Z, A])(f: A => G[B])(implicit ev: Applicative[G]): G[Const[Z, B]] =
        ev.pure(Const(fa.getConst))

      override def foldLeft[A, B](fa: Const[Z, A], b: B)(f: (B, A) => B): B = b

      override def foldRight[A, B](fa: Const[Z, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
    }
}
