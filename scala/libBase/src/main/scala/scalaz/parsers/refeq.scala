package scalaz.parsers

import cats.effect.IO

object refeq {
  trait RefEq[A] {
    type Id
    def id(a: A): IO[Id]
    def hash(id: Id): Int
  }
  object RefEq {
    def universal[A <: AnyRef]: RefEq[A] { type Id = AnyRef } =
      new RefEq[A] {
        type Id = AnyRef
        def id(a: A): IO[Id] = IO { a }
        def hash(id: Id): Int = System.identityHashCode(id)
      }
  }
}
