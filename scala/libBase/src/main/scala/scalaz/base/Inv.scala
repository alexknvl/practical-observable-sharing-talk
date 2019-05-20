package scalaz.base

final case class Inv[A, B](to: A => B, from: B => A)