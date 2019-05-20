package scalaz.base

final case class Separated[S, A](value: Option[Separated1[S, A]])
object Separated {
  def iso[S, A]: Iso[Option[Separated1[S, A]], Separated[S, A]] =
    Iso.unsafe(x => Separated(x), x => x.value)
}

final case class Separated1[S, A](head: A, tail: Option[S /\ Separated1[S, A]])
object Separated1 {
  def iso[S, A]: Iso[A /\ Option[S /\ Separated1[S, A]], Separated1[S, A]] =
    Iso.unsafe((Separated1.apply[S, A] _).tupled, x => Separated1.unapply(x).get)
}