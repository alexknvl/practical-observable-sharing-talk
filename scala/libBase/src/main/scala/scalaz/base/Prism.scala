package scalaz.base

abstract class Prism[A, B] { ab =>
  def to(a: A): Option[B]
  def from(b: B): A

  def andThen[C](bc: Prism[B, C]): Prism[A, C] = new Prism[A, C] {
    def to(a: A): Option[C] = ab.to(a).flatMap(bc.to)
    def from(c: C): A = ab.from(bc.from(c))
  }

  def compose[Z](za: Prism[Z, A]): Prism[Z, B] = za.andThen(ab)
}
object Prism {
  def apply[A, B](implicit ab: Prism[A, B]): Prism[A, B] = ab

  def unsafe[A, B](ab: A => Option[B], ba: B => A): Prism[A, B] = new Prism[A, B] {
    def to(a: A): Option[B] = ab(a)
    def from(b: B): A = ba(b)
  }

  final case class Refl[A]() extends Prism[A, A] {
    def to(a: A): Option[A] = Some(a)
    def from(b: A): A = b
  }

  def id[A]: Prism[A, A] = Refl[A]()
}
