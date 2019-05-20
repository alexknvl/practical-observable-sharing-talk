package app

import cats.Contravariant
import cats.data.NonEmptyList
import scalaz.base._
import scalaz.parsers.parsers.Parsing
import scalaz.parsers.symbols.SymbolSet

trait Show[A] { A =>
  def apply(a: A): String

  def contramap[B](f: B => A): Show[B] =
    b => A(f(b))
  def list(s: String): Show[List[A]] =
    l => l.map(A(_)).mkString(s)
  def nel(s: String): Show[NonEmptyList[A]] =
    l => l.toList.map(A(_)).mkString(s)
  def sep1[S](sep: String, S: Show[S]): Show[Separated1[S, A]] =
    l => A(l.head) ++
      l.tail.map { case s /\ t => sep ++ S(s) ++ sep ++ sep1(sep, S)(t) }
        .getOrElse("")
  def br(left: String, right: String): Show[A] =
    a => left ++ A(a) ++ right
}

object Show {
  def universal[A]: Show[A] = x => x.toString

  implicit val showParsing: Contravariant[Show] with Parsing[Show] { type Symbol = Char } =
    new Parsing[Show] with Contravariant[Show] {
      type Symbol = Char
      type F[A] = Show[A]

      def pure[A](a: A): F[A] = a => ""

      def any: F[Symbol] = s => s.toString

      def sym(symbol: Symbol): F[Unit] = _ => symbol.toString

      def contramap[A, B](A: F[A])(f: B => A): F[B] = s => A(f(s))

      /** Language union. */
      def alt[A, B](A: F[A], B: F[B]): F[A \/ B] = {
        case -\/(a) => A(a)
        case \/-(b) => B(b)
      }

      /** Sequential composition. */
      def zip[A, B](A: F[A], B: F[B]): F[A /\ B] = {
        case (a, b) => A(a) ++ B(b)
      }

      def delay[A](A: => F[A]): F[A] = {
        lazy val p: F[A] = A
        a => p(a)
      }

      def rule[A](name: String, f: F[A]): F[A] = f

      def anyOf(r: SymbolSet[Symbol]): F[r.Type] = r => r.toString

      def end: F[Unit] = _ => ""
    }
}