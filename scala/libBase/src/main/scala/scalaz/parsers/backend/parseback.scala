package scalaz.parsers.backend

import cats.Functor
import scalaz.base._
import scalaz.parsers.parsers.Parsing
import scalaz.parsers.symbols.SymbolSet
import _root_.parseback.Parser

object parseback {
  sealed abstract class PBWrapper[A]
  final case class Value[A](value: Parser[A]) extends PBWrapper[A]
  final case class Delay[A](value: () => Parser[A]) extends PBWrapper[A]
  object PBWrapper {
    implicit val functor: Functor[PBWrapper] = new Functor[PBWrapper] {
      override def map[A, B](fa: PBWrapper[A])(f: A => B): PBWrapper[B] = fa match {
        case Value(x) => Value(x.map(f))
        case Delay(x) => Delay(() => x().map(f))
      }
    }
  }

  def force[A](w: PBWrapper[A]): Parser[A] = w match {
    case Value(p) => p
    case Delay(p) => p()
  }

  implicit val parsebackParsing: Parsing[PBWrapper] { type Symbol = Char } =
    new Parsing[PBWrapper] {
      type Symbol = Char
      type F[A] = PBWrapper[A]

      def pure[A](a: A): F[A] = Value(Parser.Epsilon(a))

      def any: F[Symbol] =
        Value(Parser.Regex(".".r).map(_.head))

      def sym(symbol: Symbol): F[Unit] =
        Value(Parser.Literal(symbol.toString).map(_ => *))

      /** Language union. */
      def alt[A, B](A: F[A], B: F[B]): F[A \/ B] =
        Value(Parser.Union(() => force(A).map(-\/(_)), () => force(B).map(\/-(_))))

      /** Sequential composition. */
      def zip[A, B](A: F[A], B: F[B]): F[A /\ B] = (A, B) match {
        case (Value(pa), Value(pb)) => Value(Parser.Sequence(pa, None, pb))
        case (pa, pb) => Delay(() => Parser.Sequence(force(pa), None, force(pb)))
      }

      def delay[A](A: => F[A]): F[A] =
        Delay(() => force(A))

      def rule[A](name: String, f: F[A]): F[A] = f

      // THIS IS HORRIBLE, but technically correct.
      def anyOf(r: SymbolSet[Symbol]): F[r.Type] =
        r.enumerate.foldLeft(None : Option[Parser[r.Type]]) {
          case (None, s) => Some(Parser.Literal(s.toString).map(_ => s))
          case (Some(p), s) => Some(Parser.Union(() => p, () => Parser.Literal(s.toString).map(_ => s)))
        } match {
          case Some(p) => Value(p)
          case None => Value(Parser.Failure(Nil))
        }
    }
}
