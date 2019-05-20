package scalaz.parsers.backend

import cats.{Eq, Functor}
import cats.syntax.all._
import scalaz.base._
import scalaz.parsers.parsers.Parsing
import scalaz.parsers.symbols.SymbolSet

object simple {
  final case class Simple[S, A](runParser: List[S] => List[(List[S], A)])

  implicit class parseAll[A](val p: Simple[Char, A]) extends AnyVal {
    def apply(p: Simple[Char, A], s: String): List[A] = parseAll(s)
    def parseAll(s: String): List[A] = p.runParser(s.toList).collect {
      case (Nil, a) => a
    }
  }

  object Simple {
    implicit def simpleFunctor[S]: Functor[Simple[S, ?]] = new Functor[Simple[S, ?]] {
      def map[A, B](A: Simple[S, A])(f: A => B): Simple[S, B] = Simple { s =>
        A.runParser(s).map { case s /\ a => s /\ f(a) }
      }
    }

    implicit def simpleParsing[S: Eq]: Parsing[Simple[S, ?]] { type Symbol = S } =
      new Parsing[Simple[S, ?]] {
        type Symbol = S
        type F[A] = Simple[S, A]

        def pure[A](a: A): F[A] = Simple(s => List(s /\ a))

        def any: F[Symbol] = Simple { s =>
          if (s.nonEmpty) List(s.tail /\ s.head)
          else Nil
        }

        def sym(symbol: Symbol): F[Unit] = Simple { s =>
          if (s.nonEmpty && s.head === symbol) List(s.tail /\ *)
          else Nil
        }

        /** Language union. */
        def alt[A, B](A: F[A], B: F[B]): F[A \/ B] = Simple { s =>
          A.runParser(s).map { case s /\ a => s /\ -\/(a) } ++
            B.runParser(s).map { case s /\ a => s /\ \/-(a) }
        }

        /** Sequential composition. */
        def zip[A, B](A: F[A], B: F[B]): F[A /\ B] = Simple { s =>
          A.runParser(s).flatMap { case s /\ a =>
            B.runParser(s).map { case s /\ b => s /\ ((a, b)) }
          }
        }

        def delay[A](A: => F[A]): F[A] = {
          lazy val p: F[A] = A
          Simple { s => p.runParser(s) }
        }

        def rule[A](name: String, f: F[A]): F[A] = f

        def anyOf(r: SymbolSet[Symbol]): F[r.Type] = Simple { s =>
          if (s.nonEmpty) {
            r.unapply(s.head) match {
              case Some(x) => List(s.tail /\ x)
              case None => Nil
            }
          } else Nil
        }

//        def end: F[Unit] = Simple { s =>
//          if (s.isEmpty) List(s /\ *)
//          else Nil
//        }
      }
  }
}
