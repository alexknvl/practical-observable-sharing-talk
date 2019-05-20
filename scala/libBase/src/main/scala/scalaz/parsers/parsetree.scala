package scalaz.parsers

import cats.{Functor, Monad, Monoid, Semigroup}
import cats.data.{NonEmptyList, State}
import cats.syntax.all._
import cats.instances.string._
import scalaz.base._
import scalaz.parsers.parsers.Parsing
import scalaz.parsers.symbols.SymbolSet

object parsetree {
  //\\ Let's define an ADT for parsing trees.
  sealed abstract class ParseTree[+S]
  final case class PTLeaf[S](c: S) extends ParseTree[S]
  final case class PTUnnamed[S](children: NonEmptyList[ParseTree[S]]) extends ParseTree[S]
  final case class PTNamed[S](name: String, children: NonEmptyList[ParseTree[S]]) extends ParseTree[S]

  implicit def parseTreeMonoid[S]: Semigroup[ParseTree[S]] = new Semigroup[ParseTree[S]] {
    def combine(t1: ParseTree[S], t2: ParseTree[S]): ParseTree[S] = (t1, t2) match {
      case (PTLeaf(_) | PTNamed(_, _)) /\ (PTLeaf(_) | PTNamed(_, _)) =>
        PTUnnamed(NonEmptyList.of(t1, t2))

      case (PTLeaf(_) | PTNamed(_, _)) /\ PTUnnamed(t2) => PTUnnamed(t1 :: t2)
      case PTUnnamed(t1) /\ (PTLeaf(_) | PTNamed(_, _)) => PTUnnamed(t1.concat(List(t2)))
      case PTUnnamed(t1) /\ PTUnnamed(t2) => PTUnnamed(t1 |+| t2)
    }
  }

  def parseTreeToDOT[S](p: ParseTree[S], default: String, render: S => String): String = {
    def gosub[F[_]: Monad](name: String, children: NonEmptyList[ParseTree[S]], next: F[Int]): F[Int /\ String] =
      for {
        chlds <- children.traverse(go(_, next))
        root <- next
      } yield {
        root /\ (
          s"""   $root [label="${name}"]""" ++ "\n" ++
            chlds.foldMap { case i /\ s => s ++ s"""   $root -> $i""" ++ "\n" }
          )
      }

    def go[F[_]: Monad](p: ParseTree[S], next: F[Int]): F[Int /\ String] = p match {
      case PTLeaf(x) => next.map(i => i /\ (s"""   $i [label="${render(x)}"]""" ++ "\n"))
      case PTUnnamed(x) => gosub(default, x, next)
      case PTNamed(name, x) => gosub(name, x, next)
    }

    val next: State[Int, Int] = State { s => (s + 1, s) }

    val tree: ParseTree[S] = p match {
      case x@PTLeaf(_) => PTNamed(default, NonEmptyList.of(x))
      case PTUnnamed(x) => PTNamed(default, x)
      case x => x
    }

    "digraph G {\n" ++ go[State[Int, ?]](tree, next).run(0).value._2._2 ++ "}"
  }

  type WithParseTree[F[_], S, A] = WriterT[F, Option[ParseTree[S]], A]

  implicit def parsingWithParseTree[G[_], S]
  (implicit G: Functor[G], PG: Parsing[G] { type Symbol = S }
  ): Parsing[WithParseTree[G, S, ?]] { type Symbol = S } =
    new Parsing[WithParseTree[G, S, ?]]  {
      type Symbol = S
      type F[A] = WithParseTree[G, S, A]

      def pure[A](a: A): F[A] =
        WriterT(PG.pure(a).map(a => None /\ a))

      def any: F[Symbol] =
        WriterT(PG.any.map(a => PTLeaf(a).some /\ a))

      def sym(symbol: Symbol): F[Unit] =
        WriterT(PG.sym(symbol).map(a => PTLeaf(symbol).some /\ a))

      def iso[A, B](A: F[A])(f: Iso[A, B]): F[B] =
        WriterT(PG.iso(A.run)(Iso.id and f))

      /** Language union. */
      def alt[A, B](A: F[A], B: F[B]): F[A \/ B] =
        WriterT(PG.alt(A.run, B.run).map {
          case -\/(s /\ x) => s /\ -\/(x)
          case \/-(s /\ x) => s /\ \/-(x)
        })

      /** Sequential composition. */
      def zip[A, B](A: F[A], B: F[B]): F[A /\ B] =
        WriterT(PG.zip(A.run, B.run).map {
          case (t1 /\ a) /\ (t2 /\ b) =>
            val t = (t1, t2) match {
              case None /\ a => a
              case a /\ None => a
              case Some(x) /\ Some(y) => Some(x |+| y)
            }
            t /\ (a /\ b)
        })

      def delay[A](A: => F[A]): F[A] =
        WriterT(PG.delay(A.run))

      def rule[A](name: String, f: F[A]): F[A] =
        WriterT(f.run.map { case t /\ a =>
          (t match {
            case None => None
            case Some(t@(PTLeaf(_) | PTNamed(_, _))) =>
              PTNamed(name, NonEmptyList.of(t)).some
            case Some(PTUnnamed(x)) => PTNamed(name, x).some
          }) /\ a
        })

      def anyOf(r: SymbolSet[Symbol]): F[r.Type] =
        WriterT(PG.anyOf(r).map(a => PTLeaf(a).some /\ a))

//      def end: F[Unit] = WriterT(PG.end.map(a => None /\ a))
    }
}
