package scalaz.parsers

import cats.{Applicative, Contravariant, Functor, Monoid}
import cats.data.Const
import cats.syntax.all._
import cats.instances.list._
import scalaz.base._
import scalaz.parsers.parsers.Parsing
import scalaz.parsers.refeq.RefEq
import scalaz.parsers.reified.MuRef
import scalaz.parsers.symbols.SymbolSet
import escapes._
import scalaz.parsers.graphs.{Graph, Unique}

object cfg {
  sealed abstract class CFG[+S]
  object CFG {
    final case object EOF extends CFG[Nothing]
    final case object Any extends CFG[Nothing]
    final case class Rng[S](lower: S, upper: S) extends CFG[S]
    final case class Nam[S](name: String, value: CFG[S]) extends CFG[S]
    final case class Alt[S](list: List[CFG[S]]) extends CFG[S]
    final case class Seq[S](list: List[CFG[S]]) extends CFG[S]
    final class Del[S](x: => CFG[S]) extends CFG[S] {
      lazy val value: CFG[S] = x
    }

    implicit def muRef[S]: MuRef[CFG[S]] { type DeRef[X] = CFGF[S, X] } =
      new MuRef[CFG[S]] {
        type DeRef[X] = CFGF[S, X]
        def mapDeRef[F[_], U](cfg: CFG[S])(f: CFG[S] => F[U])(implicit F: Applicative[F]): F[DeRef[U]] =
          cfg match {
            case EOF       => F.pure(CFGF.EOF)
            case Any       => F.pure(CFGF.Any)
            case Rng(l, u) => F.pure(CFGF.Rng(l, u))
            case Nam(n, v) => f(v).map(CFGF.Nam(n, _))
            case Alt(l)    => l.traverse(f).map(CFGF.Alt(_))
            case Seq(l)    => l.traverse(f).map(CFGF.Seq(_))

            case cfg : Del[S] => f(cfg.value).map(CFGF.Del(_))
          }
      }

    implicit def monoid[S]: Monoid[CFG[S]] = new Monoid[CFG[S]] {
      def empty: CFG[S] = Seq(Nil)
      def combine(a: CFG[S], b: CFG[S]): CFG[S] = (a, b) match {
        case Seq(a) /\ Seq(b) => Seq(a ++ b)
        case Seq(a) /\ b      => Seq(a :+ b)
        case a /\ Seq(b)      => Seq(a +: b)
        case a /\ b           => Seq(List(a, b))
      }
    }

    implicit def refEq[S]: RefEq[CFG[S]] { type Id = AnyRef } =
      RefEq.universal[CFG[S]]
  }

  sealed abstract class CFGF[+S, +A]
  object CFGF {
    final case object EOF extends CFGF[Nothing, Nothing]
    final case object Any extends CFGF[Nothing, Nothing]
    final case class Rng[S](lower: S, upper: S) extends CFGF[S, Nothing]
    final case class Nam[A](name: String, value: A) extends CFGF[Nothing, A]
    final case class Alt[A](list: List[A]) extends CFGF[Nothing, A]
    final case class Seq[A](list: List[A]) extends CFGF[Nothing, A]
    final case class Del[A](value: A) extends CFGF[Nothing, A]
  }

  def printGraphAsBNF(g: Graph[CFGF[Char, ?]]): String = {
    import scala.collection.{ mutable => mut }
    import java.{ util => ju }

    import CFGF._

    def refs[S, A](n: CFGF[S, A]): List[A] = n match {
      case EOF | Any | Rng(_, _) => Nil
      case Nam(_, a) => List(a)
      case Alt(l) => l
      case Seq(l) => l
      case Del(a) => List(a)
    }

    var nextId = 0

    val queue = new ju.ArrayDeque[Unique]
    val visited = mut.Set.empty[BigInt]
    val names = mut.Map.empty[BigInt, String]
    queue.add(g.id)

    while (queue.size() > 0) {
      val id = queue.pop()
      visited += id

      g.nodes(id) match {
        case Nam(n, j) =>
          names += (id /\ n)
          queue.addFirst(j)

        case node => refs(node).foreach { j =>
          if (visited(j)) {
            if (!names.contains(j)) {
              names(j) = s"anon$j"
              nextId += 1
            }
          } else {
            queue.addFirst(j)
          }
        }
      }
    }

    def printNode(node: CFGF[Char, BigInt], inSeq: Boolean): String =
      node match {
        case EOF => "EOF"
        case Any => "ANY"
        case Rng(a, b) =>
          if (a == b) "\"" + escapeJava(a.toString) + "\""
          else if (a.toInt + 4 >= b.toInt) {
            val s = (a to b).map { x => "\"" + escapeJava(x.toString) + "\"" }.mkString(" | ")
            if (inSeq) "(" + s + ")" else s
          } else s"[${escapeJava(a.toString)}-${escapeJava(b.toString)}]"

        case Alt(List()) => "FAIL"
        case Alt(List(a)) => print(a, true, false)
        case Alt(as) =>
          val s = as.map(print(_, true, false)).mkString(" | ")
          if (inSeq) "(" + s + ")" else s

        case Seq(List()) => "ε"
        case Seq(List(a)) => print(a, true, true)
        case Seq(as) => as.map(print(_, true, true)).mkString(" ")

        case Del(a) => print(a, true, inSeq)

        case Nam(_, a) => sys.error("impossible")
      }

    def print(id: Unique, lookup: Boolean, inSeq: Boolean): String =
      if (lookup && names.contains(id)) { names(id) }
      else printNode(g.nodes(id), inSeq)

    def print1(id: BigInt): String = g.nodes(id) match {
      case Nam(_, a) => print(a, true, false)
      case a         => print(id, false, false)
    }

    def printRule(id: BigInt, name: String): String =
      s"$name := " + print1(id)

    names.toList.map { case (i, n) => printRule(i, n) }.mkString("\n")
  }

  final case class CFGP[S, A](get: CFG[S])
  object CFGP {
    implicit def instance[S]: Functor[CFGP[S, ?]] with Contravariant[CFGP[S, ?]] =
      new Functor[CFGP[S, ?]] with Contravariant[CFGP[S, ?]] {
        override def map[A, B](fa: CFGP[S, A])(f: A => B): CFGP[S, B] = CFGP(fa.get)
        override def contramap[A, B](fa: CFGP[S, A])(f: B => A): CFGP[S, B] = CFGP(fa.get)
      }

    implicit def cfgParsing[S: Enumerable]: Parsing[CFGP[S, ?]] { type Symbol = S } =
      new Parsing[CFGP[S, ?]] {
        import CFG._
        type Symbol = S
        type F[A] = CFGP[S, A]

//        def end: F[Unit]               = CFGP(EOF)
        def pure[A](a: A)  : F[A]      = CFGP(Seq(Nil))
        def any            : F[Symbol] = CFGP(Any)
        def sym(s: Symbol) : F[Unit]   = CFGP(Rng(s, s))
        def anyOf(r: SymbolSet[Symbol]): F[r.Type] =
          CFGP(Alt(r.intervals.map((Rng.apply[Symbol] _).tupled)))

        def rule[A](n: String, f: F[A]): F[A] = CFGP(Nam(n, f.get))

        def alt[A, B](A: F[A], B: F[B]): F[A \/ B] = CFGP((A.get, B.get) match {
          case Alt(a) /\ Alt(b) => Alt(a ++ b)
          case Alt(a) /\ b      => Alt(a :+ b)
          case a /\ Alt(b)      => Alt(a +: b)
          case a /\ b           => Alt(List(a, b))
        })

        def zip[A, B](A: F[A], B: F[B]): F[A /\ B] =
          CFGP(A.get |+| B.get)

        def delay[A](A: => F[A]): F[A] = CFGP(new Del(A.get))
      }
  }
}
