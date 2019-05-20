package scalaz.parsers

import cats.Order
import cats.instances.char._
import cats.syntax.order._
import scalaz.base._

object symbols {
  sealed abstract class SymbolSet[P] protected () {
    type Type <: P

    def isValid(p: P): Boolean

    def unapply(ch: P): Option[Type] =
      if (isValid(ch)) Some(ch.asInstanceOf[Type]) else None

    def prism : Prism[P, Type] =
      Prism.unsafe(unapply, t => t)

    def enumerate(implicit E: Enumerable[P]): List[Type]
    def intervals(implicit E: Enumerable[P]): List[(Type, Type)]
  }
  object SymbolSet {
    final case class Range[P: Order](ch1: P, ch2: P) extends SymbolSet[P] {
      val start: Type = (ch1 min ch2).asInstanceOf[Type]
      val end: Type   = (ch1 max ch2).asInstanceOf[Type]

      def isValid(c : P): Boolean =
        (start: P) <= c && c <= (end: P)

      // FIXME: technically .flatMap(unapply) is identity
      def enumerate(implicit E: Enumerable[P]): List[Type] =
        E.range(start, end).flatMap(unapply)

      def intervals(implicit E: Enumerable[P]) =
        List(start /\ end)
    }

    final case class Subset[P](sup: SymbolSet[P])(pred: P => Boolean) extends SymbolSet[P] {
      def bounds: (P, P) = sup match {
        case r@Range(_, _) => (r.start, r.end)
        case r@Subset(_)   => r.bounds
      }

      def isValid(x: P): Boolean =
        sup.isValid(x) && pred(x)

      def enumerate(implicit E: Enumerable[P]): List[Type] = {
        val (l, u) = bounds
        E.range(l, u).flatMap(unapply)
      }

      def intervals(implicit E: Enumerable[P]): List[(Type, Type)] = {
        val (start, end) = bounds
        type State = List[Type /\ Type] /\ Option[Type /\ Type]
        E.range(start, end).foldLeft(Nil /\ None : State) {
          case (l /\ None, sym) =>
            unapply(sym) match {
              case Some(validSym) => l /\ Some(validSym /\ validSym)
              case None => l /\ None
            }
          case (l /\ Some(p1 /\ p2), sym) =>
            unapply(sym) match {
              case Some(p3) => l /\ Some(p1 /\ p3)
              case None => (p1 /\ p2 :: l) /\ None
            }
        } match {
          case l /\ None    => l.reverse
          case l /\ Some(p) => (p :: l).reverse
        }
      }
    }

    val Ascii: SymbolSet[Char] = Range('\u0000', '\u007F')
    type Ascii = Ascii.Type
  }

}
