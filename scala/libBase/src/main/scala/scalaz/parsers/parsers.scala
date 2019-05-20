package scalaz.parsers

import cats.Invariant
import cats.data.NonEmptyList
import cats.evidence.===
import scalaz.base._
import scalaz.parsers.symbols.SymbolSet

object parsers {
  trait Parsing[F[_]] extends ParsingDerived[F] with Syntax {
    type Symbol

    /** Accepts empty input. */
    def pure[A](a: A): F[A]

    /** Accepts any symbol. */
    def any: F[Symbol]

    /** Accepts a specific symbol. */
    def sym(symbol: Symbol): F[Unit]

    /** Language union. */
    def alt[A, B](A: F[A], B: F[B]): F[A \/ B]

    /** Sequential composition. */
    def zip[A, B](A: F[A], B: F[B]): F[A /\ B]

    def delay[A](A: => F[A]): F[A]

    def rule[A](name: String, f: F[A]): F[A]

    def anyOf(r: SymbolSet[Symbol]): F[r.Type]
  }

  object Parsing {
    def apply[F[_]](implicit F: Parsing[F] { }): Parsing[F] { type Symbol = F.Symbol } = F
  }

  trait ParsingDerived[F[_]] { self: Parsing[F] =>
    def unit: F[Unit] = pure(())

    def iso[A, B](fa: F[A])(f: Iso[A, B])(implicit I: Invariant[F]): F[B] =
      I.imap(fa)(f.to)(f.from)

    def opt[A](A: F[A])(implicit I: Invariant[F]): F[Option[A]] =
      iso(alt(unit, A))(optIso)

    def bool(A: F[Unit])(implicit I: Invariant[F]): F[Boolean] =
      iso(alt(unit, A))(boolIso)

    def zipL[A](A: F[A], B: F[Unit])(implicit I: Invariant[F]): F[A] =
      iso(zip(A, B))(Iso.Product.unitR[A].flip)

    def zipR[B](A: F[Unit], B: F[B])(implicit I: Invariant[F]): F[B] =
      iso(zip(A, B))(Iso.Product.unitL[B].flip)

    def many[A](A: F[A])(implicit I: Invariant[F]): F[List[A]] = {
      lazy val r: F[List[A]] =
        iso(alt(unit, zip(A, delay(r))))(listIso)
      r
    }
    def many1[A](A: F[A])(implicit I: Invariant[F]): F[NonEmptyList[A]] =
      iso(zip(A, many(A)))(nelIso)

    def sepBy1[S, A](S: F[S], A: F[A])(implicit I: Invariant[F]): F[Separated1[S, A]] = {
      lazy val r: F[Separated1[S, A]] =
        iso(zip(A, opt(zip(S, delay(r)))))(Separated1.iso)
      r
    }

    def sepBy[S, A](S: F[S], A: F[A])(implicit I: Invariant[F]): F[Separated[S, A]] =
      iso(opt(sepBy1(S, A)))(Separated.iso)
  }

  trait Syntax {
    implicit def toParsingOps[F[_], A](A: F[A]): ToParsingOps[F, A] = new ToParsingOps(A)
    implicit def toLazyParsingOps[F[_], A](A: => F[A]): ToLazyParsingOps[F, A] = new ToLazyParsingOps(A)

    implicit def toInvParsingOps1[F[_], A](A: F[A]) = new ToInvParsingOps1(A)
    implicit def toInvParsingOps2[F[_], A, B](A: F[A /\ B]) = new ToInvParsingOps2(A)
    implicit def toInvParsingOps3_1[F[_], A, B, C](A: F[(A /\ B) /\ C]) = new ToInvParsingOps3_1(A)
  }
  object syntax extends Syntax

  final class ToParsingOps[F[_], A](val A: F[A]) extends AnyVal {
    type PF = Parsing[F]

    def @: (name: String)(implicit F: PF): F[A] = F.rule(name, A)

    def ? (implicit F: PF, I: Invariant[F]): F[Option[A]] = F.opt(A)
    def ?? (implicit F: PF, ev: A === Unit, I: Invariant[F]): F[Boolean] = F.bool(ev.substitute(A))

    def ~ [B](B: F[B])(implicit F: PF, I: Invariant[F]): F[(A, B)] = F.zip(A, B)
    def ~> [B](B: F[B])(implicit F: PF, ev: A === Unit, I: Invariant[F]): F[B] = F.zipR(ev.substitute(A), B)
    def <~ (B: F[Unit])(implicit F: PF, I: Invariant[F]): F[A] = F.zipL(A, B)

    def * (implicit F: PF, I: Invariant[F]): F[List[A]] = F.many(A)
    def + (implicit F: PF, I: Invariant[F]): F[NonEmptyList[A]] = F.many1(A)

    def sepBy [S](S: F[S])(implicit F: PF, I: Invariant[F]): F[Separated[S, A]] = F.sepBy(S, A)
    def sepBy1[S](S: F[S])(implicit F: PF, I: Invariant[F]): F[Separated1[S, A]] = F.sepBy1(S, A)
  }

  final class ToLazyParsingOps[F[_], A](A: => F[A]) {
    def | [B](B: => F[B])(implicit F: Parsing[F]): F[A \/ B] =
      F.alt(F.delay(A), F.delay(B))
  }

  final class ToInvParsingOps1[F[_], A](val A: F[A]) {
    def ^^[Z](f: Inv[A, Z])(implicit F: Parsing[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
    def ^^[Z](f: Iso[A, Z])(implicit F: Parsing[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
  }
  final class ToInvParsingOps2[F[_], A, B](val A: F[A /\ B]) {
    def ^^[Z](f: Inv[(A, B), Z])(implicit F: Parsing[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
    def ^^[Z](f: Iso[(A, B), Z])(implicit F: Parsing[F], FF: Invariant[F]): F[Z] =
      FF.imap(A)(f.to)(f.from)
  }
  final class ToInvParsingOps3_1[F[_], A, B, C](val A: F[(A /\ B) /\ C]) {
    def ^^[Z](f: Inv[(A, B, C), Z])(implicit F: Parsing[F], FF: Invariant[F]): F[Z] =
      FF.imap(F.iso(A)(tupleIso3_1))(f.to)(f.from)
    def ^^[Z](f: Iso[(A, B, C), Z])(implicit F: Parsing[F], FF: Invariant[F]): F[Z] =
      FF.imap(F.iso(A)(tupleIso3_1))(f.to)(f.from)
  }

  def nelIso[A]: Iso[(A, List[A]), NonEmptyList[A]] =
    Iso.unsafe((NonEmptyList.apply[A] _).tupled, x => (x.head, x.tail))

  def listIso[A]: Iso[Unit \/ (A /\ List[A]), List[A]] = Iso.unsafe(
    { case -\/(()) => Nil; case \/-(h /\ t) => h :: t },
    { case x :: xs => \/-(x /\ xs); case Nil => -\/(()) })

  def boolIso[A]: Iso[Unit \/ Unit, Boolean] = Iso.unsafe[Unit \/ Unit, Boolean]({
    case -\/(()) => false
    case \/-(()) => true
  }, {
    case false => -\/(())
    case true  => \/-(())
  })
  def tupleIso3_1[A, B, C]: Iso[(A /\ B) /\ C, (A, B, C)] = Iso.unsafe(
    { case ((a, b), c) => (a, b, c) },
    { case (a, b, c) => ((a, b), c) })

  def optIso[A]: Iso[Unit \/ A, Option[A]] = Iso.unsafe({
    case -\/(()) => None
    case \/-(x)  => Some(x)
  }, {
    case None    => -\/(())
    case Some(x) => \/-(x)
  })
}
