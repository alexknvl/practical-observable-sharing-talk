package app

import cats.{Applicative, Functor, Invariant}
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all._
import cats.instances.char._
import parseback.LineStream
import scalaz.base._
import scalaz.parsers.cfg.{CFG, CFGP, printGraphAsBNF}
import scalaz.parsers.reified.reify
import scalaz.parsers.parsers.Parsing
import scalaz.parsers.backend.simple.Simple
import scalaz.parsers.symbols.SymbolSet
import scalaz.parsers.escapes.escapeJava
import scalaz.parsers.parsetree.{WithParseTree, parseTreeToDOT}

object Main {
  //\\ First we define a bunch of character sets.
  val LetterOrDigit = SymbolSet.Subset(SymbolSet.Ascii)(_.isLetterOrDigit)
  val Digit         = SymbolSet.Subset(LetterOrDigit)(_.isDigit)
  val Letter        = SymbolSet.Subset(LetterOrDigit)(_.isLetter)
  val PlusMinus     = SymbolSet.Subset(SymbolSet.Ascii)(x => x == '+' || x == '-')
  val Multiply      = SymbolSet.Subset(SymbolSet.Ascii)(x => x == '*')
  val Whitespace    = SymbolSet.Subset(SymbolSet.Ascii)(_.isWhitespace)

  type LetterOrDigit = LetterOrDigit.Type
  type Digit         = Digit.Type
  type Letter        = Letter.Type
  type PlusMinus     = PlusMinus.Type
  type Multiply      = Multiply.Type
  type Whitespace    = Whitespace.Type

  //\\ And now an ADT for our mini-language.
  final case class Number(minus: Boolean, digits: NonEmptyList[Digit])
  final case class Id(head: Letter, tail: List[LetterOrDigit])

  sealed abstract class Factor
  final case class FPar(value: Expr) extends Factor
  final case class FVar(value: Id) extends Factor
  final case class FNum(value: Number) extends Factor

  final case class Term(list: Separated1[Multiply, Factor])
  final case class Expr(list: Separated1[PlusMinus, Term])


  // These should be derived automatically.
  val idIso: Iso[Letter /\ List[LetterOrDigit], Id] =
    Iso.unsafe((Id.apply _).tupled, x => Id.unapply(x).get)
  val numberIso: Iso[Boolean /\ NonEmptyList[Digit], Number] =
    Iso.unsafe((Number.apply _).tupled, x => Number.unapply(x).get)
  def nelIso[A]: Iso[(A, List[A]), NonEmptyList[A]] =
    Iso.unsafe((NonEmptyList.apply[A] _).tupled, x => (x.head, x.tail))
  val factorIso: Iso[Expr \/ Id \/ Number, Factor] = Iso.unsafe(
    { case -\/(-\/(x)) => FPar(x)
    case -\/(\/-(x)) => FVar(x)
    case \/-(x)      => FNum(x) },
    { case FPar(x) => -\/(-\/(x))
    case FVar(x) => -\/(\/-(x))
    case FNum(x) => \/-(x) })
  val termIso: Iso[Separated1[Multiply, Factor], Term] =
    Iso.unsafe(x => Term(x), x => x.list)
  val exprIso: Iso[Separated1[PlusMinus, Term], Expr] =
    Iso.unsafe(x => Expr(x), x => x.list)

  //\\ This is only for the purpose of showing that
  //\\ the parser actually is working as intended.
  //\\ We can use the grammar to generate
  //\\ the printer instead.
  object HandWrittenShow {
    val char: Show[Char] = Show.universal

    val num: Show[Number] =
      a => s"${if(a.minus) "-" else ""}${a.digits.toList.mkString("")}"
    val id: Show[Id] =
      a => s"${a.head}${a.tail.mkString("")}"
    lazy val factor: Show[Factor] = {
      case FPar(e) => "(" + expr(e) + ")"
      case FVar(a) => id(a)
      case FNum(n) => num(n)
    }
    lazy val term: Show[Term] =
      factor.sep1(" ", char.contramap[Multiply](identity))
        .contramap(_.list)
    lazy val expr: Show[Expr] =
      term.sep1(" ", char.contramap[PlusMinus](identity))
        .contramap(_.list)
  }

  //\\ Now we define our grammar.
  //\\ Think of it as a polymorphic function, returning a record.
  //\\ Or a bunch of mutually recursive polymorphic functions.
  def calc[F[_]]
  (implicit F: Parsing[F] { type Symbol = Char },
   I: Invariant[F]
  ): F[Expr] = {
    import F._

    val digit         = "digit"          @: anyOf(Digit)
    val letter        = "letter"         @: anyOf(Letter)
    val letterOrDigit = "letterOrDigit"  @: anyOf(LetterOrDigit)
    val plusMinus     = "plusMinus"      @: anyOf(PlusMinus)
    val multiply      = "multiply"       @: anyOf(Multiply)

    val variable      = "id"             @: { letter ~ letterOrDigit.* ^^ idIso }
    val number        = "num"            @: { sym('-').?? ~ digit.+ ^^ numberIso }

    lazy val factor         = "factor"   @: {
      (sym('(') ~> expr <~ sym(')') |
      variable | number) ^^ factorIso }
    lazy val term           = "term"     @: { factor.sepBy1(multiply) ^^ termIso }
    lazy val expr: F[Expr]  = "expr"     @: { term.sepBy1(plusMinus) ^^ exprIso }

    expr
  }

  val testSimpleAndShow: IO[Unit] = {
    val p = calc[Simple[Char, ?]]
    val List(tree) = p.parseAll("(a+12)*(3+5*x)")

    val handWritten = HandWrittenShow.expr(tree)
    val derived     = calc[Show].apply(tree)

    putStrLn(handWritten) *>
      putStrLn(derived) *>
      putStrLn("\n")
  }

  val testParseTrees: IO[Unit] = {
    val p = calc[WithParseTree[Simple[Char, ?], Char, ?]]
    val List(Some(pt) /\ _) = p.run.parseAll("(a+12)*(3+5*x)")
    val result = parseTreeToDOT(pt, "?", (ch: Char) => escapeJava(ch.toString))
    putStrLn(result) *> putStrLn("\n")
  }

  val testBNF: IO[Unit] = {
    val cfg = calc[CFGP[Char, ?]]

    for {
      graph <- reify(cfg.get)
      bnf = printGraphAsBNF(graph)
      _ <- putStrLn(bnf)
      _ <- putStrLn("\n")
    } yield ()
  }

  //\\ Now we define a normalizing grammar.
  def normalizingCalc[F[_]]
  (implicit F: Parsing[F] { type Symbol = Char },
   I: Invariant[F]
  ): F[Expr] = {
    import F._

    val normMaybeWS: Inv[List[Whitespace], Unit] =
      Inv(_ => (), _ => List())

    val maybeWS : F[Unit] = "maybeWS" @: { anyOf(Whitespace).* ^^ normMaybeWS }

    val digit         = "digit"          @: anyOf(Digit)
    val letter        = "letter"         @: anyOf(Letter)
    val letterOrDigit = "letterOrDigit"  @: anyOf(LetterOrDigit)
    val plusMinus     = "plusMinus"      @: anyOf(PlusMinus)
    val multiply      = "multiply"       @: anyOf(Multiply)

    val variable      = "id"  @: { letter ~ letterOrDigit.* ^^ idIso }
    val number        = "num" @: { sym('-').?? ~ digit.+ ^^ numberIso }

    lazy val factor         = "factor" @: { (sym('(') ~> maybeWS ~> expr <~ maybeWS <~ sym(')') |
      variable | number) ^^ factorIso }
    lazy val term           = "term"   @: { factor.sepBy1(maybeWS ~> multiply <~ maybeWS) ^^ termIso }
    lazy val expr: F[Expr]  = "expr"   @: { term.sepBy1(maybeWS ~> plusMinus <~ maybeWS) ^^ exprIso }

    expr
  }

  val testNormalizingSimpleAndShow: IO[Unit] = {
    val simpleCalc       = normalizingCalc[Simple[Char, ?]]
    val showCalc         = normalizingCalc[Show]

    val p = simpleCalc
    val List(tree) = p.parseAll("(  a +  12) * ( 3 +5*x)")

    val handWritten = HandWrittenShow.expr(tree)
    val derived     = showCalc(tree)

    putStrLn(handWritten) *>
      putStrLn(derived) *>
      putStrLn("\n")
  }

  val testNormalizingParseTrees: IO[Unit] = {
    val simpleWithPTCalc = normalizingCalc[WithParseTree[Simple[Char, ?], Char, ?]]

    val p = simpleWithPTCalc
    val List(Some(pt) /\ _) = p.run.parseAll("(  a +  12) * ( 3 +5*x)")
    val result = parseTreeToDOT(pt, "?", (ch: Char) => escapeJava(ch.toString))
    putStrLn(result) *> putStrLn("\n")
  }

  val testNormalizingBNF: IO[Unit] = {
    val cfg = normalizingCalc[CFGP[Char, ?]]

    for {
      graph <- reify(cfg.get)
      bnf = printGraphAsBNF(graph)
      _ <- putStrLn(bnf)
      _ <- putStrLn("\n")
    } yield ()
  }

  val testParseback: IO[Unit] = {
    import parseback._
    import parseback.compat.cats._
    import scalaz.parsers.backend.parseback._

    {
      val p = force(calc[PBWrapper])
      val stream = LineStream[cats.Eval]("(a+12)*(3+5*x)")
      val \/-(result) = p.apply(stream).value
      val Some(first /\ rest) = result.uncons
      assert(rest.isEmpty)
      putStrLn(HandWrittenShow.expr(first))
    }
  }

  def putStrLn(s: String): IO[Unit] = IO { println(s) }

  def pureMain: IO[Unit] =
    testSimpleAndShow *> testParseTrees *> testBNF *>
    testNormalizingSimpleAndShow *> testNormalizingParseTrees *> testNormalizingBNF *>
    testParseback

  def main(args: Array[String]): Unit =
    pureMain.unsafeRunSync()
}
