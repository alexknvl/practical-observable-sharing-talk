package scalaz

package object base {
  type \/[+A, +B] = A Either B
  val -\/ : Left.type  = Left
  val \/- : Right.type = Right
  implicit class either_ops[A, B](val self: A \/ B) {
    def leftMap[C](f: A => C): C \/ B = self match {
      case \/-(b) => \/-(b)
      case -\/(a) => -\/(f(a))
    }
  }

  type /\[+A, +B] = (A, B)
  val /\ : Tuple2.type = Tuple2
  implicit class ToExtraTuple2Ops[A](val a: A) {
    def /\[B](b: B): (A, B) = (a, b)
  }

  val * : Unit = ()
}
