package scalaz.parsers

import cats.Applicative
import cats.effect.IO
import scalaz.parsers.graphs.Graph
import scalaz.parsers.refeq.RefEq

object reified {
  // see http://www.ittc.ku.edu/~andygill/papers/reifyGraph.pdf
  // and https://hackage.haskell.org/package/data-reify

  trait MuRef[A] {
    type DeRef[X]
    def mapDeRef[F[_], U](a: A)(f: A => F[U])(implicit F: Applicative[F]): F[DeRef[U]]
  }

  private[this] final class Context[RefId, F[_]] private () {
    private[this] var _idMap: Map[RefId, BigInt] = Map.empty
    private[this] var _pfMap: Map[BigInt, F[BigInt]] = Map.empty
    private[this] var _uniqueId: BigInt = BigInt(0)

    def pfMap: IO[Map[BigInt, F[BigInt]]] = IO { _pfMap }

    def lookupRefId(id: RefId): IO[Option[BigInt]] = IO { _idMap.get(id) }

    def newUnique: IO[BigInt] = IO {
      val result = _uniqueId
      _uniqueId += 1
      result
    }

    def addRefId(id: RefId, gid: BigInt): IO[Unit] = IO { _idMap += id -> gid }

    def addPF(from: BigInt, ref: F[BigInt]): IO[Unit] = IO { _pfMap += from -> ref }
  }
  private[this] object Context {
    def make[RefId, F[_]]: IO[Context[RefId, F]] = IO { new Context[RefId, F] }
  }

  def reify[T](t: T)(implicit T: MuRef[T], S: RefEq[T]): IO[Graph[T.DeRef]] = for {
    ctx <- Context.make[S.Id, T.DeRef]
    r   <- reifyWithContext[T, S.Id, T.DeRef](t, ctx)(T, S)
  } yield r

  private[this] def reifyWithContext[T, I, F[_]](t: T, ctx: Context[I, F])
                                                (implicit T: MuRef[T] { type DeRef[X] = F[X] }, S: RefEq[T] { type Id = I }): IO[Graph[F]] = for {
    root  <- findNodes[T, I, F](t, ctx, Set())
    pfMap <- ctx.pfMap
  } yield Graph(pfMap, root)

  private[this] def findNodes[T, I, F[_]](j: T, ctx: Context[I, F], s: Set[BigInt])
                                         (implicit T: MuRef[T] { type DeRef[X] = F[X] }, S: RefEq[T] { type Id = I }): IO[BigInt] = for {
    refId  <- S.id(j)
    optId  <- ctx.lookupRefId(refId)
    root   <- optId match {
      case Some(id) =>
        if (s.contains(id)) IO.pure(id)
        else for {
          res <- T.mapDeRef(j)(n => findNodes[T, I, F](n, ctx, s + id))
          _   <- ctx.addPF(id, res)
        } yield id
      case None => for {
        id  <- ctx.newUnique
        _   <- ctx.addRefId(refId, id)
        res <- T.mapDeRef(j)(n => findNodes[T, I, F](n, ctx, s + id))
        _   <- ctx.addPF(id, res)
      } yield id
    }
  } yield root
}
