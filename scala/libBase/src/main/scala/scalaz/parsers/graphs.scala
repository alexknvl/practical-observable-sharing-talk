package scalaz.parsers

object graphs {
  type Unique = BigInt
  final case class Graph[E[_]](nodes: Map[Unique, E[Unique]], id: Unique)
}
