package com.rockthejvm.interviews

object Graph extends App {
  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  def outDegree[T](graph: Graph[T], node: T): Int = graph.getOrElse(node, Set()).size


  def inDegree[T](graph: Graph[T], node: T): Int = graph.count {
    case (_, friends) => friends.contains(node)
  }

  println("outDegree")
  println(outDegree(socialNetwork, "Alice"))
  println()
  println("inDegree")
  println(inDegree(socialNetwork, "David"))

}