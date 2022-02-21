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

  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    def isPathAux(toVisit: Set[T], visited: Set[T]): Boolean =
      if (toVisit.isEmpty) false
      else if (toVisit.contains(end)) true
      else toVisit.exists(t => {
        if (visited.contains(t)) false
        else isPathAux(graph.getOrElse(t, Set()), visited + t)
      })

    isPathAux(graph.getOrElse(start, Set()), Set())
  }

  println("outDegree")
  println(outDegree(socialNetwork, "Alice"))
  println()
  println("inDegree")
  println(inDegree(socialNetwork, "David"))
  println()
  println("inDegree")
  println(isPath(socialNetwork, "Alice", "Mary"))
  println(isPath(socialNetwork, "David", "Alice"))

}
