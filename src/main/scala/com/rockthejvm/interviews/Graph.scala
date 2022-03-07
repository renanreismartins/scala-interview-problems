package com.rockthejvm.interviews

import scala.annotation.tailrec

object Graph extends App {
  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
    //    "Mary" -> Set("Bob", "Charlie", "Alice")
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


  def isPathTR[T](graph: Graph[T], start: T, end: T): Boolean = {
    @tailrec
    def isPathTRAux(toVisit: Set[T], visited: Set[T]): Boolean =
      if (toVisit.isEmpty) false
      else if (toVisit.contains(end)) true
      else {
        val allNonVisitedChildren = toVisit.flatMap(e => graph.getOrElse(e, Set())) -- visited
        isPathTRAux(allNonVisitedChildren, visited ++ toVisit)
      }

    isPathTRAux(graph.getOrElse(start, Set()), Set())
  }

  def findPathTr[T](graph: Graph[T], start: T, end: T): List[T] = {
    @tailrec
    def findPathTrAux(toVisit: List[T], visited: Set[T], acc: List[T]): List[T] =
      if (toVisit.isEmpty) List()
      else if (toVisit.head == end) acc :+ toVisit.head
      else {
        if (visited.contains(toVisit.head)) findPathTrAux(toVisit.tail, visited + toVisit.head, acc)
        else findPathTrAux(toVisit.tail ++ graph.getOrElse(toVisit.head, List()), visited + toVisit.head, if (graph.getOrElse(toVisit.head, List()).isEmpty) acc else acc :+ toVisit.head)
      }

    findPathTrAux(List(start), Set(), List())
  }

  println("outDegree")
  println(outDegree(socialNetwork, "Alice"))

  println()
  println("inDegree")
  println(inDegree(socialNetwork, "David"))
  println()

  println("isPath")
  println(isPath(socialNetwork, "Alice", "Mary"))
  println(isPath(socialNetwork, "David", "Alice"))
  println(isPath(socialNetwork, "Bob", "Charlie"))

  println("isPathTR")
  println(isPathTR(socialNetwork, "Alice", "Mary"))
  println(isPathTR(socialNetwork, "David", "Alice"))
  println(isPathTR(socialNetwork, "Bob", "Charlie"))


  println("findPathTr")
  println(findPathTr(socialNetwork, "Alice", "Mary"))
  println(findPathTr(socialNetwork, "David", "Alice"))
  println(findPathTr(socialNetwork, "Bob", "Charlie"))
}
