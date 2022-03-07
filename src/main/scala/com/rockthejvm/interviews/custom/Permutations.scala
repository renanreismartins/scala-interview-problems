package com.rockthejvm.interviews.custom

object Permutations extends App {

  /**
    * Combine all different permutations of 2 elements of the list, without repeating the element
    * Example: List("A", "B", "C") = AB, AC, BC
    */

  def permutationsOfTwoElements(l: List[String]): Set[String] = {
    def x(e: String, l: List[String]): List[String] = {
      l.map(le => s"$e$le")
    }

    x(l.head, l.tail)

    ???
  }

  // println(permutationsOfTwoElements(List("A", "B", "C")))


  def combs(l: List[String]) = {
    val r = scala.collection.mutable.ListBuffer.empty[String]
    for (i <- l.indices) {
      println(i)
      for (j <- (i + 1).until(l.size)) {
        for (h <- (j + 1).until(l.size)) {
          r += l(i) + l(j) + l(h)
        }
      }
    }
    r
  }

  println(combs(List("A", "B", "C", "D" )))

  def combs2(l: List[String]): List[String] = {
    def aux(l2: List[String], acc2: List[String], n: Int): List[String] = {
      if (l2.isEmpty) acc2
      else aux(l2.tail, acc2 ::: l2.tail.map(e => l2.head + e), n - 1)
    }

    aux(l, List(), 2)
  }
  println(combs2(List("A", "B", "C", "D", "E")))

  val combinationsLetters = (for {
    (i, idi) <- List("A", "B", "C", "D", "E").zipWithIndex
    (j, idj) <- List("A", "B", "C", "D", "E").zipWithIndex.take(idi)
    (k, _) <- List("A", "B", "C", "D", "E").zipWithIndex.take(idj)
  } yield (i, j, k))

  val combinationsNumbers = (for {
    (i, idi) <- List("1", "2", "3").zipWithIndex
    (j, _) <- List("1", "2", "3").zipWithIndex.take(idi)
  } yield (i, j))

  val combinationsAll = (for {
    i <- combinationsLetters
    j <- combinationsNumbers
  } yield (i, j))

  println()
  println(combinationsLetters)
  println(combinationsNumbers)
  println(combinationsAll)
}
