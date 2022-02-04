package com.rockthejvm.interviews

import scala.annotation.tailrec

object Strings extends App {

  def countCharacters(word: String): Map[Char, Int] = {
    @tailrec
    def count(s: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (s.isEmpty) acc
      else count(s.tail, acc + (s.head -> (acc.getOrElse(s.head, 0) + 1)))
    }

    count(word, Map())
  }

  println("countCharacters")
  println(countCharacters("scala"))


  def checkAnagram(a: String, b: String): Boolean = {
    @tailrec
    def check(a: String, b: String, i: Int): Boolean = {
      if (a.isEmpty && b.isEmpty) true
      else if (a.isEmpty ^ b.isEmpty) false
      else if (a.head != b(i)) false
      else check(a.tail, b.take(i), i - 1)
    }

    check(a, b, a.length - 1)
  }

  println("checkAnagrams")
  println(checkAnagram("desserts", "stressed"))

  def hasValidParentheses(sentence: String): Boolean = {
    @tailrec
    def hasValidParenthesesTr(s: String, counter: Int): Boolean = {
      if (counter < 0) false
      else if (s.isEmpty && counter == 0) true
      else if (s.isEmpty && counter > 0) false
      else hasValidParenthesesTr(s.tail, if (s.head == '(') counter + 1 else counter - 1)
    }

    hasValidParenthesesTr(sentence, 0)
  }

  println("hasValidParentheses")
  println(hasValidParentheses(""))
  println(hasValidParentheses("()"))
  println(hasValidParentheses("(())"))
  println()
  println(hasValidParentheses("("))
  println(hasValidParentheses(")"))
  println(hasValidParentheses(")("))
  println(hasValidParentheses("())("))

  /*

  p([1,2,3])
  p([2,3]).mapAppend(1)
  [[2,3], [3,2]].mapAppend(1)
  [[1,2,3], [1,3,2]]

  p([1,3])
  p([1,3]).mapAppend(2)
  [[1,3], [3,1]].mapAppend(2)
  [[2,1,3], [2,3,1]]

  */
    def generatePermutations(l: List[Int]): List[List[Int]] = l match {
      case List() => {
        println("nil")
        Nil
      }
      case List(a) => { println("a=" + a); List(List(a))}
      case List(a, b) => {println("final a= " + a + " b=" + b); List(List(a, b), List(b, a))}

      case _ => (for (i <- l.indices.toList) yield {
        println("l(i)= " + l(i))
        println("list= " + l)
        println("remaining= " + (l.take(i) ++ l.drop(i + 1)))

        println()
        println("recur")
        println()

        generatePermutations(l.take(i) ++ l.drop(i + 1)).map(xs =>  l(i) :: xs)
      }).flatten
  }

  println("generateCombinations")
  println(generatePermutations(List(1)))
//  println(generatePermutations(List(1, 2)))
  println(generatePermutations(List()))
//  println(generatePermutations(List(1, 2, 3, 4)))


  def generatePermutations2(l: List[Int]): List[List[Int]] = {
    def go(total: Int, n: Int, acc: List[List[Int]]): List[Int] = {
      if (n == total) acc.flatten
      else go(total, n + 1, generatePermutations2(l.take(n) ++ l.drop(n + 1)).map(xs =>  l(n) :: xs))
    }

    l match {
      case Nil => Nil
      case List(a) => List(List(a))
      case List(a, b) => List(List(a, b), List(b, a))
      case _ => List(go(l.indices.length, 0, List(List())))
//      case _ => (for (i <- l.indices.toList) yield {
//        generatePermutations2(l.take(i) ++ l.drop(i + 1)).map(xs =>  l(i) :: xs)
//      }).flatten
    }
  }

  println("generateCombinations2")
  println(generatePermutations2(List()))
  println(generatePermutations2(List(1)))
  println(generatePermutations2(List(1, 2)))
  println(generatePermutations2(List(1, 2, 3)))


  def generateX(l: List[String], acc: List[String], result: List[List[String]]): List[List[String]] = {
    if (l.isEmpty) acc :: result
    else if (l.size == 1) (l.head :: acc) :: result
    else {
      for (i <- l.indices.toList) yield {
        generateX(l.take(i) ++ l.drop(i + 1), acc, result).map(xs => l(i) :: xs)
      }
    }.flatten
  }

  println("3rd gen")
  println(generateX(List("1", "2", "3"), List(), List()))




//
//  def generateValidParentheses(n: Int): List[String] = {
//
//    def generateTr(n: Int, acc: List[String]): List[String] = {
//      if (n == 0) acc
//      else if
//    }
//    ???
//  }
}
