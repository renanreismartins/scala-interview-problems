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
}
