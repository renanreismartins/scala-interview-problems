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
    case List(a) => {
      println("a=" + a);
      List(List(a))
    }
    case List(a, b) => {
      println("final a= " + a + " b=" + b);
      List(List(a, b), List(b, a))
    }

    case _ => (for (i <- l.indices.toList) yield {
      println("l(i)= " + l(i))
      println("list= " + l)
      println("remaining= " + (l.take(i) ++ l.drop(i + 1)))

      println()
      println("recur")
      println()

      generatePermutations(l.take(i) ++ l.drop(i + 1)).map(xs => l(i) :: xs)
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
      else go(total, n + 1, generatePermutations2(l.take(n) ++ l.drop(n + 1)).map(xs => l(n) :: xs))
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

  def generateValidParentheses(n: Int): List[String] = {

    def gen(n: Int, combinations: List[String]): List[String] = {
      if (n == 1) combinations
      else {
        val newCombinations = combinations.map(c => {
          for {
            index <- 0 until c.length
          } yield {
            val (before, after) = c.splitAt(index)
            s"$before()$after"
          }
        }).flatten

        gen(n - 1, newCombinations)
      }
    }

    gen(n, List("()"))
  }

  println("generateValidParentheses")
  //    println(generateValidParentheses(1))
  println(generateValidParentheses(2))
  println(generateValidParentheses(3))


  def ransomNote(note: String, magazine: String): Boolean = {
    @tailrec
    def tr(remainingNote: String, remainingMagazine: String): Boolean = {
      if (remainingNote.isEmpty) true
      else if (remainingMagazine.isEmpty) false
      else {
        val idx = indexOf(remainingNote.head, magazine, 0)
        if (idx == -1) false
        else tr(remainingNote.tail, remainingMagazine.take(idx) + remainingMagazine.drop(idx + 1))
      }
    }

    @tailrec
    def indexOf(c: Char, sentence: String, count: Int): Int = {
      if (sentence.isEmpty) -1
      else if (c == sentence.head) count
      else indexOf(c, sentence.tail, count + 1)
    }

    tr(note, magazine)
  }

  println("ransomNote")
  println(ransomNote("renan", "renan"))
  println(ransomNote("renan", "rrenan"))
  println(ransomNote("renan", "nrena"))
  println(ransomNote("renan", "r"))
  println(ransomNote("renan", "123"))


  def compareVersions(v1: String, v2: String): Int = {
    @tailrec
    def tr(sV1: List[String], sV2: List[String]): Int = {
      if (sV1.isEmpty && sV2.isEmpty) 0
      else if (sV1.isEmpty) if (sV2.exists(_.toInt != 0)) -1 else 0
      else if (sV2.isEmpty) if (sV1.exists(_.toInt != 0)) 1 else 0
      else if (sV2.isEmpty) 1
      else if (sV1.head.toInt.compareTo(sV2.head.toInt) == 0) tr(sV1.tail, sV2.tail)
      else sV1.head.toInt.compareTo(sV2.head.toInt)
    }

    val splitV1 = v1.split("\\.").toList
    val splitV2 = v2.split("\\.").toList

    tr(splitV1, splitV2)
  }

  println("compare versions")
  println(compareVersions("0.9", "0.9"))
  println(compareVersions("0.9", "1.0"))
  println(compareVersions("0.9", "0.9.1"))
  println(compareVersions("0.9", "0.9.0"))
  println(compareVersions("1.0.3.4", "1.1.0"))
  println(compareVersions("1.0", "1.0.0"))
  println(compareVersions("1.0.0", "1.0"))
  println(compareVersions("2.0", "1.0"))
  println(compareVersions("1.0.1", "1.0"))


  // to it again with all possible strings to practice
  def reorganizeString(s: String): String = {
    def reorg(charC: Map[Char, Int], acc: String): String = {
      if (charC.values.forall(v => v == 0)) acc
      else {
        val lastChar = acc.takeRight(1).charAt(0)
        val (mKey, mValue) = charC.removed(lastChar).maxBy({
          case (_, value) => value
        })
        reorg(charC.updated(mKey, mValue - 1), acc + mKey)
      }
    }

    val charCount = s.groupBy(c => c)
      .view
      .mapValues(_.length)
      .toMap

    val charOccursTooMuch = charCount
      .values
      .exists(l => l > (s.length + 1) / 2)

    if (charOccursTooMuch) ""
    else {
      val (mKey, mValue) = charCount.maxBy({
        case (_, value) => value
      })
      reorg(charCount.updated(mKey, mValue - 1), mKey.toString)
    }
  }

  println("reog string")
  println(reorganizeString("aaabc"))


  // could have dont split on empty spaces, filter, reverse
  def reverseWords(s: String): String = {
    def reverseTr(remaining: String, acc: String): String = {
      val trimmed = remaining.trim
      if (trimmed.isEmpty) acc.trim
      else {
        val word = trimmed.takeWhile(c => !c.isSpaceChar)
        reverseTr(trimmed.drop(word.length), s"$word $acc")
      }
    }

    reverseTr(s, "")
  }

  println("reverse words")
  println(reverseWords("   renan    loves  scala   "))





  def justify(text: String, maxLineSize: Int): String = {
    def packLines(words: List[String], currentLine: List[String], currentLineSize: Int, acc: List[List[String]]): List[List[String]] = {
      if (words.isEmpty && currentLine.isEmpty) acc
      else if (words.isEmpty) acc :+ currentLine
      else if (currentLineSize + words.head.size > maxLineSize) packLines(words, List(), 0,  acc :+ currentLine)
      else if (currentLineSize + words.head.size == maxLineSize) packLines(words.tail, List(), 0, acc :+ (currentLine :+ words.head))
      else packLines(words.tail, currentLine :+ words.head, currentLineSize + words.head.size + 1, acc)
    }

    def makeSpace(n: Int): String = (1 to n).map(_ => " ").mkString

    def justifyLine(line: List[String]): String = {
      if (line.size == 1) line.head
      else {
        val maxSpaces = maxLineSize - line.map(_.length).sum
        val spacesPerInterval = makeSpace(maxSpaces / (line.size - 1))
        val firstSpaces = makeSpace(maxSpaces % (line.size - 1))

        val first = s"${line.head}$spacesPerInterval$firstSpaces"
        val middle = line.tail.dropRight(1).map(l => s"$l$spacesPerInterval").mkString
        val last = line.takeRight(1).mkString

        s"$first$middle$last"
      }
    }

    val packed: Seq[List[String]] = packLines(text.split(" ").toList, List(), 0, List())
    packed.map(justifyLine).mkString("\n")
  }
////acc +: currentLine
//  //acc : currentLine
  println("justify")
  println(justify("a", 5))
  println(justify("a b", 5))
  println(justify("a b c", 5))
  println(justify("a b c d", 5))
  println(justify("a bb ccc", 5))
  println(justify("12345", 5))
  println(justify("a bb cb", 5))
  println(justify("re na loves scala", 6))

}
