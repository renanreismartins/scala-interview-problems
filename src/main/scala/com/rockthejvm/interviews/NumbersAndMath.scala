package com.rockthejvm.interviews

import scala.annotation.tailrec

object NumbersAndMath extends App {

  def isPrime(n: Int): Boolean = {
    if (n == 0 || n == 1) false
    else (2 to (n - 1)).dropWhile(e => n % e != 0).isEmpty
  }
  // isPrime
  println(isPrime(0))
  println(isPrime(1))
  println(isPrime(2))
  println(isPrime(3))
  println(isPrime(4))
  println(isPrime(11))
  println(isPrime(12))
  println(isPrime(15))
  println(isPrime(2003))
  println(isPrime(2004))

  def isPrime2(n: Int): Boolean = {
    @tailrec
    def isPrimeTailRec(currentDivisor: Int): Boolean = {
      if (currentDivisor == n) true
      else (n % currentDivisor != 0) && isPrimeTailRec(currentDivisor + 1)
    }

    isPrimeTailRec(2)
  }

  def decompose(n: Int): List[Int] = {
    @tailrec
    def decomposeTailRec(currentDivisor: Int, acc: List[Int]): List[Int] = {
      if (n == 0 || n == 1) acc
      else if (currentDivisor == n) acc
      else if (n % currentDivisor == 0) decomposeTailRec(currentDivisor + 1, acc :+ currentDivisor)
      else decomposeTailRec(currentDivisor + 1, acc)
    }

    decomposeTailRec(2, List())
  }
  // decompose
  println(decompose(0))
  println(decompose(1))
  println(decompose(2))
  println(decompose(3))
  println(decompose(4))
  println(decompose(11))
  println(decompose(12))
  println(decompose(15))
  println(decompose(2003))
  println(decompose(2004))
  println(decompose(2731189))

  def factors(n: Int): List[Int] = {
    @tailrec
    def factorsTailRec(counter: Int, acc: List[Int]): List[Int] = {
      if (acc.contains(counter)) acc
      else if (n % counter == 0) factorsTailRec(counter + 1, (counter :: (n / counter) :: acc))
      else factorsTailRec(counter + 1, acc)
    }

    factorsTailRec(1, List()).sorted
  }

  println(factors(30))

  def parse(string: String): Int = {
    @tailrec
    def removeLeadingSpaces(s: String): String = {
      if (s.head == ' ') removeLeadingSpaces(s.tail)
      else s
    }

    def extractSign(s: String): (Int, String) =
      if (s.head == '-') (-1, s.tail)
      else if (s.head == '+') (1, s.tail)
      else (1, s)

    @tailrec
    def removePostfix(s: String, acc: String): String = {
      if (s.isEmpty) acc
      else if (s.head.isDigit) removePostfix(s.tail, acc + s.head)
      else acc
    }

    @tailrec
    def convert(s: String, sign: Int, acc: Int): Int = {
      if (s.isEmpty) acc
      else if (acc > Int.MaxValue / 10 || (acc == Int.MaxValue / 10 && (s.head - '0') > 7)) {
        if (sign > 0) Int.MaxValue else Int.MinValue
      } else convert(s.tail, sign, 10 * acc + (s.head - '0'))
    }

    val str = removeLeadingSpaces(string)
    val (sign, strSignalExtracted) = extractSign(str)
    val converted = convert(removePostfix(strSignalExtracted, ""), sign, 0)
    converted * sign
  }

  println("parse")
  println(parse("-3"))
  println(parse("20"))
  println(parse("-20"))
  println(parse("+100"))
  println(parse("100"))
  println(parse("-100 this is life"))
  println(parse("  -100"))
  println(parse(" +100"))
  println(parse(" +100"))
  println(parse(" +999999999999999999999"))
  println(parse(" -999999999999999999999"))

  def largestNumber(numbers: List[Int]): String = {
//    //@tailrec
//        def largest(a: String, b: String): Boolean = {
////          if (a.isEmpty) true
////          else if(b.isEmpty) false
////          else if (a.head.toInt > b.head.toInt) true
////          else if (a.head.toInt < b.head.toInt) false
////          else largest(a.tail, b.tail)
//
//        }

    numbers.map(_.toString).sortWith((a, b) => (a + b).compareTo(b + a) >= 0).mkString
  }

  println(largestNumber(List(903, 925))) // 925903
  println(largestNumber(List(1, 2, 9, 99, 3))) // 925903
}
