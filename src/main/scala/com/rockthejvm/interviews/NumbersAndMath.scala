package com.rockthejvm.interviews

object NumbersAndMath extends App {

  def isPrime(n: Int): Boolean = {
    if (n == 0 || n == 1) false
    else (2 to (n - 1)).dropWhile(e => n % e != 0).isEmpty
  }

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
  println(isPrime(1000000000))
}
