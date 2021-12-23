package com.rockthejvm.advanced

object PatternMatching extends App{

  class Person(val name: String, val age: Int)
  object Person {
    def unapply(age: Int): Option[String] = Some(if (age > 18) "major" else "minor")
    def unapply(name: String): Option[(String, String)] = Some(("whatever1", "whatever2"))
  }

  val bob = new Person("bob", 30)

  val legalStatus: String = 30 match { // if there is a unnaply method in Person obj that that receives an int and returns a opt string
    case Person(status) => status
  }
  println(s"bob is $legalStatus")


  val whateverStatus: (String, String) = "if there is a unnaply method that receives a string and return opt tuple of strings..." match {
    case Person(whateverStatus) => whateverStatus
  }
  println(s"bob whateverStatus is $whateverStatus")

  val decomposedWhateverStatus: String = "if there is a unnaply method that receives a string and return opt tuple of strings..." match {
    case Person((a, b)) => s"$a and $b"
  }
  println(s"bob whateverStatus is $decomposedWhateverStatus")
}
