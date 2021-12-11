package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int) = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"

  }

  override def apply(index: Int): T = {
    @tailrec
    def applyTailRec(remaining: RList[T], remainingIndex: Int): T = {
      if (remainingIndex == 0) remaining.head
      else applyTailRec(remaining.tail, remainingIndex - 1)
    }

    applyTailRec(this, index)
  }

  override def length: Int = {
    @tailrec
    def lengthTailRec(remaining: RList[T], acc: Int): Int = {
      if (remaining.isEmpty) acc
      else lengthTailRec(remaining.tail, acc + 1)
    }

    lengthTailRec(this, 0);
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseTailRec(remaining: RList[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else reverseTailRec(remaining.tail, remaining.head :: acc)
    }

    reverseTailRec(this,  RNil)
  }

}

object ListProblems extends App {

  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  println(aSmallList)
  println(aSmallList(0))
  println(aSmallList(1))
  //println(aSmallList(5))


  println("Reverse:")
  println(RNil.reverse)
  println((1 :: RNil).reverse)
  println(aSmallList.reverse)


}
