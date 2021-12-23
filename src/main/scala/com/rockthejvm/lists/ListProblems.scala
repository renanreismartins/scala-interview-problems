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

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]

  def rle: RList[(T, Int)]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int) = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = this

  override def map[S](f: Nothing => S): RList[S] = this

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = this

  override def filter(f: Nothing => Boolean): RList[Nothing] = this

  override def rle: RList[(Nothing, Int)] = this
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

    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatTailRec(current: RList[T], acc: RList[S]): RList[S] = {
      if (current.isEmpty) acc
      else concatTailRec(current.tail, current.head :: acc)
    }

    concatTailRec(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtTailRec(remaining: RList[T], index: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else if (index == 0) acc.reverse ++ remaining.tail
      else removeAtTailRec(remaining.tail, index - 1, remaining.head :: acc)
    }

    removeAtTailRec(this, index, RNil)
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapTailRec(remaining: RList[T], acc: RList[S]): RList[S] =
      if (remaining.isEmpty) acc
      else mapTailRec(remaining.tail, f(remaining.head) :: acc)

    mapTailRec(this, RNil).reverse
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else flatMapTailRec(remaining.tail, f(remaining.head) ++ acc)
    }

    flatMapTailRec(this, RNil).reverse
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailRec(remaining: RList[T], acc: RList[T]): RList[T] =
      if (remaining.isEmpty) acc
      else filterTailRec(remaining.tail, if (f(remaining.head)) remaining.head :: acc else acc)

    filterTailRec(this, RNil).reverse
  }

  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleTailRec(remaining: RList[T], acc: Map[T, Int]): Map[T, Int] = {
      if (remaining.isEmpty) acc
      else rleTailRec(remaining.tail, acc.updated(remaining.head, acc.getOrElse(remaining.head, 0) + 1))
    }

    rleTailRec(this.reverse, Map()).foldLeft(RNil: RList[(T, Int)])((list, tuple) => tuple :: list)
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

  println("Concat:")
  println(RNil ++ aSmallList)
  println(aSmallList ++ (4 :: 5 :: RNil))

  println("Remove at:")
  println(RNil.removeAt(2))
  println((1 :: RNil).removeAt(0))
  println((1 :: 2 :: RNil).removeAt(0))
  println((1 :: 2 :: RNil).removeAt(1))
  println((1 :: 2 :: 3 :: 4 :: 5 :: RNil).removeAt(2))
  println((1 :: 2 :: 3 :: 4 :: 5 :: RNil).removeAt(9))

  println("map:")
  println(aSmallList.map(_ * 2))

  println("flatMap:")
  println(aSmallList.flatMap(e => e :: e :: RNil))

  println("filter:")
  println(aSmallList.filter(_ > 1))

  println("rle:")
  println(aSmallList.rle)
}
