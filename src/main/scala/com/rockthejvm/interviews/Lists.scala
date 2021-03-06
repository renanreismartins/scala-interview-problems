package com.rockthejvm.interviews

import scala.annotation.tailrec
import scala.util.Random

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

  def duplicateEach(k: Int): RList[T]

  def splitAt(n: Int): RList[T]

  def rotate(n: Int): RList[T]

  def sample(n: Int): RList[T]

  def sorted[S >: T](ordering: Ordering[S]): RList[S]
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

  override def duplicateEach(k: Int): RList[Nothing] = this

  override def splitAt(n: Int): RList[Nothing] = this

  override def rotate(n: Int): RList[Nothing] = this

  override def sample(n: Int): RList[Nothing] = this

  override def sorted[S >: Nothing](ordering: Ordering[S]): RList[S] = this
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
      else flatMapTailRec(remaining.tail, f(remaining.head).reverse ++ acc)
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

  def rleNoMap: RList[(T, Int)] = {
    @tailrec
    def rleTailRec(remaining: RList[T], currentTuple: (T, Int), acc: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty) currentTuple :: acc
      else if (currentTuple._1 == remaining.head) rleTailRec(remaining.tail, (currentTuple._1, currentTuple._2 + 1), acc)
      else rleTailRec(remaining.tail, (remaining.head, 1), currentTuple :: acc)
    }

    rleTailRec(this.tail, (this.head, 1), RNil).reverse
  }

  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def duplicateEachTailRec(remaining: RList[T], remainingK: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else if (remainingK == 0) duplicateEachTailRec(remaining.tail, k, acc)
      else duplicateEachTailRec(remaining, remainingK - 1, remaining.head :: acc)
    }

    duplicateEachTailRec(this.reverse, k, RNil)
  }

  override def splitAt(n: Int): RList[T] = {
    @tailrec
    def splitTailRec(remaining: RList[T], remainingN: Int): RList[T] = {
      if (remainingN == 0 || remaining.isEmpty) remaining
      else splitTailRec(remaining.tail, remainingN - 1)
    }

    splitTailRec(this, n)
  }

  override def rotate(n: Int): RList[T] = {
    @tailrec
    def rotateTailRec(remaining: RList[T], remainingN: Int, acc: RList[T]): RList[T] = {
      if (remainingN == 0) remaining ++ acc.reverse
      else if (remaining.isEmpty) rotateTailRec(this, remainingN, RNil)
      else rotateTailRec(remaining.tail, remainingN - 1, remaining.head :: acc)
    }

    rotateTailRec(this, n, RNil)
  }

  override def sample(n: Int): RList[T] = {
    @tailrec
    def sampleTailRec(remainingN: Int, acc: RList[T]): RList[T] = {
      if (remainingN == 0) acc.reverse
      else sampleTailRec( remainingN - 1, this(new Random().nextInt(this.length)) :: acc)
    }

    sampleTailRec(n, RNil)
  }

  override def sorted[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def insertSorted(element: T, remaining: RList[S], acc: RList[S]): RList[S] = {
      if (acc.isEmpty || ordering.lteq(element, acc.head)) remaining.reverse ++ (element :: acc)
      else insertSorted(element, acc.head :: remaining, acc.tail)
    }

    @tailrec
    def sortedTailRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc
      else sortedTailRec(remaining.tail, insertSorted(remaining.head, RNil, acc))

    }
    sortedTailRec(this, RNil)
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
  println(aSmallList.flatMap(e => e :: e * 2 :: RNil))

  println("filter:")
  println(aSmallList.filter(_ > 1))

  println("rle:")
  println(("a" :: "b" :: "b" :: "c" :: RNil).rle)

  println("rleNoMap:")
  println(("a" :: "b" :: "b" :: "c" :: RNil).asInstanceOf[::[(String, Int)]].rleNoMap)

  println("duplicateEach:")
  println(("a" :: RNil).duplicateEach(1))
  println(("a" :: RNil).duplicateEach(2))
  println(("a" :: RNil).duplicateEach(3))
  println(("a" :: "b" :: RNil).duplicateEach(4))
  //  println(("a" :: "b" :: RNil).duplicateEach(2))

  println("splitAt:")
  println(("a" :: "b" :: "c" :: RNil).splitAt(0))
  println(("a" :: "b" :: "c" :: RNil).splitAt(1))
  println(("a" :: "b" :: "c" :: RNil).splitAt(2))
  println(("a" :: "b" :: "c" :: RNil).splitAt(5))

  println("rotate:")
  println(("a" :: RNil).rotate(2))
  println(("a" :: "b" :: "c" :: RNil).rotate(0))
  println(("a" :: "b" :: "c" :: RNil).rotate(1))
  println(("a" :: "b" :: "c" :: RNil).rotate(2))
  println(("a" :: "b" :: "c" :: RNil).rotate(3))
  println(("a" :: "b" :: "c" :: RNil).rotate(4))

  println("sample:")
  println(aSmallList.sample(3))

  println("sorted:")
  println((1 :: RNil).sorted(Ordering.fromLessThan[Int](_ < _)))
  println((2 :: 1 :: RNil).sorted(Ordering.fromLessThan[Int](_ < _)))
  println((2 :: 3 :: 4 :: 1 :: RNil).sorted(Ordering.fromLessThan[Int](_ < _)))


}
