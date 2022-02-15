package com.rockthejvm.interviews

import java.util.NoSuchElementException

sealed abstract class BTree[+T] {
  def value: T

  def left: BTree[T]

  def right: BTree[T]

  def isEmpty: Boolean

  def isLeaf: Boolean

  def collectLeaves: List[BTree[T]]

  def leafCount: Int

  def size: Int
}

case object BEmpty extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def left: BTree[Nothing] = throw new NoSuchElementException

  override def right: BTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BTree[Nothing]] = List.empty

  override def leafCount: Int = 0

  override def size: Int = 0
}

case class BNode[+T](value: T, left: BTree[T], right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false

  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
      if (isLeaf) List(this)
      else left.collectLeaves ::: right.collectLeaves

    // collect(3, ())
    //collect(1, ()) ::: collect(2, ())
    //(collect(4, ()) ::: collect(5, ())) ::: collect(2, ()))
  }

  override def leafCount: Int = collectLeaves.size

  override def size: Int = 1 + left.leafCount + right.leafCount
}

object BTreeProblems extends App {
  println("leaf count")
  println(BNode(3, BEmpty, BEmpty).leafCount)
  println(BNode(3, BNode(1, BEmpty, BEmpty), BEmpty).leafCount)
  println(BNode(3, BNode(1, BEmpty, BEmpty), BNode(2, BEmpty, BEmpty)).leafCount)
  println(BNode(3, BNode(2, BNode(1, BEmpty, BEmpty), BEmpty), BEmpty).leafCount)

  println()
  println()
  println()

  println("collect leaves")
  println(BNode(3, BEmpty, BEmpty).collectLeaves)
  println(BNode(3, BNode(1, BEmpty, BEmpty), BNode(2, BEmpty, BEmpty)).collectLeaves)
  println(BNode(3, BNode(1, BNode(4, BEmpty, BEmpty), BNode(5, BEmpty, BEmpty)), BNode(2, BEmpty, BEmpty)).collectLeaves)
}