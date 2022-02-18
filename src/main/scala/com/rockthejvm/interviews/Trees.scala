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

  val size: Int

  def collectNodes(n: Int): List[BTree[T]]

  def collectNodesTailRec(n: Int): List[BTree[T]]

  def mirror: BTree[T]

  def mirrorTailRec: BTree[T]

  def sameShapeAs[S >: T](that: BTree[S]): Boolean

  def sameShapeAsTr[S >: T](that: BTree[S]): Boolean

  def toListPreOrder: List[T]

  def toListPreOrderTr: List[T]
}

case object BEmpty extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def left: BTree[Nothing] = throw new NoSuchElementException

  override def right: BTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BTree[Nothing]] = List.empty

  override def leafCount: Int = 0

  override val size: Int = 0

  override def collectNodes(n: Int): List[BTree[Nothing]] = List.empty

  override def collectNodesTailRec(n: Int): List[BTree[Nothing]] = List.empty

  override def mirror: BTree[Nothing] = BEmpty

  override def mirrorTailRec: BTree[Nothing] = BEmpty

  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty

  override def sameShapeAsTr[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty

  override def toListPreOrder: List[Nothing] = List()

  override def toListPreOrderTr: List[Nothing] = List()
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

  override val size: Int = 1 + left.leafCount + right.leafCount

  override def collectNodes(n: Int): List[BTree[T]] = {
    if (n == 1) List(this)
    else left.collectNodes(n - 1) ::: right.collectNodes(n - 1)
  }

  override def collectNodesTailRec(n: Int): List[BTree[T]] = {
    def tr(toVisit: List[BTree[T]], i: Int): List[BTree[T]] = {
      if (toVisit.isEmpty) toVisit
      else if (i == 1) toVisit
      else {
        val levelNodes = toVisit.flatMap(n => List(n.left, n.right)).filter(n => !n.isEmpty)
        //                val levelNodes = for {
        //                  node <- toVisit
        //                  child <- List(node.left, node.right) if !child.isEmpty
        //                }
        tr(levelNodes, i - 1)
      }
    }

    tr(List(this), n)
  }

  override def mirror: BTree[T] =
    if (isEmpty) BEmpty
    else if (isLeaf) this
    else BNode(value, right.mirror, left.mirror)

  override def mirrorTailRec: BTree[T] = {
    def tr(todo: List[BTree[T]], visited: Set[BTree[T]], acc: List[BTree[T]]): BTree[T] = {
      if (todo.isEmpty) acc.head
      else if (visited.contains(todo.head)) tr(todo.tail, visited, BNode(todo.head.value, acc.head, acc.tail.head) :: acc.drop(2))
      else if (todo.head.isLeaf || todo.head.isLeaf) tr(todo.tail, visited, todo.head :: acc)
      else tr(todo.head.left :: todo.head.right :: todo, visited + todo.head, acc)
    }

    tr(List(this), Set.empty, List.empty)
  }

  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
    if ((left.isEmpty ^ that.left.isEmpty) || (right.isEmpty ^ that.right.isEmpty)) false
    else right.sameShapeAs(that.right) && left.sameShapeAs(that.left)
  }

  override def sameShapeAsTr[S >: T](that: BTree[S]): Boolean = {
    def tr(toExpand1: List[BTree[S]], toExpand2: List[BTree[S]]): Boolean = {
      if (toExpand1.isEmpty && toExpand2.isEmpty) true
      else if (toExpand1.isEmpty != toExpand2.isEmpty) false
      else {
        val t1 = toExpand1.head
        val t2 = toExpand2.head

        if (t1.isEmpty != t2.isEmpty) false
        //else if (t1.isLeaf != t2.isLeaf) false
        //else if ((t1.left.isEmpty ^ t2.left.isEmpty) || (t1.right.isEmpty ^ t2.right.isEmpty)) false
        else {
          val expandedT1 = expand(toExpand1)
          val expandedT2 = expand(toExpand2)
          tr(expandedT1, expandedT2)
        }
      }
    }

    def expand(l: List[BTree[S]]): List[BTree[S]] = l.flatMap(e => List(e.left, e.right)).filter(c => !c.isEmpty)

    tr(List(this), List(that))
  }

  override def toListPreOrder: List[T] = {
    if (isEmpty) List()
    else List(value) ::: left.toListPreOrder ::: right.toListPreOrder
  }

  override def toListPreOrderTr: List[T] = {
    def tr(toExpand: List[BTree[T]], acc: List[T]): List[T] = {
      if (toExpand.isEmpty) acc
      else {
        val expanded = for {
          current <- toExpand
          child <- List(current.left, current.right) if !child.isEmpty
        } yield child

        tr(expanded ::: toExpand.tail, acc ::: List(toExpand.head.value))
      }
    }

    tr(List(this), List())
  }
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

  println("collect nodes")
  println(BNode(3, BEmpty, BEmpty).collectNodes(1))
  println(BNode(3, BNode(1, BEmpty, BEmpty), BNode(2, BEmpty, BEmpty)).collectNodes(1))
  println(BNode(3, BNode(1, BEmpty, BEmpty), BNode(2, BEmpty, BEmpty)).collectNodes(2))

  println("collect nodes tr")
  println(BNode(3, BEmpty, BEmpty).collectNodesTailRec(1))
  println(BNode(3, BNode(1, BEmpty, BEmpty), BNode(2, BEmpty, BEmpty)).collectNodesTailRec(1))
  println(BNode(3, BNode(1, BEmpty, BEmpty), BNode(2, BEmpty, BEmpty)).collectNodesTailRec(2))
  println(BNode(3, BNode(1, BNode(4, BEmpty, BEmpty), BEmpty), BNode(2, BEmpty, BEmpty)).collectNodesTailRec(3))
  println(BNode(1, BNode(2, BNode(3, BEmpty, BEmpty), BNode(4, BEmpty, BEmpty)), BNode(5, BNode(6, BEmpty, BEmpty), BNode(7, BEmpty, BEmpty))).collectNodesTailRec(3))

  println("mirror")
  println(BNode(3, BEmpty, BEmpty).mirror)
  println(BNode(3, BNode(1, BEmpty, BEmpty), BNode(2, BEmpty, BEmpty)).mirror)
  println(BNode(3, BNode(1, BNode(4, BEmpty, BEmpty), BEmpty), BNode(2, BEmpty, BEmpty)).mirror)
  println(BNode(1, BNode(2, BNode(3, BEmpty, BEmpty), BNode(4, BEmpty, BEmpty)), BNode(5, BNode(6, BEmpty, BEmpty), BNode(7, BEmpty, BEmpty))).mirror)

  println("mirror TR")
  //println(BNode(3, BEmpty, BEmpty).mirrorTailRec)
  println(BNode(1, BNode(2, BEmpty, BEmpty), BNode(6, BEmpty, BEmpty)).mirrorTailRec)

  println("same shape")
  println(BNode(1, BNode(2, BEmpty, BEmpty), BNode(6, BEmpty, BEmpty)).sameShapeAs(BNode(1, BNode(0, BEmpty, BEmpty), BNode(6, BEmpty, BEmpty))))
  println(BNode(1, BNode(2, BEmpty, BEmpty), BNode(6, BEmpty, BNode(7, BEmpty, BEmpty))).sameShapeAs(BNode(1, BNode(0, BEmpty, BEmpty), BNode(6, BEmpty, BEmpty))))
  println(BNode(1, BNode(2, BEmpty, BEmpty), BNode(6, BEmpty, BNode(7, BEmpty, BEmpty))).sameShapeAs(BNode(1, BNode(0, BEmpty, BEmpty), BNode(6, BNode(7, BEmpty, BEmpty), BEmpty))))

  println("same shape TR")
  println(BEmpty.sameShapeAsTr(BEmpty))
  println(BEmpty.sameShapeAsTr(BNode(3, BEmpty, BEmpty)))
  println(BNode(3, BEmpty, BEmpty).sameShapeAsTr(BEmpty))
  println(BNode(3, BEmpty, BEmpty).sameShapeAsTr(BNode(3, BEmpty, BEmpty)))
  println(BNode(3, BNode(1, BNode(2, BEmpty, BEmpty), BEmpty), BEmpty).sameShapeAsTr(BNode(3, BNode(1, BEmpty, BNode(2, BEmpty, BEmpty)), BEmpty)))
  println(BNode(1, BNode(2, BEmpty, BEmpty), BNode(6, BEmpty, BEmpty)).sameShapeAsTr(BNode(1, BNode(0, BEmpty, BEmpty), BNode(6, BEmpty, BEmpty))))
  println(BNode(1, BNode(2, BEmpty, BEmpty), BNode(6, BEmpty, BNode(7, BEmpty, BEmpty))).sameShapeAsTr(BNode(1, BNode(0, BEmpty, BEmpty), BNode(6, BEmpty, BEmpty))))
  println(BNode(1, BNode(2, BNode(3, BEmpty, BEmpty), BNode(4, BEmpty, BNode(5, BEmpty, BEmpty))), BNode(6, BNode(7, BEmpty, BEmpty), BNode(8, BEmpty, BEmpty)))
    .sameShapeAsTr(BNode(8, BNode(9, BNode(1, BEmpty, BEmpty), BNode(3, BEmpty, BNode(4, BEmpty, BEmpty))), BNode(2, BNode(2, BEmpty, BEmpty), BNode(7, BEmpty, BEmpty)))))

  println("pre order non tr")
  println(BEmpty.toListPreOrder)
  println(BNode(3, BEmpty, BEmpty).toListPreOrder)
  println(BNode(1, BNode(2, BNode(3, BEmpty, BEmpty), BEmpty), BNode(4, BEmpty, BEmpty)).toListPreOrder)

  println("pre order tr")
  println(BEmpty.toListPreOrderTr)
  println(BNode(3, BEmpty, BEmpty).toListPreOrderTr)
  println(BNode(1, BNode(2, BNode(3, BEmpty, BEmpty), BEmpty), BNode(4, BEmpty, BEmpty)).toListPreOrderTr)
}