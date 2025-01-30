package tree

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed trait Tree {
  def add(value: Int): Tree

  def delete(value: Int): Tree

  def foldLeft[A](acc: A)(f: (A, Int) => A): A

  def breadthFirstSearch[A](f: (A, Int) => A, acc: A): A

  def depthFirstSearch[A](f: (A, Int) => A, acc: A): A

  def max(searchMethod: (Tree, (Int, Int) => Int, Int) => Int): Int

  def min(searchMethod: (Tree, (Int, Int) => Int, Int) => Int): Int

  def size: Int

  def print(): Unit

  def minValue: Int
}

final case class Node private (value: Int, left: Tree = Empty, right: Tree = Empty) extends Tree {

  self =>

  override def add(newValue: Int): Tree = {
    @tailrec
    def addTailRec(current: Tree, path: List[Node]): Option[Tree] = current match {
      case Empty =>
        Some(path.foldLeft(Node(newValue): Tree) { case (subtree, Node(v, l, r)) =>
          if (newValue < v) Node(v, subtree, r) else Node(v, l, subtree)
        })
      case Node(v, l, r) =>
        newValue match {
          case i if i < v => addTailRec(l, Node(v, l, r) :: path)
          case i if i > v => addTailRec(r, Node(v, l, r) :: path)
          case _          => None
        }
    }
    addTailRec(self, Nil).getOrElse(self)
  }

  override def delete(deleteValue: Int): Tree = {
    deleteValue match {
      case v if v < value => Node(value, left.delete(v), right)
      case v if v > value => Node(value, left, right.delete(v))
      case _ =>
        (left, right) match {
          case (Empty, Empty) => Empty
          case (Empty, _)     => right
          case (_, Empty)     => left
          case _ =>
            val successor = right.minValue
            Node(successor, left, right.delete(successor))
        }
    }
  }
  @tailrec
  override def minValue: Int = left match {
    case Empty      => value
    case node: Node => node.minValue
  }

  override def foldLeft[A](acc: A)(f: (A, Int) => A): A = {
    @tailrec
    def loop(queue: List[Tree], acc: A): A = queue match {
      case Nil           => acc
      case Empty :: rest => loop(rest, acc)
      case Node(v, l, r) :: rest =>
        loop(l :: r :: rest, f(acc, v))
    }

    loop(List(self), acc)
  }

  override def breadthFirstSearch[A](f: (A, Int) => A, acc: A): A = {
    @tailrec
    def bfs(queue: Queue[Tree], acc: A): A = queue.dequeueOption match {
      case Some((Empty, rest))         => bfs(rest, acc)
      case Some((Node(v, l, r), rest)) => bfs(rest.enqueue(l).enqueue(r), f(acc, v))
      case None                        => acc
    }

    bfs(Queue(self), acc)
  }

  override def depthFirstSearch[A](f: (A, Int) => A, acc: A): A = foldLeft(acc)(f)

  override def max(searchMethod: (Tree, (Int, Int) => Int, Int) => Int): Int = {
    searchMethod(self, math.max, Int.MinValue)
  }

  override def min(searchMethod: (Tree, (Int, Int) => Int, Int) => Int): Int = {
    searchMethod(self, math.min, Int.MaxValue)
  }

  override def size: Int = foldLeft(0)((acc, _) => acc + 1)

  override def print(): Unit = {
    @tailrec
    def levelOrderPrint(queue: List[Tree]): Unit = queue match {
      case Nil => ()
      case _ =>
        val nextQueue = queue.flatMap {
          case Empty         => List(Empty, Empty)
          case Node(_, l, r) => List(l, r)
        }
        println(queue.collect { case Node(v, _, _) => v }.mkString(" "))
        levelOrderPrint(nextQueue)
    }

    levelOrderPrint(List(self))
  }
}

case object Empty extends Tree {

  self =>

  override def add(value: Int): Tree = Node(value)

  override def delete(value: Int): Tree = self

  override def foldLeft[A](acc: A)(f: (A, Int) => A): A = acc

  override def breadthFirstSearch[A](f: (A, Int) => A, acc: A): A = acc

  override def depthFirstSearch[A](f: (A, Int) => A, acc: A): A = acc

  override def max(searchMethod: (Tree, (Int, Int) => Int, Int) => Int): Int = Int.MinValue

  override def min(searchMethod: (Tree, (Int, Int) => Int, Int) => Int): Int = Int.MaxValue

  override def size: Int = 0

  override def print(): Unit = println()

  override def minValue: Int = 0
}
