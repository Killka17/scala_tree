package tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {

  "Tree" should "add elements correctly" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5).add(7)
    tree.size shouldEqual 7
  }

  it should "delete elements correctly" in {
    val tree = Node(1).add(1).add(1).add(1).add(1).add(1).add(1)
    tree.size shouldEqual 1
  }
  it should "skip same elements" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5).add(7)
    tree.size shouldEqual 7
  }

  it should "delete node with only one child" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5)
    val newTree = tree.delete(6)
    newTree.size shouldEqual 5
    newTree.max(_.breadthFirstSearch(_, _)) shouldEqual 5
  }

  "foldLeft" should "accumulate sum of all nodes" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5).add(7)
    val sum = tree.foldLeft(0)(_ + _)
    sum shouldEqual 28
  }

  "breadthFirstSearch" should "accumulate nodes in breadth-first order" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5).add(7)
    val elements = tree.breadthFirstSearch[List[Int]]((acc, v) => acc :+ v, List())
    elements shouldEqual List(4, 2, 6, 1, 3, 5, 7)
  }

  "depthFirstSearch" should "accumulate nodes in inorder (depth-first) order" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5).add(7)
    val elements = tree.depthFirstSearch[List[Int]]((acc, v) => acc :+ v, List())
    elements shouldEqual List(4, 2, 1, 3, 6, 5, 7)
  }

  "max" should "return maximum value using breadthFirstSearch" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5).add(7)
    tree.max(_.breadthFirstSearch(_, _)) shouldEqual 7
  }

  "min" should "return minimum value using depthFirstSearch" in {
    val tree = Node(4).add(2).add(6).add(1).add(3).add(5).add(7)
    tree.min(_.depthFirstSearch(_, _)) shouldEqual 1
  }
}
