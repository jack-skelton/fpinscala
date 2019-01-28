package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def countNodes[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(t1, t2) => 1 + countNodes(t1) + countNodes(t2)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(_) => _
    case Branch(t1, t2) => max(t1) max max(t2)
  }
}