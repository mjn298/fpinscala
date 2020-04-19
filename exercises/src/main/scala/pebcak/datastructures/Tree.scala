package pebcak.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[_](t: Tree[_]): Int = t match {
    case l: Leaf[_] => 1
    case b: Branch[_] => size(b.left) + size(b.right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case l: Leaf[Int] => l.value
    case b: Branch[Int] => maximum(b.left) max maximum(b.right)
  }
}

