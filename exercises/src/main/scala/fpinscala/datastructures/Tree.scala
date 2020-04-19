package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(lft, rt) => b(fold(lft)(l)(b), fold(rt)(l)(b))
  }

  def foldSize[A](t: Tree[A]): Int = fold(t)(l => 1)(1 + _ + _)

  def maxViaFold(t: Tree[Int]): Int = fold(t)(l => l)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(l => 1)((d1, d2) => 1 + (d1 max d2))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(l => Leaf(f(l)): Tree[B])(Branch(_, _))
}




