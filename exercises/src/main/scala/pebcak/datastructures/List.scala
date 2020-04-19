package pebcak.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Cons(_, ys) => ys
    case Nil => Nil
  }

  def setHead[A](xs: List[A], x: A): List[A] = xs match {
    case Cons(_, ys) => Cons(x, ys)
    case Nil => Cons(x, Nil)
  }

  @tailrec
  def drop[A](xs: List[A], n: Int): List[A] = {
    if(n == 0) xs
    else xs match {
      case Nil => Nil
      case Cons(_, ys) => drop(ys, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Cons(y, ys) if f(y) => dropWhile(ys, f)
    case _ => xs
  }

  def append[A](xs1: List[A], xs2: List[A]): List[A] = xs1 match {
    case Nil => xs2
    case Cons(y, ys) => Cons(y, append(ys, xs2))
  }
}