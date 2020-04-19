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
  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Cons(y, ys) if f(y) => dropWhile(ys)(f)
    case _ => xs
  }

  def append[A](xs1: List[A], xs2: List[A]): List[A] = xs1 match {
    case Nil => xs2
    case Cons(y, ys) => Cons(y, append(ys, xs2))
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(xs: List[Int]): Int = foldRight(xs, 0)(_ + _)
  def mult2(xs: List[Double]): Double = foldRight(xs, 1.0)(_ * _)

  def length[A](xs: List[A]): Int = foldRight(xs, 0)((_,b) => 1 + b)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }

  def leftSum(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)
  def leftProd(xs: List[Int]): Double = foldLeft(xs, 1.0)(_ * _)
  def leftLen[A](xs: List[A]): Int = foldLeft(xs, 0)((_, acc) => 1 + acc)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(acc, h))


  def appendFold[A](xs: List[A], zs: List[A]): List[A] = foldLeft(xs, zs)((a, b) => Cons(a, b))

  def addOne(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(y, ys) => Cons(y + 1, addOne(ys))
  }

  def dblToStr(xs: List[Double]): List[String] = xs match {
    case Nil => Nil
    case Cons(y, ys) => Cons(y.toString, dblToStr(ys))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(y, ys) => Cons(f(y), map(ys)(f))
  }

  def mapFold[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(y, ys) if f(y) => Cons(y, filter(ys)(f))
    case Cons(_, ys) => filter(ys)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(y, ys) => appendFold(f(y), flatMap(ys)(f))
  }

  def fmFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def addItems(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addItems(xs, ys))
    case (Cons(x, xs), _) => Cons(x, xs)
    case (_, Cons(y, ys)) => Cons(y, ys)
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def take[A](as: List[A], n: Int): List[A] = {
    if(n <= 0) Nil
    else as match {
      case Nil => Nil
      case Cons(y, ys) => Cons(y, take(ys, n - 1))
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(y, ys) =>
      if(take(Cons(y, ys), length(sub)) == sub) true
      else hasSubsequence(ys, sub)
  }

}