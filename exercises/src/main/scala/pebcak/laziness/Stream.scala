package pebcak.laziness

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toListRec: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRec
  }

  def toListTailRec: List[A] = {
    @tailrec
    def go(as: Stream[A], acc: List[A]): List[A] = as match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List.empty[A]).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 0 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => Stream.empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsFold(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if(p(h)) Stream.cons(h, t) else Stream.empty[A])

  def headOptionFold: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[A, B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))
  def filter[A](f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if(f(h)) Stream.cons(h, t) else t)
  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((h, t) => Stream.cons(h, t))
  }
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((h, t) => f(h) append t)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Stream.empty[A]
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

//  def fromUnfold(n: Int): Stream[Int] = cons(n, )

}
