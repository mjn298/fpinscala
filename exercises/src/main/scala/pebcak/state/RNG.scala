package pebcak.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S, +A](run: S => (A, S))


object RNG {

  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapG[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) =
    s => {
      val (x, y) = a(s)
      (f(x), y)
    }


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    r1 => {
      val (a, rngA) = ra(r1)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }


  def mapWithFlat[S, A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(x => unit(f(x)))

  def map2WithFlat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if(i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(i)
    }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeEvn: Rand[Int] =
    map(nonNegativeInt) (i => i - i % 2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rand, nextRng) = rng.nextInt
    (if (rand < 0) -(rand + 1) else rand, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (rand, nextRng) = rng.nextInt
    val rd = rand.toDouble
    ((rd/Int.MaxValue.toDouble + 1), nextRng)
  }

  def mapDouble(ri: Rand[Int]): Rand[Double] = {
    map(ri)(_ /Int.MaxValue.toDouble + 1)
  }


  def intDouble(rng: RNG): Rand[(Int, Double)] = {
    both(int, double)
  }

  def doubleInt(rng: RNG): Rand[(Double, Int)] = {
    both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r) = double(rng)
    val (d2, r1) = double(r)
    val (d3, r2) = double(r1)
    ((d1, d2, d3), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def acc(c: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if(c == 0) (Nil, r)
      else {
        val (i, r1) = nonNegativeInt(r)
        acc(c - 1, r1, i :: l)
      }
    }
    acc(count, rng, Nil)
  }
}