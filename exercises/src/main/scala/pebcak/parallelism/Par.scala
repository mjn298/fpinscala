package pebcak.parallelism



object Par {
  type Par[A] = A

  def unit[A](a: A): Par[A] = ???
  def get[A](a: Par[A]): A = ???
  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = ???

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

}
