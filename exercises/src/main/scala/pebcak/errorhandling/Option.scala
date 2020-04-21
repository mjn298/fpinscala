package pebcak.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(b) => b
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) if f(a) => Some(a)
  }

  def isEmpty[A](a: Option[A]): Boolean = a match {
    case Some(_) => false
    case _ => true
  }

  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case a: Exception => None
    }
  }
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def traverseFold[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

  def seq1[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  def map2Fold[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)


}