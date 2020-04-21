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
}