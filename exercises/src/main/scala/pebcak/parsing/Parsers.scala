package pebcak.parsing

trait Parsers[ParseError, Parser[+_]] {
  def run[A](P: Parser[A])(input: String): Either[ParseError,A]
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

}
