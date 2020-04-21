import pebcak.errorhandling._
import pebcak.errorhandling.Option

import scala.util.Try

def mean(xs: Seq[Double]): Option[Double] =
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double

def parseInsuranceRateQuote(age: String,
                            numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Option.Try(age.toInt)
  val optTickets: Option[Int] = Option.Try(numberOfSpeedingTickets.toInt)
  Option.map2(optAge, optTickets)(insuranceRateQuote)
}