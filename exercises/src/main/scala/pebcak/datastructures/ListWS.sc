import pebcak.datastructures._
import pebcak.datastructures.List._

foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))

length(List(1,2,3,4,5,6,1,2,3))

foldLeft(List(1,2,3,4,5), 1.0)(_ * _)

reverse(List(1,2,3,4,5))

addOne(List(1,2,3,4,5))

dblToStr(List(1.0, 2.0, 3.3))

mapFold(List(1,2,3,4))(_ * 2)

fmFilter(List(1,2,3,4,5))(_ % 2 == 1)

flatMap(List(1,2,3,4,5))(a => List(a, a))

addItems(List(1,2,3,4,5,90), List(4,5,6,7))

take(List(1,2,3,4,5), 3)

hasSubsequence(List(1,2,3,4,5,6,7,8,9,10), List(7,8,9,10))