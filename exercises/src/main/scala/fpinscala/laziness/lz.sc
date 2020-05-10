def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
  if (cond) onTrue else onFalse
}

val a = 12

if2(a < 22,
  () => println("a"),
  () => println("b")
)

def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j + j else 0
}

maybeTwice2(true, 12)