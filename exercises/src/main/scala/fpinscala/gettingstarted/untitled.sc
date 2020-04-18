def partial1[A, B, C](a: A, f: (A,B) => C): B => C = {
  (b: B) => f(a, b)
}

partial1(5, (a: Int, b: Int) => a :: List(b))(3)

def curry[A,B,C](f: (A,B) => C): A => B => C = {
  a => b => f(a, b)
}

curry((a: Int, b: Int) => s"we have $a and $b")(20)(5)


def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}