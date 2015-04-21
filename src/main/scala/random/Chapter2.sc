
def curry[A,B,C] (f : (A,B) => C) : A => (B => C) = {
  (a : A) => (b: B) => f(a, b)
}

def unCurry[A,B,C](f : A => B => C) : (A,B) => C = {
  (a: A, b: B) => f(a)(b)
}
