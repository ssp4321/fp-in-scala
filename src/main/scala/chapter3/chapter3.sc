import chapter3.{Cons, C3Nil, C3List}

def dropWhile[A](l: C3List[A])(f: A => Boolean): C3List[A] = l match {
  case C3Nil => l
  case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else dropWhile(l)(f)
}

dropWhile(Cons(1, Cons(2, Cons(3, C3Nil))))(_ < 2)