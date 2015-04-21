package chapter3

/**
 * Created by Sandeep Patil on 08/04/2015.
 */

sealed trait C3List[+A]

case object C3Nil extends C3List[Nothing] {
  override def equals(that: Any): Boolean = if(that == C3Nil) true else false
}

case class Cons[+A](h : A, tail: C3List[A]) extends C3List[A] {
  override def equals(that: Any): Boolean = that match {
    case Cons(x, C3Nil) => tail == C3Nil && h == x
    case Cons(x, xs:Cons[A]) => if (x == h) xs.equals(tail) else false
    case _ => false
  }
}

object C3List {

  def apply[A] (xs : A*) : C3List[A] = {
    if (xs.isEmpty) C3Nil
    else
      Cons(xs.head, apply(xs.tail: _*))
  }

  def tail[A](l : C3List[A]) : C3List[A] = l match {
    case C3Nil => C3Nil
    case Cons(x, xs) => xs
  }

  def replaceHead[A](l : C3List[A], newH : A) : C3List = l match {
    case C3Nil => C3Nil
    case Cons(x, xs) => Cons(newH, xs)
  }

  def drop[A](l: C3List[A], n: Int): C3List[A] = l match {
    case C3Nil => C3Nil
    case Cons(x, xs) => if (n > 0) drop(xs, n-1) else xs
  }

  def dropWhile[A](l: C3List[A], f: A => Boolean): C3List[A] = l match {
    case C3Nil => C3Nil
    case Cons(x, xs) =>
      if(f(x)) dropWhile(xs, f)
      else Cons(x, dropWhile(xs, f))
  }

  def append[A](a1: C3List[A], a2: C3List[A]): C3List[A] =  a1 match {
    case C3Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  /**
   * Implement a function, that returns a List consisting of all but the last element of a List.
   * So given List(1,2,3,4),init will return List(1,2,3).
   * Why can’t this List(1,2,3,4) init List(1,2,3) function be implemented in constant time like tail?
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l: C3List[A]): C3List[A] = l match {
    case C3Nil => C3Nil
    case Cons(x, C3Nil) => C3Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def dropWhile[A](l: C3List[A])(f: A => Boolean): C3List[A] = l match {
    case C3Nil => l
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else dropWhile(l)(f)
  }
}
