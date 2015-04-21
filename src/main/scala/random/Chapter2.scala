/**
 * Created by Sandeep Patil on 17/03/2015.
 */

object Chapter2 {
  object Factorial {
    //@annotation.tailrec -> Doesn't work here
    def factorial(n: Int): Int = {
      if (n == 0)
        1
      else
        n * factorial(n - 1)
    }

    def factorial_tail(n: Int): Int = {
      @annotation.tailrec
      def fac(n : Int, soFar : Int): Int = {
        if (n == 0)
          soFar
        else
          fac(n-1, soFar*n)
      }
      fac(n, 1)
    }

    def factorial_tail_NoSoGood(n: Int): Int = {
      @annotation.tailrec
      def fac(soFar: Int, i: Int, till: Int): Int = {
        if (i == till)
          soFar * i
        else
          fac(soFar * i, i + 1, till)
      }
      if (n == 0)
        1
      else
        fac(1, 1, n)
    }
  }

  object Fibonacci {
    def fibonacci(n: Int): Int = {
      if (n == 0 )
        0
      else if (n == 1)
        1
      else
        fibonacci(n - 1) + fibonacci(n - 2)
    }

    def fibonacci_tail(n : Int) : Int = {
      @annotation.tailrec
      def go(n: Int, sumSoFar: Int, sumBeforeThat: Int) : Int = {
        if (n == 0)
          sumSoFar
        else
          go(n-1, sumBeforeThat+sumSoFar, sumSoFar)
      }

      go(n, 0, 1)
    }


    def fibonacci_tail_notSoGood(n: Int): Int = {
      @annotation.tailrec
      def fib(i: Int, n: Int, sumSoFar: Int, sumBeforeThat: Int): Int = {
        if (i == n)
          sumSoFar + sumBeforeThat
        else
          fib(i + 1, n, sumSoFar + sumBeforeThat, sumSoFar)
      }

      if (n == 0)
        0
      else if (n == 1)
        1
      else
        fib(2, n, 1, 0)
    }
  }



  object Misc {
    /**
     * Works!
     *
     * @param a
     * @param gt
     * @tparam A
     * @return
     */
    def isSorted[A](a : Array[A], gt : (A,A) => Boolean) : Boolean = {
      def loop(n : Int, checkAsc : Boolean) : Boolean = {
        if (n == a.length-1)
          true
        else
          if (a(n) == a(n+1))
            loop(n+1, checkAsc)
          else if (checkAsc && gt(a(n), a(n+1)))
            false
          else if (!checkAsc && gt(a(n+1), a(n)))
            false
          else
            loop(n+1, checkAsc)
      }

      def isAscSorted: Boolean = loop(0, true)

      def isDscSorted: Boolean = loop(0, false)

      if (isAscSorted)
        true
      else
        isDscSorted

    }
  }

  def partial1[A,B,C] (a : A, f: (A,B) => C) : B => C = {
    (b : B) => f(a, b)
  }

  def curry[A,B,C] (f : (A,B) => C) : A => (B => C) = {
    (a : A) => (b: B) => f(a, b)
  }

  //A => B => C is same as A => (B => C)
  def unCurry[A,B,C](f : A => B => C) : (A,B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * Compose two functions - same as "f compose g" or "f andThen g" -- actual implementation first triggers g and feeds its input to f
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def compose[A,B,C] (f : B => C, g : A => B) : A => C = {
    (a : A) => f(g(a))
  }
}