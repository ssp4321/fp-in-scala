/**
 * Created by Sandeep Patil on 17/03/2015.
 */

object Chapter2 {

  //@annotation.tailrec -> Doesn't work here
  def factorial(n: Int): Int = {
    if (n == 0)
      1
    else
      n * factorial(n - 1)
  }

  def factorial_tail(n: Int): Int = {
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

  def fibonacci(n: Int): Int = {
    if (n == 0)
      0
    else if (n == 1)
      1
    else
      fibonacci(n - 1) + fibonacci(n - 2)
  }


  def fibonacci_tail(n: Int): Int = {
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