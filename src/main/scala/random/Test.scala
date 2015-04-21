def isSorted[A](a: Array[A], gt: (A, A) => Boolean): Boolean = {
  def loop(n: Int, checkAsc: Boolean): Boolean = {
    if (n == a.length-1)
      true
    else if (a(n) == a(n + 1))
      loop(n + 1, checkAsc)
    else if (checkAsc && gt(a(n), a(n + 1)))
      false
    else if (!checkAsc && gt(a(n + 1), a(n)))
      false
    else
      loop(n + 1, checkAsc)
  }

  def isAscSorted: Boolean = loop(0, true)

  def isDscSorted: Boolean = loop(0, false)

  if (isAscSorted)
    true
  else
    isDscSorted

}

Array(1, 2, 4, 3, 0)

isSorted(Array(1, 2, 3), (x: Int, y: Int) => x > y)
