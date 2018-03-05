def tailRecFib(n: Int): Int = {

  def go(n: Int, first: Int, second: Int): Int =
    if (n <= 1) first
    else if (n == 2) second
    else go(n - 1, second, first + second)

  go(n, 0, 1)

}
