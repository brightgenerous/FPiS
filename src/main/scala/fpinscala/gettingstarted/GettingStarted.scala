package fpinscala.gettingstarted

import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) - n
    else n

  def factorial(n: Int): Int = {

    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {

    @tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(findFirst(Array(1, 2, 3, 4), (a: Int) => a % 2 == 0))
  }
}
