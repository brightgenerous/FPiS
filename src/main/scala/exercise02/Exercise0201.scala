package exercise02

import scala.annotation.tailrec

object Exercise0201 {

  def fib(n: Int): Int = {

    @tailrec
    def fib_inner(a: Int, b: Int, count: Int): Int =
      if (count == 1)
        a
      else
        fib_inner(b, a + b, count - 1)

    if (n > 0)
      fib_inner(0, 1, n)
    else
      throw new Exception()
  }

  def main(args: Array[String]): Unit =
    (1 to 10).foreach { i =>
      println(s"$i => ${fib(i)}")
    }
}
