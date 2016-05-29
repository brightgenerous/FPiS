package exercise.exercise02

import scala.annotation.tailrec

object Exercise0201 {

  def fib(n: Int): Int = {

    @tailrec
    def fib_inner(a: Int, b: Int, cnt: Int): Int =
      if (cnt == 1)
        a
      else
        fib_inner(b, a + b, cnt - 1)

    if (n > 0)
      fib_inner(0, 1, n)
    else
      throw new Exception("n must be greater than 0.")
  }

  def main(args: Array[String]): Unit =
    (1 to 10).foreach { i =>
      println(s"$i => ${fib(i)}")
    }
}
