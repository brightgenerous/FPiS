package exercise.exercise02

import scala.annotation.tailrec

object Exercise0202 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def isSorted_inner(head: A, tail: Array[A]): Boolean =
      if (tail.isEmpty)
        true
      else if (ordered(head, tail.head))
        isSorted_inner(tail.head, tail.tail)
      else
        false

    if (as.isEmpty) true
    else isSorted_inner(as.head, as.tail)
  }

  def main(args: Array[String]): Unit = {
    Seq(
      (Array(1, 2, 2, 3), (a: Int, b: Int) => a <= b),
      (Array(1, 2, 2, 3), (a: Int, b: Int) => a >= b),
      (Array(1, 2, 3, 2), (a: Int, b: Int) => a <= b)
    ).foreach { array_func =>
      val (array, func) = array_func
      println(isSorted(array, func))
    }
  }
}
