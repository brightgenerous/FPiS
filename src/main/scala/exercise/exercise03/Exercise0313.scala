package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0313 {

  def exec1 = List.foldLeftR(0, List(1,2,3))(_ + _)
  def exec2 = List.foldRightL(List(1,2,3), 0)(_ + _)

  def main(args: Array[String]): Unit = {
    println(exec1)
    println(exec2)
  }
}
