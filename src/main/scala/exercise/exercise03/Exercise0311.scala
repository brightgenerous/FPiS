package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0311 {

  def exec1 = List.sumL(List(1,2,3,4))
  def exec2 = List.productL(List(1,2,3,4))
  def exec3 = List.lengthL(List(1,2,3,4))

  def main(args: Array[String]): Unit = {
    println(exec1)
    println(exec2)
    println(exec3)
  }
}
