package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0324 {

  def exec1 = List.hasSubsequence(List(1,2,3,4), List(2,3))
  def exec2 = List.hasSubsequence(List(1,2,3,4), List())
  def exec3 = List.hasSubsequence(List(1,2,3,4), List(2,4))

  def main(args: Array[String]): Unit = {
    println(exec1)
    println(exec2)
    println(exec3)
  }
}
