package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0324 {

  def exec = List.hasSubsequence(List(1,2,3,4), List(2,3))

  def main(args: Array[String]): Unit =
    println(exec)
}
