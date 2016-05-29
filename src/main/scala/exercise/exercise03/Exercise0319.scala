package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0319 {

  def exec = List.filter(List(1,2,3,4,5))(_ % 2 == 0)

  def main(args: Array[String]): Unit =
    println(exec)
}
