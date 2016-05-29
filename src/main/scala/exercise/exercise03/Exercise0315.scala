package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0315 {

  def exec = List.flatten(List(List(1,2,3), List(4,5,6), List(7,8,9)))

  def main(args: Array[String]): Unit =
    println(exec)
}
