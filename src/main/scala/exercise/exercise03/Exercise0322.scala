package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0322 {

  def exec = List.zipAdd(List(1,2,3,4), List(4,5,6))

  def main(args: Array[String]): Unit =
    println(exec)
}
