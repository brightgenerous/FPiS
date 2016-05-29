package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0302 {

  def exec = List.tail(List(1,2,3,4,5))

  def main(args: Array[String]): Unit =
    println(exec)
}
