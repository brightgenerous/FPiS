package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0320 {

  def exec = List.flatMap(List(1,2,3))(i => List(i, i))

  def main(args: Array[String]): Unit =
    println(exec)
}
