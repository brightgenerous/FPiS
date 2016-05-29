package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0310 {

  def exec = List.foldLeft(0, List(1,2,3))(_ + _)

  def main(args: Array[String]): Unit =
    println(exec)
}
