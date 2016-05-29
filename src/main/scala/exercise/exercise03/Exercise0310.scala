package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0310 {

  def exec = List.foldLeft(List(1,2,3), 0)(_ + _)

  def main(args: Array[String]): Unit =
    println(exec)
}
