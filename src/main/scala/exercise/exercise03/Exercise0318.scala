package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0318 {

  def exec = List.map(List(1,2,3))(_ * 2)

  def main(args: Array[String]): Unit =
    println(exec)
}
