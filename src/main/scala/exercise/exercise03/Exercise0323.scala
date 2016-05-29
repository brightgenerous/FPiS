package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0323 {

  def exec = List.zipWith(List(1,2,3,4), List(4,5,6))(_ + _)

  def main(args: Array[String]): Unit =
    println(exec)
}
