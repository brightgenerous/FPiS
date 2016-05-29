package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0305 {

  def exec = List.dropWhile(List(1,2,3,4,5))(n => n < 4)

  def main(args: Array[String]): Unit =
    println(exec)
}
