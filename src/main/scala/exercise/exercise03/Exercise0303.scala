package exercise.exercise03

import fpinscala.datastructures.List

object Exercise0303 {

  def exec = List.setHead(List(1,2,3,4,5), 100)

  def main(args: Array[String]): Unit =
    println(exec)
}
