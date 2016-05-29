package exercise03

import datastructures.List

object Exercise0304 {

  def exec = List.drop(List(1,2,3,4,5), 3)

  def main(args: Array[String]): Unit =
    println(exec)
}
