package exercise03

import datastructures.List

object Exercise0303 {

  def exec = List.setHead(List(1,2,3,4,5), 100)

  def main(args: Array[String]): Unit =
    println(exec)
}
