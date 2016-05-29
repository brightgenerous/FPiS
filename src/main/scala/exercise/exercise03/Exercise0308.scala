package exercise.exercise03

import fpinscala.datastructures.{ Cons, List, Nil }

object Exercise0308 {

  def exec = List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))

  def main(args: Array[String]): Unit =
    println(exec)
}
