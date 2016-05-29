package datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(head, tail) => tail
    case _ => sys.error("list is empty")
  }

  def drop[A](l: List[A], n: Int): List[A] = {

    @tailrec
    def drop_inner(lst: List[A], count: Int): List[A] =
      if (count <= 0) lst
      else drop_inner(tail(lst), count - 1)

    drop_inner(l, n)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

    @tailrec
    def dropWhile_inner(lst: List[A]): List[A] = lst match {
      case Cons(head, tail) if f(head) => dropWhile_inner(tail)
      case _ => lst
    }

    dropWhile_inner(l)
  }

  def setHead[A](l: List[A], n: A): List[A] =
    Cons(n, tail(l))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
