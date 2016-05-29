package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](l: List[A]): A = l match {
    case Cons(h, t) => h
    case _ => sys.error("list is empty")
  }

  def setHead[A](l: List[A], n: A): List[A] =
    Cons(n, tail(l))

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) => t
    case _ => sys.error("list is empty")
  }

  def drop[A](l: List[A], n: Int): List[A] = {

    @tailrec
    def drop_inner(lst: List[A], cnt: Int): List[A] =
      if (cnt <= 0) lst
      else drop_inner(tail(lst), cnt - 1)

    drop_inner(l, n)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {

    @tailrec
    def dropWhile_inner(lst: List[A]): List[A] = lst match {
      case Cons(h, t) if f(h) => dropWhile_inner(t)
      case _ => lst
    }

    dropWhile_inner(l)
  }

  def init[A](l: List[A]): List[A] = {

    def init_inner(lst: List[A]): List[A] = lst match {
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init_inner(t))
    }

    l match {
      case Nil => sys.error("list is empty")
      case _ => init_inner(l)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Cons(h, t) => Cons(h, append(t, a2))
    case _ => a2
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](z: B, as: List[A])(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(f(z, x), xs)(f)
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, z) => z + 1)

  def lengthL[A](as: List[A]): Int =
    foldLeft(0, as)((z, _) => z + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(Nil: List[A], as)((z, n) => Cons(n, z))

  def sum(ints: List[Int]): Int = ints match {
    case Cons(x, xs) => x + sum(xs)
    case _ => 0
  }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def sumL(ints: List[Int]): Int =
    foldLeft(0, ints)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
    case _ => 1.0
  }

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def productL(ds: List[Double]): Double =
    foldLeft(1.0, ds)(_ * _)
}
