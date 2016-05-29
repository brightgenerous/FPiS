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

  def foldRight[A, B]: (List[A], B) => ((A, B) => B) => B = foldRightL

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight2(xs, z)(f))
  }

  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    // 最終的に foldLeft が返すのは B => B
    // -> acc: B => B が決まる
    // -> proc: (B => B, A) => (B => B) が決まる
    val acc: B => B = (z: B) => z
    val proc: (B => B, A) => (B => B) = (acc: B => B, a: A) => (b: B) => acc(f(a, b))
    foldLeft(as, acc)(proc)(z)
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    // 最終的に foldRight が返すのは B => B
    // -> acc: B => B が決まる
    // -> proc: (A, B => B) => (B => B) が決まる
    val acc: B => B = (z: B) => z
    val proc: (A, B => B) => (B => B) = (a: A, acc: B => B) => (b: B) => acc(f(b, a))
    foldRight(as, acc)(proc)(z)
  }

  def length[A]: List[A] => Int = lengthL

  def lengthR[A](as: List[A]): Int =
    foldRight(as, 0)((_, z) => z + 1)

  def lengthL[A](as: List[A]): Int =
    foldLeft(as, 0)((z, _) => z + 1)

  def reverse[A]: List[A] => List[A] = reverseL

  def reverseL[A](as: List[A]): List[A] =
    foldLeft(Nil: List[A], as)((z, n) => Cons(n, z))

  def append[A]: (List[A], List[A]) => List[A] = appendR

  def append2[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Cons(h, t) => Cons(h, append2(t, a2))
    case _ => a2
  }

  def appendR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((n, z) => Cons(n, z))

  def flatten[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append(_, _))

  val sum = sumL(_)

  def sum2(ints: List[Int]): Int = ints match {
    case Cons(x, xs) => x + sum(xs)
    case _ => 0
  }

  def sumR(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def sumL(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  val product = productL(_)

  def product2(ds: List[Double]): Double = ds match {
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
    case _ => 1.0
  }

  def productR(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def productL(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)
}
