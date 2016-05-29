package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {

    // @tailrec
    def apply_inner(as: Seq[A]): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply_inner(as.tail))

    apply_inner(as)
  }

  def head[A](l: List[A]): A = l match {
    case Cons(x, _) => x
    case _ => sys.error("list is empty")
  }

  def setHead[A](l: List[A], n: A): List[A] =
    Cons(n, tail(l))

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, xs) => xs
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
      case Cons(x, xs) if f(x) => dropWhile_inner(xs)
      case _ => lst
    }

    dropWhile_inner(l)
  }

  def init[A](l: List[A]): List[A] = {

    // @tailrec
    def init_inner(lst: List[A]): List[A] = lst match {
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init_inner(xs))
    }

    l match {
      case Nil => sys.error("list is empty")
      case _ => init_inner(l)
    }
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    @tailrec
    def foldLeft_inner(lst: List[A], acc: B): B = lst match {
      case Cons(x, xs) => foldLeft_inner(xs, f(acc, x))
      case _ => acc
    }

    foldLeft_inner(as, z)
  }

  def foldLeftTwin[A, B, C](as1: List[A], as2: List[B], z: C)(f: (C, A, B) => C): C = {

    @tailrec
    def foldLeftTwin_inner(lst1: List[A], lst2: List[B], acc: C): C = (lst1, lst2) match {
      case (Cons(x1, xs1), Cons(x2, xs2)) => foldLeftTwin_inner(xs1, xs2, f(acc, x1, x2))
      case _ => acc
    }

    foldLeftTwin_inner(as1, as2, z)
  }

  def foldLeftShortcut[A, B](as: List[A], z: B)(f: (B, A) => B)(v: B, p: A => Boolean): B = {

    @tailrec
    def foldLeftShortcut_inner(lst: List[A], acc: B): B = lst match {
      case Cons(x, _) if p(x) => v
      case Cons(x, xs) => foldLeftShortcut_inner(xs, f(acc, x))
      case _ => acc
    }

    foldLeftShortcut_inner(as, z)
  }

  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    // 最終的に foldRight が返すのは B => B
    // -> acc: B => B が決まる
    // -> proc: (A, B => B) => (B => B) が決まる
    val acc: B => B = (z: B) => z
    val proc: (A, B => B) => (B => B) = (a: A, acc: B => B) => (b: B) => acc(f(b, a))
    foldRight(as, acc)(proc)(z)
  }

  def foldRight[A, B]: (List[A], B) => ((A, B) => B) => B = foldRightL

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {

    // @tailrec
    def foldRight2_inner(lst: List[A]): B = lst match {
      case Cons(x, xs) => f(x, foldRight2_inner(xs))
      case _ => z
    }

    foldRight2_inner(as)
  }

  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    // 最終的に foldLeft が返すのは B => B
    // -> acc: B => B が決まる
    // -> proc: (B => B, A) => (B => B) が決まる
    val acc: B => B = (z: B) => z
    val proc: (B => B, A) => (B => B) = (acc: B => B, a: A) => (b: B) => acc(f(a, b))
    foldLeft(as, acc)(proc)(z)
  }

  def foldRightTwin[A, B, C]: (List[A], List[B], C) => ((A, B, C) => C) => C = foldRightTwinL

  def foldRightTwinL[A, B, C](as1: List[A], as2: List[B], z: C)(f: (A, B, C) => C): C = {
    // 最終的に foldLeft が返すのは B => B
    // -> acc: C => C が決まる
    // -> proc: (C => C, A, B) => (C => C) が決まる
    val acc: C => C = (z: C) => z
    val proc: (C => C, A, B) => (C => C) = (acc: C => C, a1: A, a2: B) => (c: C) => acc(f(a1, a2, c))
    foldLeftTwin(as1, as2, acc)(proc)(z)
  }

  def foldRightShortcut[A, B]: (List[A], B) => ((A, B) => B) => (B, A => Boolean) => B = foldRightShortcutL

  def foldRightShortcutL[A, B](as: List[A], z: B)(f: (A, B) => B)(v: B, p: A => Boolean): B = {
    val acc: B => B = (z: B) => z
    val proc: (B => B, A) => (B => B) = (acc: B => B, a: A) => (b: B) => acc(f(a, b))
    foldLeftShortcut(as, acc)(proc)((z: B) => v, p)(z)
  }

  def length[A]: List[A] => Int = lengthL

  def lengthL[A](as: List[A]): Int =
    foldLeft(as, 0)((z, _) => z + 1)

  def lengthR[A](as: List[A]): Int =
    foldRight(as, 0)((_, z) => z + 1)

  def reverse[A]: List[A] => List[A] = reverseL

  def reverseL[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((z, n) => Cons(n, z))

  def append[A]: (List[A], List[A]) => List[A] = appendR

  def append2[A](as1: List[A], as2: List[A]): List[A] = {

    // @tailrec
    def append2_inner(lst1: List[A]): List[A] = lst1 match {
      case Cons(x, xs) => Cons(x, append2_inner(xs))
      case _ => as2
    }

    append2_inner(as1)
  }

  def appendR[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)((n, z) => Cons(n, z))

  def flatten[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append(_, _))

  val sum = sumL(_)

  def sum2(ints: List[Int]): Int = {

    // @tailrec
    def sum2_inner(lst: List[Int]): Int = lst match {
      case Cons(x, xs) => x + sum2_inner(xs)
      case _ => 0
    }

    sum2_inner(ints)
  }

  def sumL(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def sumR(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  val product = productShortcutL(_)

  def product2(ds: List[Double]): Double = {

    // @tailrec
    def product2_inner(lst: List[Double]): Double = lst match {
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product2_inner(xs)
      case _ => 1.0
    }

    product2_inner(ds)
  }

  def productL(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def productR(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def productShortcutL(ds: List[Double]): Double =
    foldLeftShortcut(ds, 1.0)(_ * _)(0.0, _ == 0.0)

  def productShortcutR(ds: List[Double]): Double =
    foldRightShortcut(ds, 1.0)(_ * _)(0.0, _ == 0.0)

  def incrementAll(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((n, z) => Cons(n + 1, z))

  def toStringAll(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((n, z) => Cons(n.toString, z))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((n, z) => Cons(f(n), z))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((n, z) => append(f(n), z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((n, z) => if (f(n)) Cons(n, z) else z)

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(n => if (f(n)) List(n) else List())

  def zipWith[A, B, C](as1: List[A], as2: List[B])(f: (A, B) => C): List[C] =
    foldRightTwin(as1, as2, Nil: List[C])((n1, n2, z) => Cons(f(n1, n2), z))

  def zipAdd(as1: List[Int], as2: List[Int]): List[Int] = {
    foldRightTwin(as1, as2, Nil: List[Int])((n1, n2, z) => Cons(n1 + n2, z))
  }

  def zipAdd2(as1: List[Int], as2: List[Int]): List[Int] = {

    // @tailrec
    def zipAdd_inner(lst1: List[Int], lst2: List[Int]): List[Int] = (lst1, lst2) match {
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, zipAdd_inner(xs1, xs2))
      case _ => Nil
    }

    zipAdd_inner(as1, as2)
  }
}
