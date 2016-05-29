package exercise.exercise02

object Exercise0203 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def main(args: Array[String]): Unit = {
    val f = (a: Int, b: Int) => a + b
    println(f(1, 2))
    println(curry(f)(1)(2))
  }
}
