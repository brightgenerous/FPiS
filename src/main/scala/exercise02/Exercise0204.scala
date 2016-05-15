package exercise02

object Exercise0204 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def main(args: Array[String]): Unit = {
    val f = (a: Int) => (b: Int) => a + b
    println(f(1)(2))
    println(uncurry(f)(1, 2))
  }
}
