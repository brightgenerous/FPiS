package exercise02

object Exercise0205 {

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    val g = (a: Int) => a.toDouble + 0.5
    val f = (b: Double) => (b * 3).toString
    val h = compose(f, g)
    println(f(g(1)))
    println(h(1))
  }
}
