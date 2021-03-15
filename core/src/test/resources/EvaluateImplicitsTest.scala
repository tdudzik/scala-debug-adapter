object EvaluateImplicitsTest {
  def main(args: Array[String]): Unit = {
    val x = 1
    val y = 2
    val z = add(x, y)
    println(z)
  }

  private def add[T](x: T, y: T)(implicit numeric: Numeric[T]) = numeric.plus(x, y)
}
