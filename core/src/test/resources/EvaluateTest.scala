object EvaluateTest {
  def main(args: Array[String]): Unit = {
    val c = Num(9)
    val d = Num(5)
    println(c.add(d))
  }

  case class Num(value: Int) {
    def add(num: Num): Num = Num(value + num.value)
  }
}
