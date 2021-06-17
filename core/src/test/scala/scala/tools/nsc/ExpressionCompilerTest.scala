package scala.tools.nsc

import utest._

import java.net.{URL, URLClassLoader}

object ExpressionCompilerTest extends TestSuite {
    def tests: Tests = Tests {
      "compile" - {
        val expressionCompiler = ExpressionCompiler(System.getProperty("java.class.path"), 7)
        val source =
          """object EvaluateTest {
            |  def main(args: Array[String]): Unit = {
            |    implicit val a: Int = 1
            |    val b: Int = 2
            |    var c: String = "1 + 2 = "
            |    val z: Int = 10
            |    println("hello")
            |  }
            |}
            |
            |class Expression {
            |  def evaluate(names: Array[Any], values: Array[Any]) = {
            |    val valuesByName = names.map(_.asInstanceOf[String]).zip(values).toMap
            |    val a: Int = valuesByName("a").asInstanceOf[Int]
            |    val b: Int = valuesByName("b").asInstanceOf[Int]
            |    val c: String = valuesByName("c").asInstanceOf[String]
            |    ()
            |  }
            |}
            |""".stripMargin

        // it reports an error:
        // ```
        // pos: NoPosition Error while emitting <source>
        // value b ERROR
        // ```
        // but works anyway
        expressionCompiler.compile(source,  "b * a + 2 + z")

        val url = new URL("file://" + expressionCompiler.dir + "/")
        val urlClassLoader = new URLClassLoader(Array(url))
        val expressionClass = urlClassLoader.loadClass("Expression")
        val expression = expressionClass.newInstance()
        val method = expressionClass.getMethods.find(_.getName == "evaluate").get
        val result = method.invoke(expression, Array[Any]("a", "b", "c"), Array[Any](10 , 2, "a + b = "))
        println("result = " + result)
      }
    }
}
