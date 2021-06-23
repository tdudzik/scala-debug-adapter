package scala.tools.nsc

import java.nio.file.{Files, Path}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter

object ExpressionCompiler {
  def apply(classPath: String, line: Int): ExpressionCompiler = {
    val dir = Files.createTempDirectory("expr-eval")
    val settings = new Settings
    settings.classpath.value = classPath
    settings.outputDirs.setSingleOutput(dir.toString)
    val reporter = new StoreReporter
    val global = new EvalGlobal(settings, reporter, line)
    new ExpressionCompiler(global, reporter, dir)
  }
}

class ExpressionCompiler(val global: EvalGlobal, val reporter: StoreReporter, val dir: Path) {
  private val compilerRun = new global.Run() {
    override protected def stopPhase(name: String): Boolean = {
      println(s"[phase] $name")
      super.stopPhase(name)
    }
  }

  def compile(code: String, expression: String): Unit = {
    val lines = code.split("\n")
    val newCode = (lines.take(global.line - 1) ++ Seq(expression) ++ lines.drop(global.line - 1)).mkString("\n")
    val source = new BatchSourceFile(
      "<source>",
      newCode
    )
    compilerRun.compileSources(List(source))
    reporter.infos.foreach(println)
  }
}
