package ch.epfl.scala.debugadapter.internal.evaluator

import buildinfo.BuildInfo
import com.sun.jdi.{ClassObjectReference, ObjectReference, ThreadReference, Value}

import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture
import scala.tools.nsc.Main

object Evaluator {
  def evaluate(expression: String, objectReference: ObjectReference, thread: ThreadReference): CompletableFuture[Value] = {
    compileExpression(expression)

    val vm = thread.virtualMachine()
    val classPath = "file:///home/tdudzik/expr-eval/"
    val result = for {
      classLoader <- JdiClassLoader(objectReference, thread)
      url <- classLoader
        .loadClass("java.net.URL")
        .flatMap(_.newInstance(List(vm.mirrorOf(classPath))))
      urls <- JdiArray("java.net.URL", 1, classLoader, thread)
      _ <- urls.setValue(0, url.reference)
      urlClassLoader <- classLoader
        .loadClass("java.net.URLClassLoader")
        .flatMap(_.newInstance(List(urls.reference)))
      global <- urlClassLoader
        .invoke(
          "loadClass",
          "(Ljava/lang/String;)Ljava/lang/Class;",
          List(vm.mirrorOf("Global")))
        .map(_.asInstanceOf[ClassObjectReference])
        .map(new JdiClassObject(_, classLoader, thread))
        .flatMap(_.newInstance(List()))
      result <- global.invoke("exec", List())
    } yield result

    CompletableFuture.completedFuture(result.get)
  }

  private def compileExpression(expression: String): Unit = {
    val dir = Paths.get("/home/tdudzik/expr-eval")
    val path = dir.resolve("Global.scala")
    Files.writeString(path,
      s"""class Global {
         |  def exec() = {
         |    $expression
         |  }
         |}
         |""".stripMargin)
    val args = Array(
      "-d", dir.toAbsolutePath.toString,
      "-classpath", BuildInfo.scalaLibraries,
      path.toAbsolutePath.toString
    )
    val success = Main.process(args)
    if (!success) throw new IllegalArgumentException(s"cannot compile $path")
  }
}
