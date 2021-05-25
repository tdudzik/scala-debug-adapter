package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.internal.DebugAdapter.SourceLookUpProvider
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.sun.jdi._

import java.util.concurrent.CompletableFuture
import scala.collection.JavaConverters._
import scala.tools.nsc.ExpressionCompiler

object Evaluator {
  def evaluate(expression: String, thread: ThreadReference, frame: StackFrame)(sourceLookUpProvider: ISourceLookUpProvider): CompletableFuture[Value] = {
    val vm = thread.virtualMachine()
    val thisObject = frame.thisObject()


    val location = frame.location()
    val sourcePath = location.sourcePath()
    val lineNumber = location.lineNumber()

    val uri = sourceLookUpProvider.getSourceFileURI("", sourcePath)
    val content = sourceLookUpProvider.getSourceContents(uri)

    val result = for {
      classLoader <- classLoader(thisObject).flatMap(JdiClassLoader(_, thread))
      names <- Some(frame.visibleVariables().asScala.map(_.name()).map(vm.mirrorOf).toList)
      values <- Some(frame.visibleVariables().asScala.map(frame.getValue).flatMap(value => boxIfNeeded(value, classLoader, thread)).toList)
      systemClass <- classLoader.loadClass("java.lang.System")
      classPath <- systemClass
        .invokeStatic("getProperty", List(vm.mirrorOf("java.class.path")))
        .map(_.toString)
        .map(_.drop(1).dropRight(1)) // remove quotation marks
      expressionCompiler <- Some(ExpressionCompiler(classPath, lineNumber))
      expressionClassPath = s"file://${expressionCompiler.dir.toString}/"
      _ <- Some(expressionCompiler.compile(content, expression))
      url <- classLoader
        .loadClass("java.net.URL")
        .flatMap(_.newInstance(List(vm.mirrorOf(expressionClassPath))))
      urls <- JdiArray("java.net.URL", 1, classLoader, thread)
      _ <- urls.setValue(0, url.reference)
      urlClassLoader <- classLoader
        .loadClass("java.net.URLClassLoader")
        .flatMap(_.newInstance(List(urls.reference)))
        .map(_.reference.asInstanceOf[ClassLoaderReference])
        .flatMap(JdiClassLoader(_, thread))
      expressionClass <- urlClassLoader.loadClass("Expression")
      expression <- expressionClass.newInstance(List())
      namesArray <- JdiArray("java.lang.String", names.size, classLoader, thread)
      valuesArray <- JdiArray("java.lang.Object", values.size, classLoader, thread) // add boxing
      _ <- Some(namesArray.setValues(names))
      _ <- Some(valuesArray.setValues(values))
      result <- expression.invoke("evaluate", List(namesArray.reference, valuesArray.reference))
    } yield result

    // Handle error
    CompletableFuture.completedFuture(result.get)
  }

  private def boxIfNeeded(value: Value, classLoader: JdiClassLoader, thread: ThreadReference): Option[Value] = value match {
    case value: IntegerValue => JdiPrimitive.boxed(value.value(), classLoader, thread).map(_.reference)
    case value => Some(value)
  }
}
