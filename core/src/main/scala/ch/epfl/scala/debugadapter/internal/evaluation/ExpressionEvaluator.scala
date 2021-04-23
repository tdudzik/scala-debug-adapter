package ch.epfl.scala.debugadapter.internal.evaluation

import com.sun.jdi.{ObjectReference, ThreadReference, Value}

import java.util.concurrent.CompletableFuture
import collection.JavaConverters._

private [internal] object ExpressionEvaluator {
  def evaluate(expression: String, thread: ThreadReference, depth: Int): CompletableFuture[Value] = {
    val frame = thread.frame(depth)
    val c = frame.getValue(frame.visibleVariableByName("c")).asInstanceOf[ObjectReference]
    val d = frame.getValue(frame.visibleVariableByName("d"))
    val addMethod = c.referenceType().methodsByName("add").asScala.head
    val num = c.invokeMethod(thread, addMethod, List(d).asJava, 0).asInstanceOf[ObjectReference]
    val value = num.getValue(num.referenceType().fieldByName("value"))

    CompletableFuture.completedFuture(value)
  }
}
