package ch.epfl.scala.debugadapter.internal

import com.sun.jdi._

import scala.collection.JavaConverters._
import scala.util.Try

package object evaluator {
  private[evaluator] def method(name: String, referenceType: ReferenceType): Option[Method] =
    Try(referenceType.methodsByName(name).asScala.headOption).toOption.flatten

  private[evaluator] def method(name: String, signature: String, referenceType: ReferenceType): Option[Method] =
    Try(referenceType.methodsByName(name, signature).asScala.headOption).toOption.flatten

  private[evaluator] def invokeMethod(objRef: ObjectReference, method: Method, args: List[Value], thread: ThreadReference): Option[Value] =
    Try(objRef.invokeMethod(thread, method, args.asJava, ObjectReference.INVOKE_SINGLE_THREADED)).toOption
}