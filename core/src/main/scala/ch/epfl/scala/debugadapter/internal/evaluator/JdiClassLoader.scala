package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.{ClassLoaderReference, ClassObjectReference, ClassType, Method, ObjectReference, ReferenceType, ThreadReference}

import scala.util.Try

private[evaluator] object JdiClassLoader {
  def apply(objectReference: ObjectReference, thread: ThreadReference): Option[JdiClassLoader] =
    for {
      classLoader <- Try(objectReference.referenceType().classLoader()).toOption
      classLoaderType <- Try(classLoader.referenceType().asInstanceOf[ClassType]).toOption
      loadClassMethod <- loadClassMethod(classLoaderType)
    } yield new JdiClassLoader(classLoader, loadClassMethod, thread)

  private def loadClassMethod(classLoaderType: ReferenceType): Option[Method] =
    method("loadClass", "(Ljava/lang/String;)Ljava/lang/Class;", classLoaderType)
}

private [evaluator] class JdiClassLoader(
                      classLoaderRef: ClassLoaderReference,
                      loadClassMethod: Method,
                      thread: ThreadReference) {
  private val vm = thread.virtualMachine()

  def loadClass(name: String): Option[JdiClassObject] =
    invokeMethod(classLoaderRef, loadClassMethod, List(vm.mirrorOf(name)), thread)
      .map(_.asInstanceOf[ClassObjectReference])
      .map(new JdiClassObject(_, this, thread))
}
