package ch.epfl.scala.debugadapter.internal.evaluator

import ch.epfl.scala.debugadapter.internal.EvaluationProvider.{evaluateFractionals, evaluateIntegrals}
import com.sun.jdi.{BooleanValue, ByteValue, CharValue, DoubleValue, FloatValue, IntegerValue, LongValue, PrimitiveValue, ShortValue, ThreadReference, Value, VirtualMachine}

import scala.runtime.BoxesRunTime
import scala.util.Try

object JdiPrimitive {
  def apply(value: PrimitiveValue, thread: ThreadReference): JdiPrimitive = value match {
    case value: DoubleValue => new JdiPrimitive(value.value(), thread)
    case value: FloatValue => new JdiPrimitive(value.value(), thread)
    case value: LongValue => new JdiPrimitive(value.value(), thread)
    case value: IntegerValue => new JdiPrimitive(value.value(), thread)
    case value: ShortValue => new JdiPrimitive(value.value(), thread)
    case value: CharValue => new JdiPrimitive(value.value(), thread)
    case value: ByteValue => new JdiPrimitive(value.value(), thread)
    case value: BooleanValue => new JdiPrimitive(value.value(), thread)
  }
}

private [evaluator] class JdiPrimitive(value: AnyVal, thread: ThreadReference) {
  private val vm = thread.virtualMachine()

//  def invoke(methodName: String, args: List[Value]): Option[Value] = for {
//    op <- op(methodName)
//    arg <- args.headOption.map(x => JdiPrimitive(x.asInstanceOf[PrimitiveValue], thread))
//    result <- Try(op(value, arg.value)).toOption
//  } yield vm.mirrorOf(result.asInstanceOf[Int])

//  private def evaluateOp(lhs: Value, rhs: Value, opName: String, vm: VirtualMachine): Either[String, Value] = (lhs, rhs) match {
//    case (x: PrimitiveValue, y: PrimitiveValue) => (x, y) match {
//      //    println(BoxesRunTime.toDouble(BoxesRunTime.add(4.2, 3.2)))
//      case (_: DoubleValue, _) | (_, _: DoubleValue) =>
//        evaluateFractionals(x.doubleValue(), y.doubleValue(), opName).map(vm.mirrorOf)
//      case (_: FloatValue, _) | (_, _: FloatValue) =>
//        evaluateFractionals(x.floatValue(), y.floatValue(), opName).map(vm.mirrorOf)
//      case (_: LongValue, _) | (_, _: LongValue) =>
//        evaluateIntegrals(x.longValue(), y.longValue(), opName).map(vm.mirrorOf)
//      case (_, _) | (_, _) =>
//        evaluateIntegrals(x.intValue(), y.intValue(), opName).map(vm.mirrorOf)
//      case _ => Left("unsupported operation")
//    }
//
//    case _ => Left("unable to evaluate non-primitives")
//  }
}
