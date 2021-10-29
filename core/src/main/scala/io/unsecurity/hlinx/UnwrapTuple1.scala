package io.unsecurity

import scala.language.implicitConversions

package object hlinx {

  type UnwrapTuple1[T <: Tuple] = T match {
    case a *: EmptyTuple => a
    case _ => T
  }

  extension [T <: Tuple](t: T) def unwrap: UnwrapTuple1[T] = {
    t match {
      case t *: EmptyTuple => t.asInstanceOf[UnwrapTuple1[T]]
      case a => a.asInstanceOf[UnwrapTuple1[T]]
    }
  }

  implicit def unwrapTuple1[T](tup: T *: EmptyTuple): T = tup.head

}



