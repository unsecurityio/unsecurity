package io.unsecurity.hlinx

import io.unsecurity.hlinx.TransformParams.NotUnit

import scala.Tuple.Filter

type TransformParams[T <: Tuple, RU <: Tuple] = Filter[Tuple.Concat[T,RU], NotUnit]

extension [Tup <: Tuple](tup: Tup){
  def filter[P[_] <: Boolean](p : [t] => t => P[t]): Filter[Tup, P] = (tup match {
    case e : EmptyTuple => e
    case h *: tail => p(h) match {
      case true => h *: tail.filter(p)
      case false => tail
    }
  }).asInstanceOf[Filter[Tup, P]]
}

object TransformParams {

  type NotUnit[T] <: Boolean = T match {
    case Unit => false
    case _ => true
  }

  def apply[T <: Tuple, RU <: Tuple](tupled: T, ru: RU): TransformParams[T, RU] = {
    val appended: Tuple.Concat[T,RU] = tupled ++ ru
    val notUnit: [T] => T => NotUnit[T] = [T] => (t : T) => (t match {
      case _:Unit => false
      case _ => true
    }).asInstanceOf[NotUnit[T]]
    val filtered: Tuple.Filter[Tuple.Concat[T,RU], NotUnit] = appended.filter(notUnit)
    filtered
  }
}
