package io.unsecurity.hlinx

import shapeless.HList
import shapeless.ops.hlist.{Reverse, Tupler}

trait ReversedTupled[L] {
  type Out
  def apply(l: L): Out
}

object ReversedTupled {
  type Aux[L, O] = ReversedTupled[L] { type Out = O }
  def apply[L](implicit r: ReversedTupled[L]): Aux[L, r.Out] = r

  implicit def hListReverseTupled[T, L <: HList, M <: HList](implicit reverse: Reverse.Aux[L, M],
                                                             tupler: Tupler.Aux[M, T]): ReversedTupled.Aux[L, T] = {
    new ReversedTupled[L] {
      type Out = T
      override def apply(l: L): T = {
        tupler(reverse(l))
      }
    }
  }
}
