package io.unsecurity.hlinx

import shapeless.ops.hlist.Reverse
import shapeless.{DepFn1, Generic, HList}

trait ReversedGeneric[L] extends DepFn1[L]

object ReversedGeneric {
  type Aux[L, O] = ReversedGeneric[L] { type Out = O }
  def apply[L](implicit r: ReversedGeneric[L]): Aux[L, r.Out] = r

  implicit def hListReverseGeneric[T, L <: HList, M <: HList](implicit reverse: Reverse.Aux[L, M],
                                                              generic: Generic.Aux[T, M]): ReversedGeneric.Aux[L, T] = {
    new ReversedGeneric[L] {
      type Out = T
      override def apply(l: L): T = {
        generic.from(reverse(l))
      }
    }
  }
}
