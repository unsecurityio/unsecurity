package io.unsecurity.hlinx

import shapeless.DepFn2
import shapeless.ops.tuple.{FilterNot, Prepend}

trait TransformParams[T, RU] extends DepFn2[T, RU]

object TransformParams {
  type Aux[T, RU, O] = TransformParams[T, RU] { type Out = O }
  def apply[T, RU](implicit r: TransformParams[T, RU]): Aux[T, RU, r.Out] = r

  implicit def transform[T, RU <: Product, T2, T3, T4](
      implicit append: Prepend.Aux[T, RU, T2],
      filterNot: FilterNot.Aux[T2, Unit, T3],
      unwrap: UnwrapTuple1.Aux[T3, T4]
  ): TransformParams.Aux[T, RU, T4] = {
    new TransformParams[T, RU] {
      type Out = T4
      override def apply(tupled: T, ru: RU): T4 = {
        val appended: T2 = append(tupled, ru)
        val filtered: T3 = filterNot(appended)
        val unwrapped: T4 = unwrap(filtered)
        unwrapped
      }
    }
  }
}
