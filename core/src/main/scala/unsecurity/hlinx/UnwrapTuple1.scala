package unsecurity
package hlinx

import shapeless.DepFn1

trait UnwrapTuple1[T] extends DepFn1[T] with Serializable

object UnwrapTuple1 extends LowPriorityUnwrapInstances {
  def apply[T](implicit unwrap: UnwrapTuple1[T]): Aux[T, unwrap.Out] = unwrap

  type Aux[T, Out0] = UnwrapTuple1[T] { type Out = Out0 }

  implicit val unwrapUnit: Aux[Unit, Unit] = new UnwrapTuple1[Unit] {
    type Out = Unit
    def apply(t: Unit): Out = ()
  }

  implicit def unwrapT1[A]: Aux[Tuple1[A], A] = new UnwrapTuple1[Tuple1[A]] {
    type Out = A
    def apply(t: Tuple1[A]): Out = t._1
  }
}

trait LowPriorityUnwrapInstances {
  implicit def unwrapTs[A <: Product]: UnwrapTuple1.Aux[A, A] = new UnwrapTuple1[A] {
    type Out = A
    def apply(t: A): Out = t
  }
}
