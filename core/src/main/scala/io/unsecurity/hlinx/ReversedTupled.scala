package io.unsecurity.hlinx

type Reversed[T <: Tuple] <: Tuple = T match {
  case EmptyTuple => EmptyTuple
  case t *: ts => Tuple.Concat[Reversed[ts], t *: EmptyTuple]
}

extension [T <: Tuple](t: T) def reversed: Reversed[T] =
  Tuple.fromArray(t.toList.reverse.toArray[Any]).asInstanceOf[Reversed[T]]

object S {
  val s: Reversed[(Int, String)] = (1, "").reversed
}