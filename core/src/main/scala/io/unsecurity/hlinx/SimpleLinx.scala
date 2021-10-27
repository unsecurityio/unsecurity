package io.unsecurity.hlinx

// TODO move overlaps functionality into this
// TODO good error messages when overlapping
// TODO wrapper class instead of List
sealed trait SimpleLinx extends Ordered[SimpleLinx] {
  override def compare(that: SimpleLinx): Int =
    (this, that) match {
      case (a: SimpleStatic, b: SimpleStatic)     => a.segment.compare(b.segment)
      case (_: SimpleStatic, _: SimpleVariable)   => -1
      case (_: SimpleVariable, _: SimpleStatic)   => 1
      case (a: SimpleVariable, b: SimpleVariable) => a.name.compare(b.name)
      case (_: SimpleParams, _)                   => -1
      case (_, _: SimpleParams)                   => 1
    }
}
case class SimpleStatic(segment: String) extends SimpleLinx {
  override def toString: String = segment
}
case class SimpleVariable(name: String) extends SimpleLinx {
  override def toString: String = s"{$name}"
}

case class SimpleParams(name: String) extends SimpleLinx {
  override def toString: String = s"{$name}"
}
