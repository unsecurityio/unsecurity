package unsecurity.hlinx

object Spike extends App {
  println(UnwrapTuple1[Unit].apply(()))
  println(UnwrapTuple1[Tuple1[Int]].apply(Tuple1(1)))
  println(UnwrapTuple1[(Int, Int)].apply((1, 2)))
}
