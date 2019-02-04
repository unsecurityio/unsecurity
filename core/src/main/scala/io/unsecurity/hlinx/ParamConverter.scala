package io.unsecurity.hlinx

trait ParamConverter[A] {
  def convert(s: String): Either[String, A]
}
