package io.unsecurity.hlinx
import scala.util.Try

trait PathParamConverter[A] extends ParamConverter[A]
object PathParamConverter {
  def apply[A: PathParamConverter]: PathParamConverter[A] = implicitly[PathParamConverter[A]]

  implicit val intParamConverter: PathParamConverter[Int]       = (s: String) => Try(s.toInt).toEither.left.map(_.getMessage)
  implicit val stringParamConverter: PathParamConverter[String] = (s: String) => Right(s)
}
case class Param[A: PathParamConverter](name: String, converter: PathParamConverter[A])
