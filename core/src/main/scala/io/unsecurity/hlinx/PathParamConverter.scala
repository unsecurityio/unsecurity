package io.unsecurity.hlinx
import scala.util.Try

trait PathParamConverter[A] {
  def convert(s: String): Either[String, A]
}
object PathParamConverter {
  def create[A](f: String => Either[String, A]): PathParamConverter[A] =
    (s: String) => Try(f(s)).toEither.left.map(_.getMessage).flatMap(id => id)

  def createSimple[A](f: String => A): PathParamConverter[A] =
    (s: String) => Try(f(s)).toEither.left.map(_.getMessage)

  def apply[A: PathParamConverter]: PathParamConverter[A] = implicitly[PathParamConverter[A]]

  implicit val intParamConverter: PathParamConverter[Int]       = (s: String) => Try(s.toInt).toEither.left.map(_.getMessage)
  implicit val stringParamConverter: PathParamConverter[String] = (s: String) => Right(s)
}
case class Param[A: PathParamConverter](name: String, converter: PathParamConverter[A])
