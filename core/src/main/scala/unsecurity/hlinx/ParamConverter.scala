package unsecurity
package hlinx

import scala.util.Try

trait ParamConverter[A] {
  def convert(s: String): Either[String, A]
  def map[B](f: A => B): ParamConverter[B] = { s =>
    convert(s).map(f)
  }
  def flatMap[B](f: A => Either[String, B]): ParamConverter[B] = { s =>
    convert(s).flatMap(f)
  }
}
object ParamConverter {
  implicit val stringParamConverter: ParamConverter[String] = (s: String) => Right(s)

  implicit val intParamConverter: ParamConverter[Int] = (s: String) =>
    Try {
      s.toInt
    }.toEither.left.map(_.getMessage)

  def create[A](f: String => Either[String, A]): ParamConverter[A] =
    (s: String) =>
      Try {
        f(s)
      }.toEither.left.map(_.getMessage).flatMap(id => id)

  def createSimple[A](f: String => A): ParamConverter[A] =
    (s: String) =>
      Try {
        f(s)
      }.toEither.left.map(_.getMessage)

  def apply[A: ParamConverter]: ParamConverter[A] = implicitly[ParamConverter[A]]
}
case class Param[A: ParamConverter](name: String, converter: ParamConverter[A])
