package io.unsecurity.hlinx

import scala.util.Try
import cats.implicits._

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

case class Param[A](name: String, converter: ParamConverter[A]){
  def ? : Params[Option[A]] = Params(name, ParamsConverter.optionsParamConverter(converter))
  def * : Params[List[A]] = Params(name, ParamsConverter.listParamConverter(converter))
}

case class Params[A](name: String, converter: ParamsConverter[A])

trait ParamsConverter[A] {
  def convert(s: List[String]): Either[String, A]
}

object ParamsConverter {

  def singleParamConverter[A](converter: ParamConverter[A]): ParamsConverter[A] = (l: List[String]) => l
        .headOption
        .map(converter.convert)
        .getOrElse(Left(s"Missing"))


  def optionsParamConverter[A](converter: ParamConverter[A]): ParamsConverter[Option[A]] = (l: List[String]) => l
      .headOption
      .map(converter.convert)
      .map(_.map(Option(_)))
      .getOrElse(Right(None))

  def listParamConverter[A](converter: ParamConverter[A]): ParamsConverter[List[A]] = (l: List[String]) => l
        .map(converter.convert)
        .separate match {
    case (error :: _, _) => Left(error)
    case (_, values) => Right(values)
  }
}