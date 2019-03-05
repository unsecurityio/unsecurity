package io.unsecurity.hlinx

import scala.util.Try

//trait Converter[A, B] {
//  def convert(a: A): Either[String, B]
//  def map[C](f: B => C): Converter[A, C]
//  def flatMap[C](f: Converter[B, C]): Converter[A, C]
//}
//
//type PConverter[A] = Converter[String, A]

trait ParamConverter[A] {
  def convert(s: String): Either[String, A]
  def map[B](f: A => B): ParamConverter[B] = {
    ParamConverter.create { s =>
      convert(s).map(f)
    }
  }
  def flatMap[B](f: A => Either[String, B]): ParamConverter[B] = {
    ParamConverter.create { s =>
      convert(s).flatMap(f)
    }
  }
}
object ParamConverter {
  def create[A](f: String => Either[String, A]): ParamConverter[A] =
    (s: String) => Try(f(s)).toEither.left.map(_.getMessage).flatMap(id => id)

  def createSimple[A](f: String => A): ParamConverter[A] =
    (s: String) => Try(f(s)).toEither.left.map(_.getMessage)

  def apply[A: ParamConverter]: ParamConverter[A] = implicitly[ParamConverter[A]]

  implicit val intParamConverter: ParamConverter[Int]       = (s: String) => Try(s.toInt).toEither.left.map(_.getMessage)
  implicit val stringParamConverter: ParamConverter[String] = (s: String) => Right(s)
}
case class Param[A: ParamConverter](name: String, converter: ParamConverter[A])
