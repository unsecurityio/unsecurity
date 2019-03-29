package io.unsecurity.hlinx

import java.net.URLDecoder

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
  // TODO use stringParamConverter as base for every creation of paramconverters (to include url decoding)
  implicit val stringParamConverter: ParamConverter[String] = (s: String) =>
    Try {
      URLDecoder.decode(s, "UTF-8")
    }.toEither.left.map(_.getMessage)

  implicit val intParamConverter: ParamConverter[Int] = (s: String) =>
    Try {
      val decoded = URLDecoder.decode(s, "UTF-8")
      decoded.toInt
    }.toEither.left.map(_.getMessage)

  def create[A](f: String => Either[String, A]): ParamConverter[A] = { (s: String) =>
    Try {
      val decoded: String = URLDecoder.decode(s, "UTF-8")
      f(decoded)
    }.toEither.left.map(_.getMessage).flatMap(id => id)
  }

  def createSimple[A](f: String => A): ParamConverter[A] =
    (s: String) =>
      Try {
        val decoded: String = URLDecoder.decode(s, "UTF-8")
        f(decoded)
      }.toEither.left.map(_.getMessage)

  def apply[A: ParamConverter]: ParamConverter[A] = implicitly[ParamConverter[A]]

}
case class Param[A: ParamConverter](name: String, converter: ParamConverter[A])
