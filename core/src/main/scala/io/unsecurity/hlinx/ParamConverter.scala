package io.unsecurity.hlinx

import java.net.URLDecoder

import scala.util.Try

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
  implicit val stringParamConverter: ParamConverter[String] = (s: String) =>
    Try {
      URLDecoder.decode(s, "UTF-8")
    }.toEither.left.map(_.getMessage)

  implicit val intParamConverter: ParamConverter[Int] =
    stringParamConverter.flatMap { (urlDecodedString: String) =>
      Try {
        urlDecodedString.toInt
      }.toEither.left.map(_.getMessage)
    }

  def create[A](f: String => Either[String, A]): ParamConverter[A] =
    stringParamConverter.flatMap { (urlDecodedString: String) =>
    Try {
      f(urlDecodedString)
    }.toEither.left.map(_.getMessage).flatMap(id => id)
  }

  def createSimple[A](f: String => A): ParamConverter[A] =
    stringParamConverter.flatMap { (urlDecodedString: String) =>
      Try {
        f(urlDecodedString)
      }.toEither.left.map(_.getMessage)
    }

  def apply[A: ParamConverter]: ParamConverter[A] = implicitly[ParamConverter[A]]
}
case class Param[A: ParamConverter](name: String, converter: ParamConverter[A])
