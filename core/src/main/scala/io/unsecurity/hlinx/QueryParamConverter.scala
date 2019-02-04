package io.unsecurity.hlinx
import scala.util.Try

trait QueryParamConverter[A] extends ParamConverter[A]
object QueryParamConverter {
  implicit val intParamConverter: QueryParamConverter[Int]       = (s: String) => Try(s.toInt).toEither.left.map(_.getMessage)
  implicit val stringParamConverter: QueryParamConverter[String] = (s: String) => Right(s)
}
case class QueryParam[A : QueryParamConverter](name: String)
