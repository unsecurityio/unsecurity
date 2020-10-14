package io.unsecurity

import cats.Monad
import io.unsecurity.hlinx.HLinx.{HLinx, HPath}
import io.unsecurity.hlinx.{ReversedTupled, SimpleParams}
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.log4s.getLogger
import shapeless.HList

abstract class AbstractPathMatcher[F[_]: Monad] {
  private[this] val log = getLogger
  type PathMatcher[A] = PartialFunction[String, Http4sDirective[F, A]]

  def createPathMatcher[PathParams <: HList, TUP](route: HLinx[PathParams])(
      implicit revTup: ReversedTupled.Aux[PathParams, TUP]): PathMatcher[TUP] =
    new PartialFunction[String, Http4sDirective[F, TUP]] {
      override def isDefinedAt(x: String): Boolean = {
        val (path, params) = route.toSimple.reverse.partition{
          case p: SimpleParams => false
          case p               => true
        }
        if (route.capture(x).isDefined) {
          log.trace(s"""'$x' did match /${path.mkString("/")}?${params.mkString("&")}""")
          true
        } else {
          log.trace(s"""'$x' did not match /${path.mkString("/")}?${params.mkString("&")}""")
          false
        }
      }

      override def apply(v1: String): Http4sDirective[F, TUP] = {
        val (path, params) = route.toSimple.reverse.partition{
          case p: SimpleParams => false
          case p               => true
        }
        val simpleRoute = s"/${path.mkString("/")}?${params.mkString("&")}"

        log.trace(s"""Match: "$v1" = $simpleRoute""")
        val value: Either[String, TUP] = route.capture(v1).get

        value match {
          case Left(errorMsg) =>
            Http4sDirective.failure(
              HttpProblem
                .badRequest("Bad Request", Some(s"""Error converting "$v1" = $simpleRoute: $errorMsg"""))
                .toResponse
            )

          case Right(params) =>
            Http4sDirective.success(params)

        }
      }
    }

}
