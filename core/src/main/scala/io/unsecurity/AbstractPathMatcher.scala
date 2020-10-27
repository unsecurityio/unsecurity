package io.unsecurity

import cats.Monad
import io.unsecurity.hlinx.HLinx.{CaptureFailure, ContinueMatching, HLinx}
import io.unsecurity.hlinx.{ReversedTupled, SimpleParams}
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.log4s.getLogger
import shapeless.HList

abstract class AbstractPathMatcher[F[_]: Monad] {
  private[this] val log = getLogger
  type PathMatcher[A] = PartialFunction[String, Http4sDirective[F, A]]

  def createPathMatcher[PathParams <: HList, TUP](route: HLinx[PathParams])(implicit revTup: ReversedTupled.Aux[PathParams, TUP]): PathMatcher[TUP] =
    new PartialFunction[String, Http4sDirective[F, TUP]] {
      override def isDefinedAt(x: String): Boolean = {
        route.capture(x) match {
          case Right(_) => true
          case Left(_ : ContinueMatching) => true
          case _ => false
        }
      }

      override def apply(v1: String): Http4sDirective[F, TUP] = {
        val (path, params) = route.toSimple.reverse.partition {
          case _: SimpleParams => false
          case _               => true
        }
        val simpleRoute = s"/${path.mkString("/")}?${params.mkString("&")}"

        log.trace(s"""Match: "$v1" = $simpleRoute""")
        val value: Either[CaptureFailure, TUP] = route.capture(v1)

        value match {
          case Left(error) =>
            Http4sDirective.failure(
              HttpProblem
                .badRequest(
                  "Bad Request",
                  Some(s"""Error converting request [$v1] to pattern [$simpleRoute]: ${error.errorMessage}""")
                )
                .toResponse
            )

          case Right(params) =>
            Http4sDirective.success(params)

        }
      }
    }

}
