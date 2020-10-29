package io.unsecurity

import cats.Monad
import io.unsecurity.hlinx.HLinx.{ContinueMatching, HLinx}
import io.unsecurity.hlinx.ReversedTupled
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.log4s.getLogger
import shapeless.HList

abstract class AbstractPathMatcher[F[_]: Monad] {
  private val log = getLogger
  type PathMatcher[A] = PartialFunction[String, Http4sDirective[F, A]]

  def createPathMatcher[PathParams <: HList, TUP](route: HLinx[PathParams])(implicit revTup: ReversedTupled.Aux[PathParams, TUP]): PathMatcher[TUP] =
    new PathMatcher[TUP] {
      override def isDefinedAt(x: String): Boolean = {
        route.capture(x) match {
          case Right(_)                  => true
          case Left(_: ContinueMatching) => true
          case _                         => false
        }
      }

      override def apply(v1: String): Http4sDirective[F, TUP] = {
        log.trace(s"""Match: "$v1" = ${route.renderString}""")
        route.capture(v1) match {
          case Left(error) =>
            Http4sDirective.failure(
              HttpProblem
                .badRequest(
                  "Bad Request",
                  Some(s"""Error converting request [$v1] to pattern [${route.renderString}]: ${error.errorMessage}""")
                )
                .toResponse
            )
          case Right(params) => Http4sDirective.success(params)
        }
      }
    }
}
