package io.unsecurity

import cats.Monad
import io.unsecurity.hlinx.HLinx.{ContinueMatching, HLinx}
import io.unsecurity.hlinx._
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.http4s.{Query, Uri}
import org.log4s.getLogger

abstract class AbstractPathMatcher[F[_]: Monad] {
  private val log = getLogger
  type PathMatcher[A] = PartialFunction[(Uri.Path, Query), Http4sDirective[F, A]]

  def createPathMatcher[PathParams <: Tuple](route: HLinx[PathParams]): PathMatcher[Reversed[PathParams]] =
    new PathMatcher[Reversed[PathParams]] {
      override def isDefinedAt(x: (Uri.Path, Query)): Boolean = {
        route.capture(x) match {
          case Right(_)                  => true
          case Left(_: ContinueMatching) => true
          case _                         => false
        }
      }

      override def apply(v1: (Uri.Path, Query)): Http4sDirective[F, Reversed[PathParams]] = {
        log.trace(s"""Match: "$v1" = ${route.renderString}""")
        route.capture(v1) match {
          case Right(params) => Http4sDirective.success(params)
          case Left(error) =>
            Http4sDirective.failure(
              HttpProblem
                .badRequest(
                  "Bad Request",
                  Some(s"""Error converting request [$v1] to pattern [${route.renderString}]: ${error.errorMessage}""")
                )
                .toResponse
            )
        }
      }
    }
}
