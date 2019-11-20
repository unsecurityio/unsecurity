package io.unsecurity

import cats.Monad
import cats.data.NonEmptyList
import no.scalabin.http4s.directives.Directive
import org.http4s.Method
import org.http4s.headers.{`Content-Type`, Allow}

abstract class AbstractContentTypeMatcher[F[_]: Monad] extends AbstractMethodMatcher[F] {

  def matchContentType[A](mediaRangeMap: MediaRangeMap[A]): Directive[F, A] = {

    for {
      request <- Directive.request[F]
      // TODO dette skal ikke gjelde for GET?
      contentType <- request.headers
                      .get(`Content-Type`)
                      .toSuccess(
                        HttpProblem
                          .unsupportedMediaType("Content-Type missing or invalid mediatype", Set.empty)
                          .toDirectiveFailure
                      )
      a2rdf <- Directive.commit {
                mediaRangeMap.get(contentType.mediaType).toSuccess { supportedMediaTypes =>
                  Directive.error(
                    HttpProblem
                      .unsupportedMediaType("Content-Type missing or invalid mediatype", supportedMediaTypes)
                      .toResponse
                  )
                }
              }

    } yield {
      a2rdf
    }
  }

}
