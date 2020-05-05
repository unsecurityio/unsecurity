package io.unsecurity

import cats.Monad
import no.scalabin.http4s.directives.Directive
import org.http4s.Method.{GET, DELETE}
import org.http4s.implicits._
import org.http4s.headers.`Content-Type`

abstract class AbstractContentTypeMatcher[F[_]: Monad] extends AbstractMethodMatcher[F] {

  val WILDCARD = mediaType"*/*"

  def matchContentType[A](mediaRangeMap: MediaRangeMap[A]): Directive[F, A] = {

    for {
      request <- Directive.request[F]
      method  = request.method
      suppliedContentType: Option[`Content-Type`] = request.headers
        .get(`Content-Type`)
      contentType <- suppliedContentType
                      .orElse { if (method == GET || method == DELETE) Some(`Content-Type`.apply(WILDCARD)) else None }
                      .toSuccess(
                        HttpProblem
                          .unsupportedMediaType(
                            "Content-Type missing",
                            mediaRangeMap.supportedMediaRanges
                          )
                          .toDirectiveFailure
                      )
      a2rdf <- Directive.commit {
                mediaRangeMap.get(contentType.mediaType).toSuccess { supportedMediaTypes =>
                  Directive.error(
                    HttpProblem
                      .unsupportedMediaType(s"Content-Type '${suppliedContentType.map(_.value).getOrElse("not specified")}' invalid or unsupported mediatype", supportedMediaTypes)
                      .toResponse
                  )
                }
              }

    } yield {
      a2rdf
    }
  }

}
