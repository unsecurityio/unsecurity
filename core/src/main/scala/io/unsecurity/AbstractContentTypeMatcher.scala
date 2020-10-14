package io.unsecurity

import cats.Monad
import cats.data.NonEmptyList
import no.scalabin.http4s.directives.Directive
import org.http4s.Method.{DELETE, GET}
import org.http4s.implicits._
import org.http4s.headers.{Accept, `Content-Type`}

abstract class AbstractContentTypeMatcher[F[_]: Monad] extends AbstractMethodMatcher[F] {

  val WILDCARD = mediaType"*/*"

  def matchRequestContentType[A](mediaRangeMap: MediaRangeMap[A]): Directive[F, NonEmptyList[ResponseAlternativeForContent[A]]] = {

    for {
      request             <- Directive.request[F]
      method              = request.method
      suppliedContentType = request.headers.get(`Content-Type`)
      contentType <- suppliedContentType
                      .orElse { if (method == GET || method == DELETE) Some(`Content-Type`(WILDCARD)) else None }
                      .toDirective(
                        HttpProblem
                          .unsupportedMediaType(
                            "Content-Type missing",
                            mediaRangeMap.supportedMediaRanges
                          )
                          .toDirectiveFailure
                      )
      a2rdf <- Directive.commit {
                mediaRangeMap.get(contentType.mediaType).toDirective { supportedMediaTypes =>
                  Directive.error(
                    HttpProblem
                      .unsupportedMediaType(s"Content-Type '${contentType.mediaType}' invalid or unsupported mediatype",
                                            supportedMediaTypes)
                      .toResponse
                  )
                }
              }

    } yield {
      a2rdf
    }
  }

  def matchAcceptContentType[A](responseAlternatives: NonEmptyList[ResponseAlternativeForContent[A]]): Directive[F, A] = {
    NonEmptyList
      .fromList(
        responseAlternatives.collect{ case ResponseAlternativeForContent(Some(responseContentType), a ) => responseContentType.mediaType -> a }
      )
      .map[Directive[F, A]]{ contentTypeResponses =>
        Directive.request[F].flatMap{ req =>
          req.headers.get(Accept).map(_.values.toList) match {
            case Some(accepts) =>
              ContentNegotiation.accept(accepts, contentTypeResponses) match {
                case Some(a) => Directive.pure(a)
                case None    => Directive.error(
                                HttpProblem
                                    .notAcceptable(s"Accept '${accepts.mkString("[", ", ", "]")}' unsupported mediatype", contentTypeResponses.map(_._1).toList.toSet)
                                    .toResponse
                                )
              }
            case None =>
              req.headers.find(_.name == Accept.name)
                .map(accepts =>
                  Directive.failure[F, A](
                     HttpProblem
                         .notAcceptable(s"Accept '${accepts.value}' invalid mediatype", contentTypeResponses.map(_._1).toList.toSet)
                         .toResponse
                     )
                )
                .getOrElse(Directive.pure(responseAlternatives.head.value))

          }
        }
      }
      .getOrElse(Directive.pure(responseAlternatives.head.value))
  }

}
