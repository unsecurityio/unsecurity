package io.unsecurity

import cats.Monad
import cats.data.NonEmptyList
import no.scalabin.http4s.directives.Directive
import org.http4s.MediaType
import org.http4s.Method.{DELETE, GET}
import org.http4s.implicits._
import org.http4s.headers.{`Content-Type`, Accept}
import org.typelevel.ci.CIStringSyntax

abstract class AbstractContentTypeMatcher[F[_]: Monad] extends AbstractMethodMatcher[F] {

  val WILDCARD = mediaType"*/*"

  def matchRequestContentType[A](mediaRangeMap: MediaRangeMap[A]): Directive[F, NonEmptyList[ResponseAlternativeForContent[A]]] = {

    for
      request             <- Directive.request[F]
      method              = request.method
      suppliedContentType = request.headers.get[`Content-Type`]
      contentType <- suppliedContentType
                      .orElse {
                        if method == GET || method == DELETE then Some(`Content-Type`(WILDCARD)) else None
                      }
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
                      .unsupportedMediaType(s"Content-Type '${contentType.mediaType}' invalid or unsupported mediatype", supportedMediaTypes)
                      .toResponse
                  )
                }
              }
    yield a2rdf
  }

  def matchAcceptContentType[A](responseAlternatives: NonEmptyList[ResponseAlternativeForContent[A]]): Directive[F, A] = {
    NonEmptyList
      .fromList(
        responseAlternatives.collect {
          case ResponseAlternativeForContent(Some(ct), a) =>
            new MediaType(
              ct.mediaType.mainType.toLowerCase,
              ct.mediaType.subType.toLowerCase,
              ct.mediaType.compressible,
              ct.mediaType.binary,
              ct.mediaType.fileExtensions,
              ct.mediaType.extensions.map(kv => kv._1.toLowerCase -> kv._2.toLowerCase)
            ) -> a
        }
      )
      .map[Directive[F, A]] { consumes =>
        Directive.request[F].flatMap { req =>
          req.headers.get[Accept].map(_.values.toList) match {
            case Some(accepts) =>
              ContentNegotiation.accept(accepts, consumes) match {
                case Some(a) => Directive.pure(a)
                case None =>
                  Directive.error(
                    HttpProblem
                      .notAcceptable(s"Accept '${accepts.mkString("[", ", ", "]")}' unsupported mediatype", consumes.map(_._1).toList.toSet)
                      .toResponse
                  )
              }
            case None =>
              req.headers.headers
                .find(_.name == ci"Accept")
                .map { accepts =>
                  Directive.failure[F, A](
                    HttpProblem
                      .notAcceptable(s"Accept '${accepts.value}' invalid mediatype", consumes.map(_._1).toList.toSet)
                      .toResponse
                  )
                }
                .getOrElse(Directive.pure(responseAlternatives.head.value))
          }
        }
      }
      .getOrElse(Directive.pure(responseAlternatives.head.value))
  }
}
