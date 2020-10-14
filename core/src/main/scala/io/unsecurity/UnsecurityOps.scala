package io.unsecurity

import java.net.URI

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Sync
import fs2.Stream
import io.circe.{Encoder, Printer}
import io.unsecurity.hlinx.ParamConverter
import no.scalabin.http4s.directives.{Directive, DirectiveOps, RequestDirectives}
import org.http4s.circe.CirceInstances
import org.http4s.headers.{`WWW-Authenticate`, Location}
import org.http4s.{Challenge, EntityEncoder, RequestCookie, Response, Status, Uri}

import scala.language.implicitConversions
import scala.util.Try

trait UnsecurityOps[F[_]] extends DirectiveOps[F] with RequestDirectives[F] {

  def eitherToDirective[E, A](either: Either[E, A], failure: E => Response[F])(
      implicit F: Monad[F]): Directive[F, A] = {
    either match {
      case Right(a)   => Directive.success(a)
      case Left(left) => Directive.failure(failure(left))
    }
  }

  implicit class TryDirectives[A](t: Try[A])(implicit S: Sync[F]) {
    def toSuccess(failure: Throwable => Directive[F, Response[F]]): Directive[F, A] = {
      t.toEither.toDirective(failure)
    }
  }

  implicit class BooleanDirectives(b: Boolean)(implicit S: Sync[F]) {
    @deprecated("use directive filter", "8/3 2019")
    def toSuccess(failure: Directive[F, Boolean]): Directive[F, Boolean] = {
      if (b) {
        Directive.success(b)
      } else {
        failure
      }
    }
  }

  def cookie(cookieName: String)(implicit sync: Sync[F]): Directive[F, RequestCookie] = {
    request
      .cookie(cookieName)
      .flatMap(opt =>
        opt.toDirective(HttpProblem.badRequest(s"Cookie '$cookieName' not found in request").toDirectiveError))
  }

  def requestCookies()(implicit sync: Sync[F]): Directive[F, List[RequestCookie]] = {
    request.cookies.map(maybeCookies => maybeCookies.toList.flatMap(cookies => cookies.toList))
  }

  def queryParamAs[A: ParamConverter](name: String)(implicit sync: Sync[F]): Directive[F, Option[A]] = {
    for {
      optParamValue <- request.queryParam(name)
      convertedParam <- optParamValue match {
                         case None => Directive.success(None)
                         case Some(param) =>
                           eitherToDirective(
                             ParamConverter[A].convert(param),
                             (s: String) => HttpProblem.badRequest(s).toResponse
                           ).map(Some(_))
                       }
    } yield {
      convertedParam
    }
  }

  def requiredQueryParam(name: String)(implicit syncEvidence: Sync[F]): Directive[F, String] = {
    request.queryParam(name).flatMap(_.toDirective(BadRequest(s"Missing parameter $name")))
  }

  def requiredQueryParamAs[A: ParamConverter](name: String)(implicit sync: Sync[F]): Directive[F, A] = {
    for {
      paramValue <- requiredQueryParam(name)
      convertedParam <- eitherToDirective(
                         ParamConverter[A].convert(paramValue),
                         (s: String) => HttpProblem.badRequest(s).toResponse
                       )
    } yield {
      convertedParam
    }
  }

  def Redirect(uri: Uri)(implicit sync: Sync[F]): Response[F] = {
    Response(Status.Found).putHeaders(Location(uri))
  }

  def Redirect(uri: String)(implicit sync: Sync[F]): Response[F] = Redirect(Uri.unsafeFromString(uri))

  def Redirect(uri: URI)(implicit sync: Sync[F]): Response[F] = Redirect(Uri.unsafeFromString(uri.toString))

  def BadRequest[B](detail: String)(implicit sync: Sync[F]): Directive[F, B] = {
    HttpProblem.badRequest("Bad Request", Some(detail)).toDirectiveError[F, B]
  }

  def NotFound[A](implicit sync: Sync[F]): Directive[F, A] = {
    HttpProblem.notFound.toDirectiveError[F, A]
  }

  def Unauthorized[B](details: String)(implicit sync: Sync[F]): Directive[F, B] = {
    Directive.error(responses.unauthorizedResponse(Some(details)))
  }

  def Forbidden[A](implicit syncEvidence: Sync[F]): Directive[F, A] = {
    Directive.error(
      HttpProblem.forbidden("Forbidden").toResponse
    )
  }

  def InternalServerError[B](title: String, detail: Option[String] = None)(
      implicit syncEvidence: Sync[F]): Directive[F, B] = {
    Directive.error(
      HttpProblem.internalServerError(title, detail).toResponse
    )
  }

  def Ok[A: Encoder](a: A)(implicit syncEvidence: Sync[F]): ResponseDirective[F] = {
    Directive.success(
      responses.ResponseJson(a, Status.Ok)
    )
  }

  def Accepted[A: Encoder](a: A)(implicit syncEvidence: Sync[F]): ResponseDirective[F] = {
    Directive.success(
      responses.ResponseJson(a, Status.Accepted)
    )
  }

  object StreamResponse {
    def apply[A](stream: Stream[F, A])(implicit bodyEncoder: EntityEncoder[F, Stream[F, A]],
                                       syncEvidence: Sync[F]): ResponseDirective[F] =
      Directive.success(
        Response[F](Status.Ok).withEntity(stream)
      )
  }

  object responses extends Responses[F]
}

trait Responses[F[_]] {
  val circeInstances: CirceInstances = CirceInstances
    .withPrinter(
      Printer.spaces2.copy(
        dropNullValues = true
      )
    )
    .build

  import circeInstances._

  object ResponseJson {
    def apply[A: Encoder](value: A, status: Status)(implicit monad: Monad[F]): Response[F] =
      Response[F](status)
        .withEntity(value)(jsonEncoderOf[F, A])
    def apply[A: Encoder](status: Status, value: A)(implicit monad: Monad[F]): Response[F] =
      Response[F](status)
        .withEntity(value)(jsonEncoderOf[F, A])
  }

  def unauthorizedResponse(details: Option[String])(implicit sync: Sync[F]): Response[F] = {
    HttpProblem
      .unauthorized("Unauthorized", details)
      .toResponse
      .putHeaders(`WWW-Authenticate`(NonEmptyList(Challenge("Cookie", "session cookie"), Nil)))
  }

}
