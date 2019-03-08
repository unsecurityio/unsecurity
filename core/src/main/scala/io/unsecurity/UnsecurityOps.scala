package io.unsecurity

import java.net.URI

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Sync
import fs2.Stream
import io.circe.{Encoder, Printer}
import io.unsecurity.hlinx.ParamConverter
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, DirectiveOps, RequestDirectives}
import org.http4s.circe.CirceInstances
import org.http4s.headers.{Location, `WWW-Authenticate`}
import org.http4s.{Challenge, EntityEncoder, RequestCookie, Response, Status, Uri}

import scala.language.{higherKinds, implicitConversions}
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
    def toSuccess(failure: Throwable => Directive[F, A]): Directive[F, A] = {
      t.toEither.toSuccess(failure)
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
    request.cookie(cookieName).flatMap(opt => opt.toSuccess(BadRequest(s"Cookie '$cookieName' not found in request")))
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
                             (s: String) => Response[F](Status.BadRequest).withEntity(s)
                           ).map(Some(_))
                       }
    } yield {
      convertedParam
    }
  }

  def requiredQueryParam(name: String)(implicit syncEvidence: Sync[F]): Directive[F, String] = {
    request.queryParam(name).flatMap(_.toSuccess(BadRequest(s"Missing parameter $name")))
  }

  def requiredQueryParamAs[A: ParamConverter](name: String)(implicit sync: Sync[F]): Directive[F, A] = {
    for {
      paramValue <- requiredQueryParam(name)
      convertedParam <- eitherToDirective(
                         ParamConverter[A].convert(paramValue),
                         (s: String) => Response[F](Status.BadRequest).withEntity(s)
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

  def BadRequest[A: Encoder, B](a: A)(implicit sync: Sync[F]): Directive[F, B] = {
    Directive.error(
      responses.ResponseJson(a, Status.BadRequest)
    )
  }

  def NotFound[A](implicit sync: Sync[F]): Directive[F, A] = {
    Directive.error(
      Response[F](Status.NotFound)
    )
  }

  def Unauthorized[A: Encoder, B](a: A)(implicit sync: Sync[F]): Directive[F, B] = {
    Directive.error(responses.unauthorizedResponse(a))
  }

  def Forbidden[A](implicit syncEvidence: Sync[F]): Directive[F, A] = {
    Directive.error(
      Response[F](Status.Forbidden)
    )
  }

  def InternalServerError[A: Encoder, B](a: A)(implicit syncEvidence: Sync[F]): Directive[F, B] = {
    Directive.error(
      responses.ResponseJson(a, Status.InternalServerError)
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
        preserveOrder = true,
        dropNullValues = true
      )
    )
    .build

  import circeInstances._

  object ResponseJson {
    def apply[A: Encoder](value: A, status: Status)(implicit monad: Monad[F]): Response[F] =
      Response[F](status)
        .withEntity(value)(jsonEncoderOf[F, A])
  }

  def unauthorizedResponse[A: Encoder](a: A)(implicit sync: Sync[F]): Response[F] = {
    ResponseJson(a, Status.Unauthorized)
      .putHeaders(`WWW-Authenticate`(NonEmptyList(Challenge("Cookie", "klaveness"), Nil))) //TODO: Parameteriser cookie. m2m ???. diskuter med erlend
  }

}
