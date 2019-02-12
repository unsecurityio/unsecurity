package io.unsecurity

import java.net.{URI, URISyntaxException}

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Sync
import fs2.Stream
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Printer}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, DirectiveOps, RequestDirectives}
import org.http4s.circe.CirceInstances
import org.http4s.headers.{Location, `Content-Type`, `WWW-Authenticate`}
import org.http4s.util.CaseInsensitiveString
import org.http4s.{Challenge, EntityEncoder, Header, MediaType, RequestCookie, Response, Status, Uri}

import scala.language.{higherKinds, implicitConversions}
import scala.util.Try

trait UnsecurityOps[F[_]] extends DirectiveOps[F] with RequestDirectives[F] {

  val circeInstances: CirceInstances = CirceInstances.withPrinter(
    Printer.spaces2.copy(
      preserveOrder = true,
      dropNullValues = true
    )
  )
  import circeInstances._

  implicit class OptionDirectives[A](opt: Option[A]) {
    def toSuccess[S[_]: Sync](failure: Directive[S, A]): Directive[S, A] = {
      opt match {
        case Some(a) => Directive.success(a)
        case None    => failure
      }
    }
  }

  implicit class EitherDirectives[E, A](either: Either[E, A]) {
    def toSuccess[S[_]: Sync](failure: E => Directive[S, A]): Directive[S, A] = {
      either match {
        case Right(a)   => Directive.success(a)
        case Left(left) => failure(left)
      }
    }
  }

  implicit class TryDirectives[A](t: Try[A]) {
    def toSuccess[S[_]: Sync](failure: Throwable => Directive[S, A]): Directive[S, A] = {
      t.toEither.toSuccess(failure)
    }
  }

  implicit class BooleanDirectives(b: Boolean) {
    def toSuccess[S[_]: Sync](failure: Directive[S, Boolean]): Directive[S, Boolean] = {
      if (b) {
        Directive.success(b)
      } else {
        failure
      }
    }
  }

  def asUri(opt: Option[String])(implicit syncEvidence: Sync[F]): Directive[F, Option[URI]] = {
    try {
      opt match {
        case Some(a) => Directive.success(Some(URI.create(a)))
        case None    => Directive.success(None)
      }
    } catch {
      case _: URISyntaxException =>
        BadRequest(s"Malformed URL: ${opt.getOrElse("empty")}")
    }
  }

  implicit def responseAsDirective(rf: Response[F])(implicit syncEvidence: Sync[F]): Directive[F, Response[F]] = {
    Directive.success(rf)
  }

  implicit def liftResponse(rf: Response[F])(implicit syncEvidence: Sync[F]): F[Response[F]] = {
    syncEvidence.delay(rf)
  }

  def cookie(cookieName: String)(implicit syncEvidence: Sync[F]): Directive[F, RequestCookie] = {
    request.cookie(cookieName).flatMap(opt => opt.toSuccess(BadRequest(s"Cookie '$cookieName' not found in request")))
  }

  def decodeBody[X](implicit decoder: Decoder[X], syncEvidence: Sync[F]): Directive[F, X] = {
    request.bodyAs[F, X](syncEvidence, jsonOf[F, X])
  }

  def requestCookies()(implicit syncEvidence: Sync[F]): Directive[F, List[RequestCookie]] = {
    request.cookies.map(maybeCookies => maybeCookies.toList.flatMap(cookies => cookies.toList))
  }

  def requestHeader(name: String)(implicit syncEvidence: Sync[F]): Directive[F, Option[Header]] =
    request.headers.map(_.get(CaseInsensitiveString(name)))

  def queryParam(name: String)(implicit syncEvidence: Sync[F]): Directive[F, Option[String]] = {
    request.queryParam(name)
  }

  def requiredQueryParam(name: String)(implicit syncEvidence: Sync[F]): Directive[F, String] = {
    request.queryParam(name).flatMap(_.toSuccess(BadRequest(s"Missing parameter $name")))
  }

  object ResponseJson {
    def apply[A: Encoder](value: A, status: Status)(implicit monad: Monad[F]): Response[F] =
      Response[F](status)
        .withContentType(`Content-Type`(MediaType.application.json))
        .withEntity(value.asJson)
  }

  object ResponseCsvFile {
    def apply(value: String, status: Status)(implicit monad: Monad[F]): Response[F] =
      Response[F](status)
        .withContentType(`Content-Type`(MediaType.text.csv))
        .withEntity(value)
  }

  def Redirect(uri: Uri)(implicit syncEvidence: Sync[F]): Response[F] = {
    Response(Status.Found).putHeaders(Location(uri))
  }

  def Redirect(uri: String)(implicit syncEvidence: Sync[F]): Response[F] = Redirect(Uri.unsafeFromString(uri))

  def Redirect(uri: URI)(implicit syncEvidence: Sync[F]): Response[F] = Redirect(Uri.unsafeFromString(uri.toString))

  def BadRequest[A: Encoder, B](a: A)(implicit syncEvidence: Sync[F]): Directive[F, B] = {
    Directive.error(ResponseJson(a, Status.BadRequest))
  }

  def NotFound[A](implicit syncEvidence: Sync[F]): Directive[F, A] = {
    Directive.failure(ResponseJson("", Status.NotFound))
  }

  def Unauthorized[A: Encoder, B](a: A)(implicit syncEvidence: Sync[F]): Directive[F, B] = {
    Directive.error(
      Response[F](Status.Unauthorized)
        .withType(MediaType.application.json)
        .putHeaders(`WWW-Authenticate`(NonEmptyList(Challenge("Cookie", "klaveness"), Nil))) //TODO: Parameteriser cookie. m2m ???. diskuter med erlend
        .withEntity(a.asJson)
    )
  }

  def Forbidden[A](implicit syncEvidence: Sync[F]): Directive[F, A] = {
    Directive.error(
      Response[F](Status.Forbidden)
        .withType(MediaType.application.json)
        .putHeaders(`WWW-Authenticate`(NonEmptyList(Challenge("Cookie", "klaveness"), Nil))) //TODO: Parameteriser cookie
        .withEntity("".asJson)
    )
  }

  def InternalServerError[A: Encoder, B](a: A)(implicit syncEvidence: Sync[F]): Directive[F, B] = {
    Directive.error(
      ResponseJson(a, Status.InternalServerError)
    )
  }

  def Ok[A: Encoder](a: A)(implicit syncEvidence: Sync[F]): ResponseDirective[F] = {
    Directive.success(
      ResponseJson(a, Status.Ok)
    )
  }

  def Accepted[A: Encoder](a: A)(implicit syncEvidence: Sync[F]): ResponseDirective[F] = {
    Directive.success(
      ResponseJson(a, Status.Accepted)
    )
  }

  def CsvFile(a: String)(implicit syncEvidence: Sync[F]): ResponseDirective[F] = {
    Directive.success(
      ResponseCsvFile(a, Status.Ok)
    )
  }

  object StreamResponse {
    def apply[A](stream: Stream[F, A])(implicit bodyEncoder: EntityEncoder[F, Stream[F, A]], syncEvidence: Sync[F]): ResponseDirective[F] =
      Directive.success(
        Response[F](Status.Ok).withEntity(stream)
      )
  }
}
