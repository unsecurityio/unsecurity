package io.unsecurity

import java.util.UUID

import cats.{Applicative, Monad}
import io.circe._
import io.circe.syntax._
import no.scalabin.http4s.directives.Directive
import org.http4s.circe.DecodingFailures
import org.http4s.{DecodeFailure, InvalidMessageBodyFailure, MalformedMessageBodyFailure, MediaType, Method, Response, Status}

import scala.util.control.NonFatal

// https://tools.ietf.org/html/rfc7807

//TODDO: Add logging and assign id to errors.. to make it easier to find the error in logs later
case class HttpProblem(status: Status, title: String, detail: Option[String], data: Option[Json], uuid:String = UUID.randomUUID().toString) extends RuntimeException {
  def toResponse[F[_]: Applicative]: Response[F] =
    Response[F](status)
      .withContentType(
        org.http4s.headers.`Content-Type`(MediaType.application.`problem+json`)
      )
      .withEntity(this)(org.http4s.circe.jsonEncoderOf[F, HttpProblem])
  def toResponseF[F[_]: Applicative]: F[Response[F]] =
    Applicative[F].pure(toResponse)

  def toDirective[F[_]: Monad]: Directive[F, Response[F]] =
    Directive.successF(toResponseF[F])

  def toDirectiveError[F[_]: Monad, A] : Directive[F, A] = Directive.errorF(toResponseF[F])
}

object HttpProblem {
  implicit val encoder: Encoder[HttpProblem] = Encoder.instance(
    p =>
      Json.obj(
        "type" := "about:blank",
        "title" := p.title,
        "status" := p.status.code,
        "detail" := p.detail,
        "data" := p.data,
        "errorId" := p.uuid,
      )
  )

  def methodNotAllowed(title:String, allowedMethods:Set[Method]) =
    HttpProblem(Status.MethodNotAllowed, title, None, Some(Json.arr(allowedMethods.map(m => Json.fromString(m.name)).toSeq:_*)))

  def badRequest(title: String, detail: Option[String] = None) =
    HttpProblem(Status.BadRequest, title, detail, None)

  def forbidden(title: String, detail: Option[String] = None, data: Option[Json] = None) =
    HttpProblem(Status.Forbidden, title, detail, data)

  def unauthorized(title: String, detail: Option[String] = None) =
    HttpProblem(Status.Unauthorized, title, detail, None)

  def internalServerError(title: String, detail: Option[String] = None, data:Option[Json] = None) =
    HttpProblem(Status.InternalServerError, title, detail, None)

  def notFound = HttpProblem(Status.NotFound, "Not Found", None, None)

  def decodingFailure(failure: DecodingFailure) =
    Json.obj(
      "path" := CursorOp.opsToPath(failure.history),
      "message" := failure.message
    )

  def handleError: PartialFunction[Throwable, HttpProblem] = {
    case h: HttpProblem => h
    case InvalidMessageBodyFailure(details, Some(failure: DecodingFailure)) =>
      HttpProblem(
        Status.BadRequest,
        "Could not decode JSON1",
        Some(details),
        Some(Json.arr(decodingFailure(failure)))
      )
    case MalformedMessageBodyFailure(details, Some(DecodingFailures(failures))) =>
      HttpProblem(
        Status.BadRequest,
        "Could not decode JSON2",
        Some(details), Some(failures.map(decodingFailure).asJson)
      )
    case d: DecodeFailure => HttpProblem(
      Status.BadRequest,
      "Could not decode JSON3",
      Some(d.message), None
    )
    case NonFatal(e) => HttpProblem.internalServerError("Internal server error", Option(e.getMessage))
  }
}
