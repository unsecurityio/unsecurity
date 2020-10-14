package io.unsecurity

import java.util.UUID

import cats.Monad
import io.circe._
import io.circe.syntax._
import no.scalabin.http4s.directives.Directive
import org.http4s.circe.DecodingFailures
import org.http4s._
import org.log4s.getLogger

import scala.util.Success
import scala.util.control.NonFatal

// https://tools.ietf.org/html/rfc7807

case class HttpProblem(status: Status,
                       title: String,
                       detail: Option[String],
                       data: Option[Json],
                       uuid: String = UUID.randomUUID().toString,
                       cause: Option[Throwable] = None)
    extends RuntimeException(detail.getOrElse(title).concat(s", uuid: $uuid"), cause.orNull) {

  def toResponse[F[_]: Monad]: Response[F] =
    Response[F](status)
      .withEntity(this)(org.http4s.circe.jsonEncoderOf[F, HttpProblem])
      .withContentType(org.http4s.headers.`Content-Type`(MediaType.application.`problem+json`))

  def toResponseF[F[_]: Monad]: F[Response[F]] =
    Monad[F].pure(toResponse)

  def toDirective[F[_]: Monad]: Directive[F, Response[F]] =
    Directive.successF(toResponseF[F])

  def toDirectiveError[F[_]: Monad, A]: Directive[F, A] = Directive.errorF(toResponseF[F])

  def toDirectiveFailure[F[_]: Monad, A]: Directive[F, A] = Directive.failureF(toResponseF[F])
}

object HttpProblem {

  private[this] val log = getLogger

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

  implicit val httpProblemDecoder: Decoder[HttpProblem] = (c: HCursor) => {
    for {
      title  <- c.downField("title").as[String]
      status <- c.downField("status").as[Status]
      detail <- c.downField("detail").as[Option[String]]
      data   <- c.downField("data").as[Option[Json]]
      uuid   <- c.downField("errorId").as[String]

    } yield HttpProblem(status, title, detail, data, uuid)

  }

  implicit val statusDecoder: Decoder[Status] =
    Decoder.decodeInt.emapTry(i => Status.fromInt(i).toTry.orElse(Success(Status.InternalServerError)))

  def methodNotAllowed(title: String, allowedMethods: Set[Method]) = HttpProblem(
    Status.MethodNotAllowed,
    title,
    None,
    Some(Json.arr(allowedMethods.map(m => Json.fromString(m.name)).toSeq: _*))
  )
  
  def badRequest(title: String, detail: Option[String] = None) =
    HttpProblem(Status.BadRequest, title, detail, None)

  def forbidden(title: String, detail: Option[String] = None, data: Option[Json] = None) =
    HttpProblem(Status.Forbidden, title, detail, data)

  def unauthorized(title: String, detail: Option[String] = None) =
    HttpProblem(Status.Unauthorized, title, detail, None)

  def internalServerError(title: String, detail: Option[String] = None, data: Option[Json] = None) =
    HttpProblem(Status.InternalServerError, title, detail, data)

  def notFound = HttpProblem(Status.NotFound, "Not Found", None, None)

  def unsupportedMediaType(detail: String, supportedRanges: Set[MediaRange]) =
    HttpProblem(
      Status.UnsupportedMediaType,
      "Unsupported Media-Type",
      Some(detail),
      Some(
        Json.obj(
          "supportedTypes" := Json.arr(supportedRanges.map(range => Json.fromString(range.toString)).toSeq: _*)
        )
      )
    )

  def notAcceptable(detail: String, responseMediaTypes: Set[MediaType]) =
    HttpProblem(
      Status.NotAcceptable,
      "Not Acceptable",
      Some(detail),
      Some(
        Json.obj(
          "responseMediaTypes" := Json.arr(
            responseMediaTypes.map(mediaType => Json.fromString(mediaType.toString)).toSeq: _*
          )
        )
      )
    )

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
        "Could not decode JSON",
        Some(details),
        Some(Json.arr(decodingFailure(failure)))
      )
    case MalformedMessageBodyFailure(details, Some(DecodingFailures(failures))) =>
      HttpProblem(
        Status.BadRequest,
        "Could not decode JSON",
        Some(details),
        Some(failures.map(decodingFailure).asJson)
      )
    case d: DecodeFailure =>
      HttpProblem(
        Status.BadRequest,
        "Could not decode JSON",
        Some(d.message),
        None
      )
    case NonFatal(e) =>
      HttpProblem(status = Status.InternalServerError,
                  title = "Internal server error",
                  detail = Option(e.getMessage),
                  data = None,
                  cause = Some(e))
  }
}
