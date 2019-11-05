package io.unsecurity

import java.util.UUID

import cats.MonadError
import cats.implicits._
import no.scalabin.http4s.directives.Directive
import org.http4s.{Query, Request, Response, Status, Uri}
import org.slf4j.Logger

case class UnsecurityPlan[F[_]](log: Logger)(implicit M: MonadError[F, Throwable]) {
  type Intent = PartialFunction[Request[F], F[Response[F]]]

  def task(pf: PartialFunction[Request[F], Directive[F, Response[F]]]): Intent = {
    case req if pf.isDefinedAt(req) =>
      pf(req)
        .run(req)
        .map { value =>
          if (!value.response.status.isSuccess) {
            val uriWithoutQueryParams = req.uri.copy(query = Query.empty).renderString
            log.warn(
              s"$uriWithoutQueryParams returned status code [${value.response.status.code.toString}]"
            )
          }

          value.response
        }
  }

  case class Mapping[X](from: Request[F] => X) {
    def apply(intent: PartialFunction[X, Directive[F, Response[F]]]): Intent = task {
      case req if intent.isDefinedAt(from(req)) => intent(from(req))
    }
  }

  lazy val PathMapping: Mapping[Uri.Path] = Mapping[Uri.Path](r => r.uri.path)
}
