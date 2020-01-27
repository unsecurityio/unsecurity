package io.unsecurity

import cats.data.OptionT
import cats.effect.{ConcurrentEffect, ExitCode, Sync, Timer}
import io.unsecurity.hlinx.SimpleLinx
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, Request, Response, Status}
import org.log4s.getLogger
import cats.implicits._

import scala.Ordering.Implicits._
import scala.concurrent.ExecutionContext

class Server[F[_]: ConcurrentEffect: Timer](port: Int, host: String, httpExecutionContext: ExecutionContext) {

  def serve[U](endpoints: AbstractUnsecurity[F, U]#Complete*): fs2.Stream[F, ExitCode] = {
    serve(Server.toHttpRoutes(endpoints.toList))
  }

  def serve[U](endpoints: List[AbstractUnsecurity[F, U]#Complete]): fs2.Stream[F, ExitCode] = {
    serve(Server.toHttpRoutes(endpoints))
  }

  def serve(routes: HttpRoutes[F]): fs2.Stream[F, ExitCode] = {

    val httpApp = Server.httpProblemMiddleware(routes).orNotFound

    for {
      _ <- BlazeServerBuilder[F]
            .bindHttp(port, host)
            .enableHttp2(false)
            .withWebSockets(false)
            .withExecutionContext(httpExecutionContext)
            .withNio2(true)
            .withConnectorPoolSize(4)
            .withHttpApp(httpApp)
            .serve
    } yield ExitCode.Success
  }

}

object Server {

  private[this] val log = getLogger

  def httpProblemMiddleware[F[_]](service: HttpRoutes[F])(implicit F: Sync[F]): HttpRoutes[F] = HttpRoutes {
    req: Request[F] =>
      service
        .run(req)
        .map {
          case resp @ (Status.ClientError(_) | Status.ServerError(_)) =>
            val loggedResp = resp.withEntity(
              resp.bodyAsText.evalTap(body => F.delay(log.error(s"Error processing: ${req.pathInfo}, message: $body")))
            )
            resp.contentType.fold(loggedResp)(ct => loggedResp.putHeaders(ct))
          case resp =>
            resp
        }
        .handleErrorWith { t =>
          OptionT.liftF(F.delay {
            val problem = HttpProblem.internalServerError("Unhandled internal server error")
            log.error(t)(s"Error processing [${req.pathInfo}] id [${problem.uuid}]")
            problem.toResponse[F]
          })
        }
  }

  def toHttpRoutes[U, F[_]](endpoints: List[AbstractUnsecurity[F, U]#Complete])(
      implicit eff: ConcurrentEffect[F]): HttpRoutes[F] = {
    type PathMatcher[A] = PartialFunction[String, Http4sDirective[F, A]]

    val linxesToList: Map[List[SimpleLinx], List[AbstractUnsecurity[F, U]#Complete]] = endpoints.groupBy(_.key)

    val mergedRoutes: List[AbstractUnsecurity[F, U]#Complete] =
      linxesToList.toList
        .map {
          case (_, groupedEndpoints) => groupedEndpoints.reduce(_ merge _)
        }
        .sortBy(_.key)

    log.trace("Ordered and grouped endpoints:")
    mergedRoutes.foreach { r =>
      log.info(
        s"""/${r.key.mkString("/")}: ${r.methodMap.keys.map { _.name }.mkString(", ")}"""
      )
    }

    val compiledRoutes: List[PathMatcher[Response[F]]] =
      mergedRoutes.map(_.compile)

    val reducedRoutes: PathMatcher[Response[F]] = compiledRoutes.reduce(_ orElse _)

    val PathMapping = new UnsecurityPlan[F].PathMapping

    val service: HttpRoutes[F] = HttpRoutes.of[F](
      PathMapping(reducedRoutes)
    )

    service
  }
}
