package io.unsecurity

import cats.data.OptionT
import cats.effect.{ConcurrentEffect, ExitCode, Resource, Sync, Timer}
import cats.implicits._
import io.unsecurity.hlinx.SimpleLinx
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{server, HttpRoutes, Request, Response, Status}
import org.log4s.getLogger
import fs2.Stream

import scala.Ordering.Implicits._
import scala.concurrent.ExecutionContext

@deprecated("Will be removed. It is better that the client controls the server creation", since = "2.0.6")
class Server[F[_]: ConcurrentEffect: Timer](port: Int, host: String, httpExecutionContext: ExecutionContext) {

  def serve(endpoints: Complete[F]*): Stream[F, ExitCode] =
    serve(Server.toHttpRoutes(endpoints.toList))

  def serve(endpoints: List[Complete[F]]): Stream[F, ExitCode] =
    serve(Server.toHttpRoutes(endpoints))

  def serve(routes: HttpRoutes[F]): fs2.Stream[F, ExitCode] = blazeServer(routes).serve

  def resource(routes: HttpRoutes[F]): Resource[F, server.Server[F]] = blazeServer(routes).resource

  private def blazeServer(routes: HttpRoutes[F]): BlazeServerBuilder[F] = {
    BlazeServerBuilder[F](httpExecutionContext)
      .bindHttp(port, host)
      .enableHttp2(false)
      .withWebSockets(true)
      .withNio2(true)
      .withHttpApp(Server.httpProblemMiddleware(routes).orNotFound)
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
            val problem = HttpProblem.handleError(t)
            log.error(t)(s"Error processing [${req.pathInfo}] id [${problem.uuid}]")
            problem.toResponse[F]
          })
        }
  }

  def toHttpRoutes[U, F[_]: Sync](endpoints: Complete[F]*): HttpRoutes[F] =
    toHttpRoutes(endpoints.toList)

  def toHttpRoutes[U, F[_]: Sync](endpoints: List[Complete[F]]): HttpRoutes[F] = {
    type PathMatcher[A] = PartialFunction[String, Http4sDirective[F, A]]

    val linxesToList: Map[List[SimpleLinx], List[Complete[F]]] = endpoints.groupBy(_.key)

    val mergedRoutes: List[Complete[F]] =
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
