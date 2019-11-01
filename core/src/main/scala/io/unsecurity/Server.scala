package io.unsecurity


import cats.Monad
import cats.effect.{ConcurrentEffect, ExitCode, IO, Timer}
import io.unsecurity.hlinx.SimpleLinx
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.http4s.implicits._
import org.http4s.server.{DefaultServiceErrorHandler, ServiceErrorHandler}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{HttpRoutes, Response}
import org.slf4j.{Logger, LoggerFactory}

import scala.Ordering.Implicits._
import scala.concurrent.ExecutionContext

class Server[F[_]](port: Int, host: String, httpExecutionContext: ExecutionContext, log: Logger)(
    implicit eff: ConcurrentEffect[F],
    timer: Timer[F]) {

  def serve[U](endpoints: AbstractUnsecurity[F, U]#Complete*): fs2.Stream[F, ExitCode] = {
    serve(Server.toHttpRoutes(endpoints.toList, log))
  }

  def serve[U](endpoints: List[AbstractUnsecurity[F, U]#Complete]): fs2.Stream[F, ExitCode] = {
    serve(Server.toHttpRoutes(endpoints, log))
  }

  def serve(routes: HttpRoutes[F]): fs2.Stream[F, ExitCode] = {

    val httpApp = routes.orNotFound

    for {
      _ <- BlazeServerBuilder[F]
            .bindHttp(port, host)
            .enableHttp2(false)
            .withWebSockets(false)
            .withExecutionContext(httpExecutionContext)
            .withNio2(true)
            .withConnectorPoolSize(4)
            .withHttpApp(httpApp)
            .withServiceErrorHandler(Server.httpProblemErrorHandler)
            .serve
    } yield ExitCode.Success
  }

  @deprecated("Use companion object Server.toHttpRoutes", "1.1")
  def toHttpRoutes[U](endpoints: List[AbstractUnsecurity[F, U]#Complete]): HttpRoutes[F] = {
    Server.toHttpRoutes(endpoints, log)
  }
}

object Server {

  private val log = LoggerFactory.getLogger(Server.getClass)

  def httpProblemErrorHandler[F[_] : Monad] : ServiceErrorHandler[F] = req => {
    HttpProblem.handleError
    .andThen{
      problem => {
        log.error(problem.toString, problem)
        problem
      }
    }
    .andThen(_.toResponseF[F]).orElse(DefaultServiceErrorHandler[F].apply(req))
  }


  def toHttpRoutes[U, F[_]](endpoints: List[AbstractUnsecurity[F, U]#Complete], log: Logger)(implicit eff: ConcurrentEffect[F]) : HttpRoutes[F] = {
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

    val PathMapping = UnsecurityPlan[F](log).PathMapping

    val service: HttpRoutes[F] = HttpRoutes.of[F](
      PathMapping(reducedRoutes)
    )

    service
  }
}