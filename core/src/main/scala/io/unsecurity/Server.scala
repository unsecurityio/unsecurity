package io.unsecurity

import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, Timer}
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext
import org.http4s.implicits._

case class Server[F[_]](port: Int, host: String)(implicit eff: ConcurrentEffect[F],
                                                 cs: ContextShift[F],
                                                 timer: Timer[F],
                                                 globalEC: ExecutionContext) {

  def serve(routes: HttpRoutes[F]): fs2.Stream[F, ExitCode] = {

    val httpApp = Router("/" -> routes).orNotFound

    for {
      _ <- BlazeServerBuilder[F]
            .bindHttp(port, host)
            .enableHttp2(false)
            .withWebSockets(false)
            .withExecutionContext(globalEC)
            .withNio2(true)
            .withConnectorPoolSize(4)
            .withHttpApp(httpApp)
            .serve
    } yield ExitCode.Success
  }
}
