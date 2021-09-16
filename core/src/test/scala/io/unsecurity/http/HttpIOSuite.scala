package io.unsecurity.http

import java.net.ServerSocket

import cats.effect.IO
import io.unsecurity.{SecurityContext, Server, Unsecurity, UnsecurityOps}
import no.scalabin.http4s.directives.Directive
import org.http4s.HttpRoutes
import org.http4s.client.Client
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.server.Server
import org.http4s.blaze.server.BlazeServerBuilder

import scala.concurrent.duration._
import org.http4s.implicits._
import cats.implicits._

class HttpIOSuite extends IOSuite {

  def availablePort: Int = {
    val server = new ServerSocket(0)
    val port   = server.getLocalPort
    server.close()
    port
  }

  val unsecurity: Unsecurity[IO, String, String] = new Unsecurity[IO, String, String] {
    override def sc: SecurityContext[IO, String, String] = new SecurityContext[IO, String, String] with UnsecurityOps[IO] {
      override def authenticate: Directive[IO, String] = {
        for {
          user <- request.header("User")
          d    <- user.filter(_.value == "valid").map(s => Directive.success[IO, String](s.value)).getOrElse(Forbidden[String])
        } yield d
      }

      override def xsrfCheck: Directive[IO, String] = ???

      override def transformUser(rawUser: String): IO[Option[String]] = IO(Some(rawUser))
    }
  }
  import unsecurity._

  val port: Int = availablePort

  val httpClient: Fixture[Client[IO]] = suiteResourceFixture(
    BlazeClientBuilder[IO](scala.concurrent.ExecutionContext.global)
      .withResponseHeaderTimeout(40.seconds)
      .withRequestTimeout(45.seconds)
      .withIdleTimeout(60.seconds)
      .resource,
    "client"
  )


  def server(httpRoute: HttpRoutes[IO], routes: Complete*): Fixture[Server[IO]] = {
    server(
      Server
        .toHttpRoutes(
          routes: _*
        ) <+> httpRoute)
  }

  def server(httpRoute: HttpRoutes[IO]): Fixture[Server[IO]] = suiteResourceFixture(
    BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global)
      .bindHttp(port, "0.0.0.0")
      .withHttpApp(httpRoute.orNotFound)
      .resource,
    "server"
  )

  def server(routes: Complete*): Fixture[Server[IO]] =
    server(
      Server
        .toHttpRoutes(
          routes: _*
        ))
}
