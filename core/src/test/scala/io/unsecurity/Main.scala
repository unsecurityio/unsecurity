package io.unsecurity

import cats.effect.{ExitCode, IO, IOApp}
import io.unsecurity.hlinx.HLinx._
import org.http4s.Method
import org.slf4j.Logger

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends IOApp {
  val unsecurity: Unsecurity[IO, String, String] = new Unsecurity[IO, String, String] {
    override def sc: SecurityContext[IO, String, String] = ???
    override def log: Logger = ???
  }
  import unsecurity._

  val server: Server[IO] = Server[IO](
    port = 8088,
    host = "0.0.0.0"
  )

  val helloWorld: unsecurity.Complete =
    unsecure(
      Endpoint(
        "Hello world; endpoint as simple as it gets",
        Method.GET,
        Root / "hello",
        Produces.json[String]
      )
    ).run { _ =>
      "Hello world"
    }

  val collidingHello =
    unsecure(
      Endpoint(
        "Endpoint that collides with hello-world-endpoint, showing that static fragments gets precedense over variable fragments",
        Method.GET,
        Root / 'collideWithHello.as[String],
        Produces.json[String]
      )
    ).run {
      case collide =>
        s"Hello, $collide"
    }

  case class StringAndInt(s: String, i: Int)

  val twoParams =
    unsecure(
      Endpoint(
        "endpoint taking two path params",
        Method.GET,
        Root / 'param1.as[String] / 'param2.as[Int],
        Produces.json[String]
      )
    ).resolve { case (s, i) => StringAndInt(s, i) }
      .run { sai =>
        sai.toString
      }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    val httpRoutes = toHttpRoutes(
      List(
        helloWorld,
        collidingHello,
        twoParams
      ))

    server
      .serve(httpRoutes)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
