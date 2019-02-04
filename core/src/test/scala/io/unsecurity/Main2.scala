package io.unsecurity

import cats.effect.{ExitCode, IO, IOApp}
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Directive
import org.http4s.Method

import scala.concurrent.ExecutionContext.Implicits.global

object Main2 extends IOApp {
  val unsecurity2: Unsecurity2[IO, String, String] = new Unsecurity2[IO, String, String] {
    override def sc: SecurityContext[IO, String, String] = ???
  }
  import unsecurity2._

  val server: Server[IO] = Server[IO](
    port = 8088,
    host = "0.0.0.0"
  )

  val helloWorld: unsecurity2.Complete =
    unsecure(
      Endpoint(
        method = Method.GET,
        path = Root / "hello",
        produces = Produces.json[String]
      )
    ).run { _ =>
      Directive.success("Hello world")
    }

  val collidingHello =
    unsecure(
      Endpoint(
        Method.GET,
        Root / param[String]("collideWithHello"),
        Produces.json[String]
      )
    ).run {
      case (collide ::: HNil, _) =>
        Directive.success(
          s"Hello, $collide"
        )
    }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    val httpRoutes = toHttpRoutes(
      List(
        helloWorld,
        collidingHello
      ))

    server
      .serve(httpRoutes)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
