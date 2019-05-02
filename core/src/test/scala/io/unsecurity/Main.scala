package io.unsecurity

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.Decoder
import io.unsecurity.hlinx.HLinx._
import org.http4s.Method
import org.slf4j.{Logger, LoggerFactory}

object Main extends IOApp {
  val unsecurity: Unsecurity[IO, String, String] = new Unsecurity[IO, String, String] {
    override def sc: SecurityContext[IO, String, String] = ???
    override def log: Logger                             = LoggerFactory.getLogger("fjon")
  }
  import unsecurity._

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
    ).map { case (s, i) => StringAndInt(s, i) }
      .run { sai =>
        sai.toString
      }

  case class Fjon(name: String)
  implicit val fjonDecoder: Decoder[Fjon] = Decoder { c =>
    for {
      name <- c.downField("name").as[String]
    } yield {
      Fjon(name)
    }
  }

  val post =
    unsecure(
      Endpoint(
        "Check if a post request carshes if x09 is sent",
        Method.POST,
        Root / "does" / "this" / "work",
        Accepts.json[Fjon],
        Produces.json[String]
      )
    ).run {
      case body =>
        println(body)
        "OK"
    }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    Server(port = 8088, host = "0.0.0.0", httpExecutionContext = scala.concurrent.ExecutionContext.Implicits.global)
      .serve(
        List(
          helloWorld,
          collidingHello,
          twoParams,
          post
        ))
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
