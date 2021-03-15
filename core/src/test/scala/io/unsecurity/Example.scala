package io.unsecurity

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.Decoder
import io.unsecurity.hlinx.HLinx._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.{MediaType, Method}
import org.http4s.implicits._

object Example extends IOApp {
  val unsecurity: Unsecurity[IO, String, String] = new Unsecurity[IO, String, String] {
    override def sc: SecurityContext[IO, String, String] = ???
  }
  import unsecurity._

  val helloWorld: Complete =
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

  val collidingHello: Complete =
    unsecure(
      Endpoint(
        "Endpoint that collides with hello-world-endpoint, showing that static fragments gets precedense over variable fragments",
        Method.GET,
        Root / "collideWithHello".as[String],
        Produces.json[String]
      )
    ).run { collide =>
      s"Hello, $collide"
    }

  val queryParamCodec: Complete =
    unsecure(
      Endpoint(
        "Show how to use query params outside dsl using Directives",
        Method.GET,
        Root / "decode" / "pathParam".as[String] / "queryparam",
        Produces.Directive.json[String]
      )
    ).run { pathParam =>
      for {
        qp <- requiredQueryParamAs[String]("qp")
      } yield {
        println(pathParam)
        println(qp)
        pathParam + qp
      }
    }

  val pathParamAndQueryParamCodec: Complete =
    unsecure(
      Endpoint(
        "Show how to use query params with the dsl",
        Method.GET,
        Root / "decode" / "pathParam".as[String] / "pathparamandqueryparam" :? "qp".as[String] & "qps".as[String].* & "qpOpt".as[String].?,
        Produces.json[String]
      )
    ).run {
      case (pathParam: String, qp: String, qps: Seq[String], qpOpt: Option[String]) =>
        println(pathParam)
        println(qp)
        println(qps)
        println(qpOpt)
        pathParam + qp + qps + qpOpt
    }

  case class StringAndInt(s: String, i: Int)

  val twoParams: Complete =
    unsecure(
      Endpoint(
        "endpoint taking two path params",
        Method.GET,
        Root / "param1".as[String] / "param2".as[Int],
        Produces.json[String]
      )
    ).map { case (s, i) => StringAndInt(s, i) }
      .run { sai =>
        sai.toString
      }

  case class ClientPayload(name: String)
  implicit val clientPayloadDecoder: Decoder[ClientPayload] = Decoder.forProduct1("name")(ClientPayload)

  val post: Complete =
    unsecure(
      Endpoint(
        "Check if a post request carshes if x09 is sent",
        Method.POST,
        Root / "does" / "this" / "work",
        Consumes.json[ClientPayload],
        Produces.json[String]
      )
    ).run { body =>
      println(body)
      "OK"
    }

  val post2: Complete =
    unsecure(
      Endpoint(
        "Check if a post request crashes if x09 is sent",
        Method.POST,
        Root / "does" / "this" / "work",
        Consumes.jsonWithMediaType[ClientPayload](
          MediaType.parse("application/clientpayload").getOrElse(throw new RuntimeException("could not parse media range"))),
        Produces.json[String]
      )
    ).run { body =>
      println(body)
      "Some Reply"
    }

  override def run(args: List[String]): IO[ExitCode] = {

    BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.Implicits.global)
      .bindHttp(8088, "0.0.0.0")
      .enableHttp2(false)
      .withWebSockets(true)
      .withHttpApp(
        Server
          .toHttpRoutes(
            List(
              helloWorld,
              collidingHello,
              pathParamAndQueryParamCodec,
              queryParamCodec,
              twoParams,
              post,
              post2
            )
          )
          .orNotFound
      )
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
