package io.unsecurity.http

import cats.effect.IO
import io.circe.Json
import io.circe.syntax._
import io.unsecurity.hlinx.HLinx._
import org.http4s.circe._
import org.http4s.client.UnexpectedStatus
import org.http4s.server.Server
import org.http4s.{Method, Request, Status, Uri}

class PathTest extends HttpIOSuite {

  import unsecurity._

  def rootExpected(msg: String): Json = Json.obj {
    "msg" := msg
  }

  def counterExpected(counter: Int): Json = Json.obj {
    "counter" := counter
  }

  val collideRootService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "collide" / "root" ,
        Produces.json[Json]
      )
    ).run { _ =>
      rootExpected("hit the correct root")
    }

  val collidePathParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "collide" / "root".as[String] ,
        Produces.json[Json]
      )
    ).run { root =>
      rootExpected(root)
    }

  val singlePathParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "single-path-param" / "counter".as[Int],
        Produces.json[Json]
      )
    ).run { counter =>
      counterExpected(counter)
    }

  val server: Fixture[Server[IO]] = server(singlePathParamService, collidePathParamService, collideRootService)

  test("Static path segments should result in that to be chosen over path params on same level.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/collide/root"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, rootExpected("hit the correct root"), req))
  }

  test("Path param segments that collide with static path should win if not exactly the same as the static segment") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/collide/notmyroot"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, rootExpected("notmyroot"), req))
  }

  test("Single required path param with one value of correct type is supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-path-param/1"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(1), req))
  }

  test("Single required path param where multiple values are supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-path-param/1/2"))
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.NotFound) //4xx
        case _ => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }

  test("Single required pathy param where no values are supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-path-param"))
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.NotFound) //4xx
        case _ => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }

  test("Single required path param where param type is illegal") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-path-param/one"))
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.BadRequest)
        case _ => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }
}
