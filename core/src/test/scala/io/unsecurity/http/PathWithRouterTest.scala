package io.unsecurity.http

import cats.effect.IO
import io.circe.Json
import io.unsecurity.Server
import io.unsecurity.hlinx.HLinx._
import org.http4s.circe._
import org.http4s.server.{Router, Server}
import org.http4s._

class PathWithRouterTest extends HttpIOSuite {

  import unsecurity._

  def zero(root: String): Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root,
        Produces.json[Json]
      )
    ).run { _ =>
      Json.fromString(s"$root/zero")
    }

  def one(root: String): Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "one",
        Produces.json[Json]
      )
    ).run { _ =>
      Json.fromString(s"$root/one")
    }

  def two(root: String): Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "one" / "two",
        Produces.json[Json]
      )
    ).run { _ =>
      Json.fromString(s"$root/two")
    }

  val withRouter: HttpRoutes[IO] = Router(
    "/root"          -> Server.toHttpRoutes(zero("root"), one("root"), two("root")),
    "/combined/root" -> Server.toHttpRoutes(zero("combined"), one("combined"), two("combined")),
    "/otherroot"     -> Server.toHttpRoutes( zero("otherroot"), one("otherroot"), two("otherroot"))
  )
  val server: Fixture[Server] = server(withRouter)

  test("Static path combined with http4s router. combined/one") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/combined/root/one"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, Json.fromString("combined/one"), req))
  }
  test("Static path combined with http4s router. root/one") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/root/one"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, Json.fromString("root/one"), req))
  }
  test("Static path combined with http4s router. root/two") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/root/one/two"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, Json.fromString("root/two"), req))
  }
  test("Static path combined with http4s router. root/zero") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/root"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, Json.fromString("root/zero"), req))
  }
  test("Static path combined with http4s router. otherroot/one") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/otherroot/one"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, Json.fromString("otherroot/one"), req))
  }
  test("Static path combined with http4s router. otherroot/two") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/otherroot/one/two"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, Json.fromString("otherroot/two"), req))
  }
  test("Static path combined with http4s router. otherroot/zero") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/otherroot"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, Json.fromString("otherroot/zero"), req))
  }
  test("Static path combined with http4s router. not defined") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/thirdroot"))
    httpClient()
      .status(req)
      .map(actual => assertEquals(actual, Status.NotFound, req))
  }

}
