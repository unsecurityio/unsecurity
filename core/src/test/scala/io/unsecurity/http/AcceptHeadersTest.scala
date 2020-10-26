package io.unsecurity.http

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.Json
import io.circe.syntax._
import io.unsecurity.hlinx.HLinx._
import org.http4s._
import org.http4s.circe._
import org.http4s.client.UnexpectedStatus
import org.http4s.headers.{`Content-Type`, Accept, MediaRangeAndQValue}
import org.http4s.server.Server

class AcceptHeadersTest extends HttpIOSuite {
  import unsecurity._

  val jsonPayload: Json = Json.obj(
    "version" := "json"
  )

  val versionOnePayload: Json = Json.obj(
    "version" := "1"
  )

  val versionTwoPayload: Json = Json.obj(
    "version" := "2"
  )

  val mixedCasePayload: Json = Json.obj(
    "version" := "mixed"
  )

  val versionOneService: Complete =
    unsecure(
      Endpoint(
        "wildcard content type test",
        Method.GET,
        Root / "accept-header",
        Produces.jsonWithContentType[Json](ContentTypes.unsecurity("1"))
      )
    ).run { _ =>
      versionOnePayload
    }

  val versionTwoService: Complete =
    unsecure(
      Endpoint(
        "wildcard content type test",
        Method.GET,
        Root / "accept-header",
        Produces.jsonWithContentType[Json](ContentTypes.unsecurity("2"))
      )
    ).run { _ =>
      versionTwoPayload
    }

  val mixedCaseService: Complete =
    unsecure(
      Endpoint(
        "mixed case content type test",
        Method.GET,
        Root / "mixed-case",
        Produces.jsonWithContentType[Json](ContentTypes.mixedCase)
      )
    ).run { _ =>
      mixedCasePayload
    }

  val jsonService: Complete =
    unsecure(
      Endpoint(
        "mixed case content type test",
        Method.GET,
        Root / "json",
        Produces.json[Json]
      )
    ).run { _ =>
      jsonPayload
    }

  val server: Fixture[Server[IO]] = server(versionTwoService, versionOneService, mixedCaseService, jsonService)
  val baseReq: Request[IO]        = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/accept-header"))

  test("Wildcard accept header") {
    val req = baseReq.withHeaders(Header("Accept", "*/*"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, versionTwoPayload, req))
  }

  test("Partial wildcard accept header") {
    val req = baseReq.withHeaders(Header("Accept", "application/*"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, versionTwoPayload, req))
  }

  test("Using manual Accept header, not using extensions") {
    val req = baseReq.withHeaders(Header("Accept", "application/vnd.unsecurity+json"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, versionTwoPayload, req))
  }

  test("Using Accept header from Https, Version 1") {
    val req = baseReq.withHeaders(AcceptHeaders.unsecurity("1"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, versionOnePayload, req))
  }

  test("Using Accept header from Https, Version 2") {
    val req = baseReq.withHeaders(AcceptHeaders.unsecurity("2"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, versionTwoPayload, req))
  }

  test("Using manual Accept header, using extensions") {
    val req = baseReq.withHeaders(Header("Accept", "application/vnd.unsEcurity+json;version=1"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, versionOnePayload, req))
  }

  test("Using manual Accept header, using unsupported extensions") {
    val req = baseReq.withHeaders(Header("Accept", "application/vnd.unsecurity+json;version=3"))
    httpClient()
      .expect[Json](req)
      .map(_ => false)
      .handleErrorWith {
        case _: UnexpectedStatus => IO(true)
        case _                   => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }

  test("std application/json") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/json"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, jsonPayload, req))
  }

  test("mixed case headers") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/mixed-case")).withHeaders(AcceptHeaders.mixedCase)
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, mixedCasePayload, req))
  }

  object MediaTypes {
    def unsecurity(version: String) =
      new MediaType("application", "vnd.unsecurity+json", compressible = true, binary = false, extensions = Map[String, String]("version" -> version))

    def mixedCase =
      new MediaType("application", "vnd.unsecurity.mixedCase+json", compressible = true, binary = false)
  }

  object AcceptHeaders {
    def unsecurity(version: String): Accept = Accept(NonEmptyList.one(MediaRangeAndQValue(MediaTypes.unsecurity(version))))
    def mixedCase: Accept                   = Accept(NonEmptyList.one(MediaRangeAndQValue(MediaTypes.mixedCase)))
  }

  object ContentTypes {
    def unsecurity(version: String): `Content-Type` = `Content-Type`(MediaTypes.unsecurity(version))
    def mixedCase: `Content-Type`                   = `Content-Type`(MediaTypes.mixedCase)
  }

}
