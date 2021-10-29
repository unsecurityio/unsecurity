package io.unsecurity.http

import cats.effect.IO
import io.circe.Json
import io.circe.syntax._
import io.unsecurity.hlinx._
import io.unsecurity.hlinx.HLinx._
import io.unsecurity.hlinx.ParamConverter
import org.http4s.circe._
import org.http4s.client.UnexpectedStatus
import org.http4s.server.Server
import org.http4s.{Method, Request, Status, Uri}

class QueryParamsTest extends HttpIOSuite {
  import unsecurity._

  def counterExpected(counter: Int): Json = Json.obj {
    "counter" := counter
  }

  val singleRequiredQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "single-query-param" :? "counter".as[Int],
        Produces.json[Json]
      )
    ).run { case counter =>
      counterExpected(counter)
    }

  val singleOptionQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "optional-query-param" :? "counter".as[Int].?,
        Produces.json[Json]
      )
    ).run { counter =>
      counterExpected(counter.getOrElse(-1))
    }

  val mulitpleQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "multiple-query-param" :? "counter".as[Int].*,
        Produces.json[Json]
      )
    ).run { counter =>
      counterExpected(counter.sum)
    }

  val twoRequiredQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "two-query-param" :? "counter".as[Int] & "nextCounter".as[Int],
        Produces.json[Json]
      )
    ).run { case (counter, nextCounter) =>
      counterExpected(counter + nextCounter)
    }

  case class EncodedParam(fullName: String)
  def encodedParamExpected(encoded: String): Json = Json.obj {
    "encoded" := encoded
  }
  implicit val encodedParamConverter: ParamConverter[EncodedParam] = ParamConverter.createSimple[EncodedParam](EncodedParam(_))
  val encodedQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "encoded-query-param" :? "encoded".as[EncodedParam],
        Produces.json[Json]
      )
    ).run { encoded =>
      encodedParamExpected(encoded.fullName)
    }

  val encodedListQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "encoded-list-query-param" :? "encoded".as[EncodedParam].*,
        Produces.json[Json]
      )
    ).run { encoded =>
      encodedParamExpected(encoded.map(_.fullName).mkString(","))
    }


  val server: Fixture[Server] = server(encodedListQueryParamService, encodedQueryParamService, singleRequiredQueryParamService, mulitpleQueryParamService, twoRequiredQueryParamService, singleOptionQueryParamService)

  test("Single required query Param with one value of correct type is supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-query-param?counter=1"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(1), req))
  }

  test("Single required query Param where multiple values are supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-query-param?counter=1&counter=3&counter=3"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(1), req))
  }

  test("Single required query Param where no values are supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-query-param"))
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.BadRequest)
        case _                   => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }

  test("Single required query Param where param type is illegal") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-query-param?counter=one"))
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.BadRequest)
        case _                   => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }

  test("Single option query Param with one value of correct type is supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/optional-query-param?counter=1"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(1), req))
  }

  test("Single option query Param with one value of correct type is supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/optional-query-param"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(-1), req))
  }

  test("Two required query Param where both values are supplied correctly.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/two-query-param?counter=1&nextCounter=4"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(5), req))
  }

  test("Two required query Param where value missing.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/two-query-param?counter=1"))
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.BadRequest)
        case _                   => IO(false)
      }
  }

  test("Multiple query Param with two values of correct type is supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/multiple-query-param?counter=1&counter=2"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(3), req))
  }

  test("Multiple query Param with no values supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/multiple-query-param"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, counterExpected(0), req))
  }

  test("Multiple query Param with one value of correct type and one with wrong type is supplied.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/multiple-query-param?counter=1&counter=two"))
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.BadRequest)
        case _                   => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }

  test("Single Query Param Encoding in DSL.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/encoded-query-param?encoded=Unsecurity%20Encodes%20Param%20Correctly"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, encodedParamExpected("Unsecurity Encodes Param Correctly"), req))
  }

  test("Multiple Query Param Encoding in DSL.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/encoded-list-query-param?encoded=Unsecurity%20Encodes%20Param%20Correctly&encoded=Even%20In%20Lists"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, encodedParamExpected("Unsecurity Encodes Param Correctly,Even In Lists"), req))
  }

  test("Multiple Query Param Encoding in DSL with no supplied params.") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/encoded-list-query-param"))
    httpClient()
      .expect[Json](req)
      .map(actual => assertEquals(actual, encodedParamExpected(""), req))
  }
}
