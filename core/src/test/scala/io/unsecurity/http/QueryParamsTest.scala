package io.unsecurity.http

import cats.effect.{Blocker, IO}
import io.circe.Json
import io.circe.syntax._
import io.unsecurity.hlinx.HLinx._
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
    ).run { counter =>
      counterExpected(counter)
    }

  val singleOptionQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "single-query-param" :? "counter".as[Int].?,
        Produces.json[Json]
      )
    ).run { counter =>
      counterExpected(counter.getOrElse(-1))
    }

  val mulitpleQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "single-query-param" :? "counter".as[Int].*,
        Produces.json[Json]
      )
    ).run { counter =>
      counterExpected(counter.sum)
    }

  val twoRequiredQueryParamService: Complete =
    unsecure(
      Endpoint(
        Method.GET,
        Root / "single-query-param" :? "counter".as[Int] & "nextCounter".as[Int],
        Produces.json[Json]
      )
    ).run { case (counter, nextCounter) =>
      counterExpected(counter + nextCounter)
    }



  val server: Fixture[Server[IO]] = server(singleRequiredQueryParamService)

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
        case st: UnexpectedStatus => IO(st.status == Status.BadRequest)  //4xx
        case _                   => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }

  test("Single required query Param where param type is illegal") {
    val req = Request[IO](uri = Uri.unsafeFromString(s"http://localhost:$port/single-query-param?counter=one"))
    import cats.implicits._
    val blocker = Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.global)
    httpClient()
      .expect[Json](req)
      .handleErrorWith {
        case st: UnexpectedStatus => IO(st.status == Status.BadRequest)
        case _                   => IO(false)
      }
      .map(actual => assertEquals(actual, true, req))
  }
}
