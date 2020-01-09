package io.unsecurity

import cats.Id
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Result
import org.http4s.implicits._
import org.http4s.{Request, Status}

class PathMatcherTest extends UnsecurityTestSuite {
  val pathMatchers = new AbstractPathMatcher[Id] {}

  test("/test") {
    val req: Request[Id] = Request[Id](uri = uri"/test")

    val pathMatcher: pathMatchers.PathMatcher[Unit] = pathMatchers.createPathMatcher(Root / "test")
    assert(pathMatcher.isDefinedAt("/test"))

    pathMatcher
      .apply("/test")
      .run(req)
      .where { case Result.Success(()) => Ok }
  }

  test("/test/{int}") {
    val uri              = uri"/test/123"
    val req: Request[Id] = Request[Id](uri = uri)

    val pathMatcher: pathMatchers.PathMatcher[Tuple1[Int]] =
      pathMatchers.createPathMatcher(Root / "test" / 'id.as[Int])
    assert(!pathMatcher.isDefinedAt("/test"))
    assert(!pathMatcher.isDefinedAt("/test/aaa/aaa"))
    assert(pathMatcher.isDefinedAt("/test/aaa"))
    assert(pathMatcher.isDefinedAt(uri.renderString))

    pathMatcher
      .apply(uri.renderString)
      .run(req)
      .where { case Result.Success(Tuple1(i: Int)) => Ok }
    pathMatcher
      .apply("/test/aaa")
      .run(req)
      .where {
        case Result.Failure(response) if response.status == Status.BadRequest => Ok
      }
  }
}
