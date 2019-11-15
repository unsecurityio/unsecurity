package io.unsecurity

import cats.effect.IO
import cats.implicits._
import no.scalabin.http4s.directives.Result
import org.http4s.{Header, Headers, MediaRange, Method, Request, _}
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.{Logger, LoggerFactory}

class ConNeg2Test extends IOFunSuite with Matchers {

  private val vndMediaType: MediaRange       = MediaRange.parse("application/vnd.testing+json").right.get
  val supportedContentTypes: Set[MediaRange] = Set(vndMediaType)

  val unsecurity: Unsecurity[IO, String, String] = new Unsecurity[IO, String, String] {
    override def sc: SecurityContext[IO, String, String] = ???
    override def log: Logger                             = LoggerFactory.getLogger("fjon")
  }

  test("Invalid content type should return Invalid Media-Type error") = {
    val req =
      Request[IO](method = Method.GET, uri = uri"/whatever", headers = Headers.of(Header("content-type", "foobar")))

    for {
      result <- unsecurity.validateContentType(supportedContentTypes).run(req)
      body <- result match {
               case Result.Error(response) =>
                 assert(response.status == Status.UnsupportedMediaType)
                 response.body
                   .through(fs2.text.utf8Decode)
                   .compile
                   .foldMonoid
               case _ => fail("ouch")
             }
    } yield {
      assert(body.contains("Content-Type missing or invalid mediatype"))
    }
  }
}
