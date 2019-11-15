package io.unsecurity

import cats.Id
import cats.effect.IO
import no.scalabin.http4s.directives.Result
import org.http4s.{Header, Headers, MediaRange, Method, Request, _}
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.{Logger, LoggerFactory}
import cats.implicits._

class ConNegTest extends FlatSpec with Matchers {

  private val vndMediaType: MediaRange       = MediaRange.parse("application/vnd.testing+json").right.get
  val supportedContentTypes: Set[MediaRange] = Set(vndMediaType)

  val unsecurity: Unsecurity[IO, String, String] = new Unsecurity[IO, String, String] {
    override def sc: SecurityContext[IO, String, String] = ???
    override def log: Logger                             = LoggerFactory.getLogger("fjon")
  }

  "Invalid content type" should "return Invalid Media-Type error" in {
    val req =
      Request[IO](method = Method.GET, uri = uri"/whatever", headers = Headers.of(Header("content-type", "foobar")))

    val result: Result[IO, MediaRange] = unsecurity.validateContentType(supportedContentTypes).run(req).unsafeRunSync()

    result match {
      case Result.Error(response) =>
        assert(response.status == Status.UnsupportedMediaType)
        val body: EntityBody[IO] = response.body
        val t: String = body.through(fs2.text.utf8Decode).compile.foldMonoid.unsafeRunSync()
        assert(t.contains("Content-Type missing or invalid mediatype"))
    }
  }

  "Unsupported content-type" should "return Usupported Media-Type error" in {
    val req = Request[IO](method = Method.GET,
                          uri = uri"/whatever",
                          headers = Headers.of(Header("content-type", "application/foobar")))
    val result = Unsecurity.validateContentType(req, supportedContentTypes)
    result.left.value.detail.value should startWith("Content-Type not supported")
  }

  "Supported content-type" should "return MediaType" in {
    val req = Request[IO](method = Method.GET,
                          uri = uri"/whatever",
                          headers = Headers.of(Header("content-type", "application/vnd.testing+json")))
    val result = Unsecurity.validateContentType(req, supportedContentTypes)
    result.right.value shouldBe (vndMediaType)
  }

}
