package io.unsecurity

import cats.effect.IO
import org.http4s.{Header, Headers, MediaRange, Method, Request, _}
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.{FlatSpec, Matchers}

class ConNegTest extends FlatSpec with Matchers {

  private val vndMediaType: MediaRange       = MediaRange.parse("application/vnd.testing+json").right.get
  val supportedContentTypes: Set[MediaRange] = Set(vndMediaType)

  "Invalid content type" should "return Invalid Media-Type error" in {
    val req =
      Request[IO](method = Method.GET, uri = uri"/whatever", headers = Headers.of(Header("content-type", "foobar")))

    val result: Either[HttpProblem, MediaRange] = Unsecurity.validateContentType(req, supportedContentTypes)

    assert(result.isLeft)
    result.left.value.detail.value should startWith("Content-Type missing or invalid mediatype")
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
