package io.unsecurity

import cats.effect.IO
import org.http4s.{Header, Headers, MediaRange, MediaType, Method, Request}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.http4s._

class ConNegTest extends FlatSpec with Matchers {

  private val vndMediaType: MediaRange       = MediaRange.parse("application/vnd.testing+json").right.get
  val supportedContentTypes: Set[MediaRange] = Set(vndMediaType)

  "Invalid content type" should "return Invalid Media-Type error" in {
    val req =
      Request[IO](method = Method.GET, uri = uri"/whatever", headers = Headers(Header("content-type", "foobar") :: Nil))
    val result = Unsecurity.validateContentType(req, supportedContentTypes)
    assert(result.isLeft)
    result.left.value.detail.value should startWith("Invalid Media-Type")
  }

  "Unsupported content-type" should "return Usupported Media-Type error" in {
    val req = Request[IO](method = Method.GET,
                          uri = uri"/whatever",
                          headers = Headers(Header("content-type", "application/foobar") :: Nil))
    val result = Unsecurity.validateContentType(req, supportedContentTypes)
    result.left.value.detail.value should startWith("Content-Type not supported")
    import io.circe.syntax._
  }

  "Supported content-type" should "return MediaType" in {
    val req = Request[IO](method = Method.GET,
                          uri = uri"/whatever",
                          headers = Headers(Header("content-type", "application/vnd.testing+json") :: Nil))
    val result = Unsecurity.validateContentType(req, supportedContentTypes)
    result.right.value shouldBe (vndMediaType)
  }

}
