package io.unsecurity

import cats.Id
import no.scalabin.http4s.directives.Result
import org.http4s.headers.`Content-Type`
import org.http4s.{Request, _}
import org.slf4j.{Logger, LoggerFactory}

class ContentTypeMatcherTest extends UnsecurityTestSuite {
  val contentTypeMatcher: AbstractContentTypeMatcher[Id] = new AbstractContentTypeMatcher[Id] {
    val log: Logger = LoggerFactory.getLogger("ContentTypeMatcherTest")
  }

  val problemJsonContentType = `Content-Type`(mediaType"application/problem+json")

  val invalidMediaType =
    Request[Id](method = Method.POST, uri = uri"/whatever", headers = Headers.of(Header("content-type", "foobar")))
  val unsupportedMediaType = Request[Id](method = Method.POST,
                                         uri = uri"/whatever",
                                         headers = Headers.of(Header("content-type", "application/foobar")))
  val supportedMediaType = Request[Id](method = Method.POST,
                                       uri = uri"/whatever",
                                       headers = Headers.of(Header("content-type", "application/fjon")))
  val fjonMediaRange: MediaRange = mediaType"application/fjon"
  val supportedMediaRange        = Set(fjonMediaRange) -> "dingdong"

  test("invalidMediaType") {
    val methodMap            = MediaRangeMap(List(supportedMediaRange))
    val methodMatchDirective = contentTypeMatcher.matchContentType(methodMap)
    val value = methodMatchDirective.run(invalidMediaType).where {
      case Result.Failure(r)
          if r.status == Status.UnsupportedMediaType
            && r.contentType.exists(_ == problemJsonContentType) =>
        Ok
    }
  }

  test("unsupportedMediaType") {
    val methodMap            = MediaRangeMap(List(supportedMediaRange))
    val methodMatchDirective = contentTypeMatcher.matchContentType(methodMap)
    val value = methodMatchDirective.run(unsupportedMediaType).where {
      case Result.Error(r)
          if r.status == Status.UnsupportedMediaType
            && r.contentType.exists(_ == problemJsonContentType) =>
        Ok
    }
  }

  test("supportedMediaType") {
    val methodMap            = MediaRangeMap(List(supportedMediaRange))
    val methodMatchDirective = contentTypeMatcher.matchContentType(methodMap)
    val value = methodMatchDirective.run(supportedMediaType).where {
      case Result.Success("dingdong") => Ok
    }
  }

}
