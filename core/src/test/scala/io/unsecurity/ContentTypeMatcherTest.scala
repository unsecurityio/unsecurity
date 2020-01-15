package io.unsecurity

import cats.Id
import no.scalabin.http4s.directives.Result
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.{Request, _}

class ContentTypeMatcherTest extends UnsecurityTestSuite {
  val contentTypeMatcher: AbstractContentTypeMatcher[Id] = new AbstractContentTypeMatcher[Id] {}

  val problemJsonContentType = `Content-Type`(mediaType"application/problem+json")

  val invalidMediaType =
    Request[Id](method = Method.POST, uri = uri"/whatever", headers = Headers.of(Header("content-type", "foobar")))
  val unsupportedMediaType = Request[Id](method = Method.POST,
                                         uri = uri"/whatever",
                                         headers = Headers.of(Header("content-type", "application/foobar")))
  val supportedMediaType = Request[Id](method = Method.POST,
                                       uri = uri"/whatever",
                                       headers = Headers.of(Header("content-type", "application/fjon")))
  val fjonMediaRange: MediaRange = MediaRange.parse("application/fjon").right.get
  val supportedMediaRange        = Set(fjonMediaRange) -> "dingdong"

  test("Invalid media-type is not accepted") {
    val mediaRangeMap             = MediaRangeMap(List(supportedMediaRange))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(invalidMediaType).where {
      case Result.Failure(r)
          if r.status == Status.UnsupportedMediaType
            && r.contentType.contains(problemJsonContentType) =>
        Ok
    }
  }

  test("Unsupported media-type not accepted") {
    val mediaRangeMap             = MediaRangeMap(List(supportedMediaRange))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(unsupportedMediaType).where {
      case Result.Error(r)
          if r.status == Status.UnsupportedMediaType
            && r.contentType.contains(problemJsonContentType) =>
        Ok
    }
  }

  test("Supported media-type accepted") {
    val mediaRangeMap             = MediaRangeMap(List(supportedMediaRange))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(supportedMediaType).where {
      case Result.Success("dingdong") => Ok
    }
  }

  test("Allows get without content-type") {
    val mediaRangeMap        = MediaRangeMap(List(Set(MediaRange.`*/*`) -> "dingdong"))
    val get                  = Request[Id](method = Method.GET, uri = uri"/whatever")
    val contentTypeDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeDirective.run(get).where {
      case Result.Success("dingdong") => Ok
    }
  }

  test("Content-type matching allows properties") {
    val mediaRangeMap = MediaRangeMap(List(supportedMediaRange))
    val requestWithVersionedContentType =
      Request[Id](method = Method.POST,
                  uri = uri"/whatever",
                  headers = Headers.of(Header("content-type", "application/fjon;version=1")))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(requestWithVersionedContentType).where {
      case Result.Success("dingdong") => Ok
    }
  }

  test("Content-type matching allows narrowed content-type") {
    val allText       = Set(MediaRange.`text/*`) -> "dingdong"
    val mediaRangeMap = MediaRangeMap(List(allText))
    val requestWithVersionedContentType =
      Request[Id](method = Method.POST, uri = uri"/whatever", headers = Headers.of(Header("content-type", "text/html")))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(requestWithVersionedContentType).where {
      case Result.Success("dingdong") => Ok
    }
  }

}
