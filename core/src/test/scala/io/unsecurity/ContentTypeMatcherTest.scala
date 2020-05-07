package io.unsecurity

import cats.Id
import cats.implicits._
import no.scalabin.http4s.directives.Result
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.{Request, _}

class ContentTypeMatcherTest extends UnsecurityTestSuite {
  val contentTypeMatcher: AbstractContentTypeMatcher[Id] = new AbstractContentTypeMatcher[Id] {}

  val problemJsonContentType = `Content-Type`(mediaType"application/problem+json")

  val versionedMediaTypeReq = Request[Id](method= Method.POST, uri = uri"/whatever", headers=Headers.of(Header("content-type", "application/vnd.custom;version=1")))
  val unversionedMediaTypeReq = Request[Id](method= Method.POST, uri = uri"/whatever", headers=Headers.of(Header("content-type", "application/vnd.custom")))

  val invalidMediaType =
    Request[Id](method = Method.POST, uri = uri"/whatever", headers = Headers.of(Header("content-type", "foobar")))
  val unsupportedMediaType = Request[Id](method = Method.POST,
                                         uri = uri"/whatever",
                                         headers = Headers.of(Header("content-type", "application/foobar")))
  val supportedMediaType = Request[Id](method = Method.POST,
                                       uri = uri"/whatever",
                                       headers = Headers.of(Header("content-type", "application/fjon")))
  val fjonMediaRange: MediaRange = MediaRange.parse("application/fjon").getOrElse(throw new RuntimeException("could not parse media range"))
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
        println(r.bodyAsText.compile.last)
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

  test("Allows delete without content-type") {
    val mediaRangeMap        = MediaRangeMap(List(Set(MediaRange.`*/*`) -> "dingdong"))
    val get                  = Request[Id](method = Method.DELETE, uri = uri"/whatever")
    val contentTypeDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeDirective.run(get).where {
      case Result.Success("dingdong") => Ok
    }
  }

  test("Request with versioned Content-type with matching server") {
    val versionedMediaRange = MediaRange.parse("application/vnd.custom;version=1")
    val versionedMediaType = Set(versionedMediaRange.toOption.get) -> "spjong"
    val mediaRangeMap = MediaRangeMap(List(versionedMediaType))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(versionedMediaTypeReq).where {
      case Result.Success("spjong") => Ok
    }
  }

  test("Request with versioned Content-type fails when server does not have specified version") {
    val versionedMediaRange = MediaRange.parse("application/vnd.custom;version=2")
    val versionedMediaType = Set(versionedMediaRange.toOption.get) -> "spjong"
    val mediaRangeMap = MediaRangeMap(List(versionedMediaType))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(versionedMediaTypeReq).where {
      case Result.Error(r) if(r.status == Status.UnsupportedMediaType) => Ok
    }
  }

  test("Request with versioned Content-type fails when server is unversioned") {
    val versionedMediaRange = MediaRange.parse("application/vnd.custom")
    val versionedMediaType = Set(versionedMediaRange.toOption.get) -> "spjong"
    val mediaRangeMap = MediaRangeMap(List(versionedMediaType))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(versionedMediaTypeReq).where {
      case Result.Error(r) if(r.status == Status.UnsupportedMediaType) => Ok
    }
  }

  test("Request without versioned Content-type succeeds when server has versioned content") {
    val version2Endpoint = Set(MediaRange.parse("application/vnd.custom;version=2").toOption.get) -> "welcome to version 2"
    val version1Endpoint = Set(MediaRange.parse("application/vnd.custom;version=1").toOption.get) -> "welcome to version 1"
    val mediaRangeMap = MediaRangeMap(List(version2Endpoint, version1Endpoint))
    val contentTypeMatchDirective = contentTypeMatcher.matchContentType(mediaRangeMap)
    contentTypeMatchDirective.run(unversionedMediaTypeReq).where {
      case Result.Success("welcome to version 2") => Ok
      case Result.Success(_) => Fail("Expected to reach version 2 endpoint, as it was defined first in list and no versiopn specified in request")
      case Result.Error(r) => Fail(r.bodyAsText.compile.last.toString)
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
