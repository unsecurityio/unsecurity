package io.unsecurity

import cats.Id
import cats.data.NonEmptyList
import no.scalabin.http4s.directives.Result
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.{Request, _}

class AcceptMatcherTest extends UnsecurityTestSuite {
  val contentTypeMatcher: AbstractContentTypeMatcher[Id] = new AbstractContentTypeMatcher[Id] {}

  val problemJsonContentType = `Content-Type`(mediaType"application/problem+json")

  val versionOneAcceptReq = Request[Id](
    method = Method.POST,
    uri = uri"/whatever",
    headers = Headers.of(Header("Accept", "application/vnd.custom;version=1"))
  )
  val unversionedAcceptReq = Request[Id](
    method = Method.POST,
    uri = uri"/whatever",
    headers = Headers.of(Header("Accept", "application/vnd.custom"))
  )

  val invalidAcceptRequest = Request[Id](
    method = Method.POST,
    uri = uri"/whatever",
    headers = Headers.of(Header("Accept", "foobar"))
  )
  val notAcceptableRequest = Request[Id](
    method = Method.POST,
    uri = uri"/whatever",
    headers = Headers.of(Header("Accept", "application/foobar"))
  )
  val supportedAcceptResquest = Request[Id](
    method = Method.POST,
    uri = uri"/whatever",
    headers = Headers.of(Header("Accept", "application/fjon"))
  )

  val Right(fjonMediaRange: `Content-Type`) = `Content-Type`.parse("application/fjon")

  val supportedContentType = ResponseAlternativeForContent(Some(fjonMediaRange), "dingdong")

  test("Invalid media-type is not accepted") {
    val responseAlternatives      = NonEmptyList.of(supportedContentType)
    val contentTypeMatchDirective = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(invalidAcceptRequest).where {
      case Result.Failure(r)
          if r.status == Status.NotAcceptable
            && r.contentType.contains(problemJsonContentType) =>
        Ok
    }
  }

  test("Unsupported media-type not accepted") {
    val responseAlternatives      = NonEmptyList.of(supportedContentType)
    val contentTypeMatchDirective = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(notAcceptableRequest).where {
      case Result.Error(r)
          if r.status == Status.NotAcceptable
            && r.contentType.contains(problemJsonContentType) =>
        Ok
    }
  }

  test("Supported media-type accepted") {
    val responseAlternatives      = NonEmptyList.of(supportedContentType)
    val contentTypeMatchDirective = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(supportedAcceptResquest).where {
      case Result.Success("dingdong") => Ok
    }
  }

  test("Allows get without accept") {
    val responseAlternatives = NonEmptyList.of(ResponseAlternativeForContent(None, "dingdong"))
    val get                  = Request[Id](method = Method.GET, uri = uri"/whatever")
    val contentTypeDirective = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeDirective.run(get).where {
      case Result.Success("dingdong") => Ok
    }
  }

  test("Allows delete without accept") {
    val responseAlternatives = NonEmptyList.of(ResponseAlternativeForContent(None, "dingdong"))
    val get                  = Request[Id](method = Method.DELETE, uri = uri"/whatever")
    val contentTypeDirective = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeDirective.run(get).where {
      case Result.Success("dingdong") => Ok
    }
  }

  test("Request with versioned Content-type with matching server") {
    val Right(versionedContentType) = `Content-Type`.parse("application/vnd.custom;version=1")
    val responseAlternatives        = NonEmptyList.of(ResponseAlternativeForContent(Some(versionedContentType), "spjong"))
    val contentTypeMatchDirective   = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(versionOneAcceptReq).where {
      case Result.Success("spjong") => Ok
    }
  }

  test("Request with versioned Content-type fails when server does not have specified version") {
    val Right(versionedContentType) = `Content-Type`.parse("application/vnd.custom;version=2")
    val responseAlternatives        = NonEmptyList.of(ResponseAlternativeForContent(Some(versionedContentType), "spjong"))
    val contentTypeMatchDirective   = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(versionOneAcceptReq).where {
      case Result.Error(r) if r.status == Status.NotAcceptable => Ok
    }
  }

  test("Request with versioned Content-type fails when server is unversioned") {
    val Right(versionedContentType) = `Content-Type`.parse("application/vnd.custom")
    val responseAlternatives        = NonEmptyList.of(ResponseAlternativeForContent(Some(versionedContentType), "spjong"))
    val contentTypeMatchDirective   = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(versionOneAcceptReq).where {
      case Result.Error(r) if r.status == Status.NotAcceptable => Ok
    }
  }

  test("Request without versioned Content-type succeeds when server has versioned content") {
    val version2Endpoint = ResponseAlternativeForContent(
      `Content-Type`.parse("application/vnd.custom;version=2").toOption,
      "welcome to version 2"
    )
    val version1Endpoint = ResponseAlternativeForContent(
      `Content-Type`.parse("application/vnd.custom;version=1").toOption,
      "welcome to version 1"
    )
    val responseAlternatives      = NonEmptyList.of(version2Endpoint, version1Endpoint)
    val contentTypeMatchDirective = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(unversionedAcceptReq).where {
      case Result.Success("welcome to version 2") => Ok
      case Result.Success(_) =>
        Fail(
          "Expected to reach version 2 endpoint, as it was defined first in list and no versiopn specified in request")
      case Result.Error(r) => Fail(r.bodyAsText.compile.last.toString)
    }
  }

  test("Content-type matching allows narrowed content-type") {
    val Right(textHtmlContentType) = `Content-Type`.parse("text/html")
    val textHtmlReponse            = ResponseAlternativeForContent(Some(textHtmlContentType), "dingdong")
    val responseAlternatives       = NonEmptyList.of(textHtmlReponse)
    val requestWithVersionedContentType =
      Request[Id](method = Method.POST, uri = uri"/whatever", headers = Headers.of(Header("Accept", "text/*")))
    val contentTypeMatchDirective = contentTypeMatcher.matchAcceptContentType(responseAlternatives)
    contentTypeMatchDirective.run(requestWithVersionedContentType).where {
      case Result.Success("dingdong") => Ok
    }
  }

}
