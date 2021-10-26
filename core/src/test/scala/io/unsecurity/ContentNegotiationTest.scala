package io.unsecurity

import org.scalatest.funsuite.AnyFunSuite
import org.http4s.{MediaType, QValue}
import cats.data.NonEmptyList
import org.http4s.headers.{Accept, MediaRangeAndQValue}
import org.http4s.syntax.literals._

class ContentNegotiationTest extends AnyFunSuite {

  val accept: List[MediaRangeAndQValue] = Accept
    .parse("text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")
    .fold(throw _, identity)
    .values
    .toList

  val acceptNarrow: List[MediaRangeAndQValue] = Accept
    .parse("text/html;level=2;q=1")
    .fold(throw _, identity)
    .values
    .toList

  test("score */*") {
    assert(
      ContentNegotiation
        .score(accept, NonEmptyList.of(MediaType.image.jpeg -> "image")) == List(qValue"0.5" -> "image")
    )
  }
  test("score explicit and type") {
    assert(
      ContentNegotiation
        .score(
          accept,
          NonEmptyList
            .of(MediaType.text.html.withExtensions(Map("level" -> "1")) -> "exact", MediaType.text.html -> "type")
        ) ==
        List(QValue.One -> "exact", qValue"0.7" -> "type")
    )
  }
  test("score range") {
    assert(
      ContentNegotiation
        .score(accept, NonEmptyList.of(MediaType.`text/event-stream` -> "range")) == List(qValue"0.3" -> "range")
    )
  }
  test("all together") {
    assert(
      ContentNegotiation.score(
        accept,
        NonEmptyList.of(
          MediaType.image.jpeg -> "image",
          MediaType.text.html.withExtensions(Map("level" -> "1")) -> "exact",
          MediaType.text.html           -> "type",
          MediaType.`text/event-stream` -> "range"
        )
      ) == List(qValue"0.5" -> "image", QValue.One -> "exact", qValue"0.7" -> "type", qValue"0.3" -> "range")
    )
  }

  test("too narrow") {
    assert(
      ContentNegotiation.score(
        acceptNarrow,
        NonEmptyList.of(
          MediaType.text.html.withExtensions(Map("level" -> "1")) -> "exact",
          MediaType.text.html -> "type",
        )
      ) == Nil
    )
  }
}
