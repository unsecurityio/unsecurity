package io.unsecurity.auth
package auth0
package oidc

import java.net.URL

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

import scala.util.Try

case class State(
    state: String,
    returnToUrl: URL,
    callbackUrl: URL,
    additionalData: String
)

object State {
  implicit val decodeURI: Decoder[URL] =
    Decoder.decodeString.flatMap { str =>
      Decoder.instanceTry { _ =>
        Try(new URL(str))
      }
    }

  implicit val encodeURI: Encoder[URL] =
    uri => Json.fromString(uri.toString)

  implicit val decodeState: Decoder[State] = Decoder[State] { c =>
    for {
      state          <- c.downField("state").as[String]
      returnToUrl    <- c.downField("returnToUrl").as[URL]
      callbackUrl    <- c.downField("callbackUrl").as[URL]
      additionalData <- c.downField("additionalData").as[String]
    } yield {
      State(
        state = state,
        returnToUrl = returnToUrl,
        callbackUrl = callbackUrl,
        additionalData = additionalData
      )
    }
  }

  implicit val encodeState: Encoder[State] =
    (s: State) =>
      Json.obj(
        ("state", Json.fromString(s.state)),
        ("returnToUrl", s.returnToUrl.asJson),
        ("callbackUrl", s.callbackUrl.asJson),
        ("additionalData", s.additionalData.asJson)
    )

}
