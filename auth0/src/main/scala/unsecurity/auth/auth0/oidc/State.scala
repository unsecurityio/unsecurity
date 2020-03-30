package unsecurity.auth.auth0.oidc

import java.net.URI

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

import scala.util.Try

case class State(
    state: String,
    returnToUrl: URI,
    callbackUrl: URI,
    additionalData: String
)

object State {
  implicit val decodeURI: Decoder[URI] =
    Decoder.decodeString.flatMap { str =>
      Decoder.instanceTry { _ =>
        Try(URI.create(str))
      }
    }

  implicit val encodeURI: Encoder[URI] =
    uri => Json.fromString(uri.toString)

  implicit val decodeState: Decoder[State] =
    Decoder[State](
      c =>
        for {
          state          <- c.downField("state").as[String]
          returnToUrl    <- c.downField("returnToUrl").as[URI]
          callbackUrl    <- c.downField("callbackUrl").as[URI]
          additionalData <- c.downField("additionalData").as[String]
        } yield {
          State(
            state = state,
            returnToUrl = returnToUrl,
            callbackUrl = callbackUrl,
            additionalData = additionalData
          )
      }
    )

  implicit val encodeState: Encoder[State] =
    (s: State) =>
      Json.obj(
        ("state", Json.fromString(s.state)),
        ("returnToUrl", s.returnToUrl.asJson),
        ("callbackUrl", s.callbackUrl.asJson),
        ("additionalData", s.additionalData.asJson)
    )

}
