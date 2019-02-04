package io.unsecurity.auth

import java.net.URLDecoder

import io.circe.syntax._
import io.circe.{Decoder, Encoder}

case class UserId(asString: String)
object UserId {
  def urlDecode(url: String): UserId =
    UserId(URLDecoder.decode(url, "utf8"))

  implicit val userIdEncoder: Encoder[UserId] = Encoder { userId =>
    userId.asString.asJson
  }

  implicit val userIdDecoder: Decoder[UserId] = Decoder { c =>
    c.as[String].map(id => UserId(id))
  }
}