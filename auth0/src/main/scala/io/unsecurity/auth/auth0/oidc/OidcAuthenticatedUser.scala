package io.unsecurity.auth
package auth0
package oidc

import java.net.{URLDecoder, URLEncoder}

import java.time.OffsetDateTime

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

case class UserId(asString: String) {
  def urlEncode: String = URLEncoder.encode(asString, "utf8")
}
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

case class OidcAuthenticatedUser(nickname: Option[String],
                                 name: String,
                                 picture: String,
                                 updatedAt: OffsetDateTime,
                                 email: String,
                                 emailVerified: Boolean,
                                 issuer: String,
                                 subject: String,
                                 audience: String,
                                 issuedAt: Long,
                                 expirationTime: Long,
                                 userId: UserId,
                                 additionalData: String)

object OidcAuthenticatedUser {
  implicit val authenticatedUserEncoder: Encoder[OidcAuthenticatedUser] = Encoder { au =>
    Json.obj(
      "nickname" := au.nickname,
      "name" := au.name,
      "picture" := au.picture,
      "updated_at" := au.updatedAt,
      "email" := au.email,
      "email_verified" := au.emailVerified,
      "iss" := au.issuer,
      "sub" := au.userId,
      "aud" := au.audience,
      "iat" := au.issuedAt,
      "exp" := au.expirationTime,
      "user_id" := au.userId,
      "additionalData" := au.additionalData
    )
  }

  implicit val authenticatedUserDecoder: Decoder[OidcAuthenticatedUser] = Decoder { c =>
    for {
      nickname       <- c.downField("nickname").as[Option[String]]
      name           <- c.downField("name").as[String]
      picture        <- c.downField("picture").as[String]
      updatedAt      <- c.downField("updated_at").as[OffsetDateTime]
      email          <- c.downField("email").as[String]
      emailVerified  <- c.downField("email_verified").as[Boolean]
      iss            <- c.downField("iss").as[String]
      sub            <- c.downField("sub").as[String]
      aud            <- c.downField("aud").as[String]
      iat            <- c.downField("iat").as[Long]
      exp            <- c.downField("exp").as[Long]
      additionalData <- c.downField("additionalData").as[String]
    } yield {
      OidcAuthenticatedUser(
        nickname,
        name,
        picture,
        updatedAt,
        email,
        emailVerified,
        iss,
        sub,
        aud,
        iat,
        exp,
        UserId(sub),
        additionalData
      )
    }
  }
}
