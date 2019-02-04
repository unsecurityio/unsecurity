package io.unsecurity.auth
package auth0
package oidc

import java.time.OffsetDateTime

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

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
                                 userId: UserId)
  extends AuthenticatedUserWithMeta

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
      "user_id" := au.userId
    )
  }

  implicit val authenticatedUserDecoder: Decoder[OidcAuthenticatedUser] = Decoder { c =>
    for {
      nickname      <- c.downField("nickname").as[Option[String]]
      name          <- c.downField("name").as[String]
      picture       <- c.downField("picture").as[String]
      updatedAt     <- c.downField("updated_at").as[OffsetDateTime]
      email         <- c.downField("email").as[String]
      emailVerified <- c.downField("email_verified").as[Boolean]
      iss           <- c.downField("iss").as[String]
      sub           <- c.downField("sub").as[String]
      aud           <- c.downField("aud").as[String]
      iat           <- c.downField("iat").as[Long]
      exp           <- c.downField("exp").as[Long]

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
        UserId(sub)
      )
    }
  }
}
