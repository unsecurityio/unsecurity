package io.unsecurity.auth.auth0.m2m

import io.circe.Decoder

case class JwtToken(iss: String, sub: String, aud: String, exp: Long, iat: Long, scopes: List[String])
object JwtToken {
  implicit val jwtTokenDecoder: Decoder[JwtToken] = Decoder { tr =>
    for {
      iss   <- tr.downField("iss").as[String]
      sub   <- tr.downField("sub").as[String]
      aud   <- tr.downField("aud").as[String]
      exp   <- tr.downField("exp").as[Long]
      iat   <- tr.downField("iat").as[Long]
      scope <- tr.downField("scope").as[Option[String]]
    } yield JwtToken(iss, sub, aud, exp, iat, scope.map(_.split(" ").toList).getOrElse(List.empty))
  }
}
