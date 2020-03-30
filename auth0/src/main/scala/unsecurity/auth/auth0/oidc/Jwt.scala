package unsecurity.auth.auth0.oidc

import io.circe.Decoder

object Jwt {

  case class JwtHeader(typ: String, alg: String, kid: String)
  object JwtHeader {
    implicit val jwtDeaderDecoder: Decoder[JwtHeader] = Decoder { tr =>
      for {
        typ <- tr.downField("typ").as[String]
        alg <- tr.downField("alg").as[String]
        kid <- tr.downField("kid").as[String]
      } yield {
        JwtHeader(
          typ,
          alg,
          kid
        )
      }
    }
  }

  case class JwtPayload(iss: String, sub: String, aud: String, exp: Long, iat: Long)
  object JwtPayload {
    implicit val jwtPayloadDecoder: Decoder[JwtPayload] = Decoder { tr =>
      for {
        iss <- tr.downField("iss").as[String]
        sub <- tr.downField("sub").as[String]
        aud <- tr.downField("aud").as[String]
        exp <- tr.downField("exp").as[Long]
        iat <- tr.downField("iat").as[Long]
      } yield JwtPayload(iss, sub, aud, exp, iat)
    }
  }

}
