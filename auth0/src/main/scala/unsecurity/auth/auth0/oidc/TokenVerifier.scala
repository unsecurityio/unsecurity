package unsecurity.auth.auth0.oidc

import java.security.interfaces.{RSAPrivateKey, RSAPublicKey}
import java.time.{Instant, OffsetDateTime, ZoneId, ZoneOffset}

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import com.auth0.jwt.interfaces.RSAKeyProvider
import io.circe.parser._
import org.apache.commons.codec.binary.Base64
import unsecurity.auth.auth0.oidc.Jwt.JwtPayload

object TokenVerifier {

  def decodeBase64(value: String): String = new String(Base64.decodeBase64(value), "UTF-8")

  // Private Key is stored at IdP and not in our application, hence exception throwing
  def createPublicKeyProvider(publicKey: RSAPublicKey): RSAKeyProvider = {
    new RSAKeyProvider {

      override def getPrivateKeyId =
        throw new UnsupportedOperationException(
          "The private key is stored at the IdP and should never hit our app. Use this KeyProvider only for verification, not signing!")

      override def getPublicKeyById(keyId: String): RSAPublicKey = publicKey

      override def getPrivateKey: RSAPrivateKey =
        throw new UnsupportedOperationException(
          "The private key is stored at the IdP and should never hit our app. Use this KeyProvider only for verification, not signing!")
    }
  }

  def validateIdToken(alg: Algorithm,
                      issuer: String,
                      clientId: String,
                      idToken: String): Either[String, OidcAuthenticatedUser] = {
    val verifier = JWT
      .require(alg)
      .withIssuer(issuer)
      .withAudience(clientId)
      .build()

    try {
      val verifiedToken = verifier.verify(idToken)
      decode[JwtPayload](decodeBase64(verifiedToken.getPayload)) match {
        case Left(error) => Left(s"Unable to decode JWT payload: $error")
        case Right(payload) =>
          val expirationTime = OffsetDateTime.from(Instant.ofEpochSecond(payload.exp).atOffset(ZoneOffset.UTC))
          val now            = OffsetDateTime.now(ZoneId.from(ZoneOffset.UTC))

          if (now.isAfter(expirationTime)) {
            Left(s"Token is expired! $now is after expirationTime: $expirationTime")
          } else {
            decode[OidcAuthenticatedUser](decodeBase64(verifiedToken.getPayload)) match {
              case Left(e) =>
                Left(s"Could not parse userprofile from auth0 $e. Payload: ${verifiedToken.getPayload}")
              case Right(profile) =>
                Right(profile)
            }
          }
      }

    } catch {
      case ex: Exception => Left(s"Exception decoding token ${ex.getMessage}")
    }

  }

}
