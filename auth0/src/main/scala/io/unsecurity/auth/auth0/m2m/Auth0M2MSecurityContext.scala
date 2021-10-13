package io.unsecurity.auth.auth0.m2m

import cats.effect.Sync
import com.auth0.jwk.JwkProvider
import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import com.auth0.jwt.interfaces.{DecodedJWT, RSAKeyProvider}
import io.circe.parser.decode
import io.unsecurity.auth.auth0.oidc.Jwt.JwtHeader
import io.unsecurity.{HttpProblem, SecurityContext, UnsecurityOps}
import no.scalabin.http4s.directives.Directive
import okio.ByteString
import org.http4s.headers.Authorization
import org.http4s.{Credentials, Method, Uri}
import org.typelevel.ci.CIStringSyntax

import java.security.interfaces.{RSAPrivateKey, RSAPublicKey}
import java.time.{Instant, OffsetDateTime, ZoneId, ZoneOffset}
import scala.util.Try

class Auth0M2MSecurityContext[F[_], U](lookup: OauthAuthenticatedApplication => F[Option[U]], issuer: String, audience: String, jwkProvider: JwkProvider)(
    implicit F: Sync[F])
    extends SecurityContext[F, OauthAuthenticatedApplication, U]
    with UnsecurityOps[F] {

  override def authenticate: Directive[F, OauthAuthenticatedApplication] = {
    for {
      attemptedMethod  <- Directive.request[F].map(_.method)
      attemptedPath    <- request.path
      requestAuthToken <- requestAuthToken(attemptedMethod, attemptedPath)
      decodedJWT       <- decodedJWT(attemptedMethod, attemptedPath, requestAuthToken)
      jwtHeader        <- jwtHeader(attemptedMethod, attemptedPath, decodedJWT)
      alg              <- cryptoAlgorithm(attemptedMethod, attemptedPath, jwtHeader)
      verifiedToken    <- verifyAccessToken(attemptedMethod, attemptedPath, alg, requestAuthToken)
      jwtToken         <- jwtToken(attemptedMethod, attemptedPath, verifiedToken)
      _                <- checkExpiration(attemptedMethod, attemptedPath, jwtToken)
      userProfile      <- extractProfile(jwtToken, requestAuthToken)
    } yield {
      userProfile
    }
  }

  /**
    * For machine 2 machine communications xsrf is not really relevant since it is not a browser that
    * sends the messages. So the implementation of just returning success is sufficient
    *
    * @return success
    */
  override def xsrfCheck: Directive[F, String] = Directive.success("")

  override def transformUser(rawUser: OauthAuthenticatedApplication): F[Option[U]] = lookup(rawUser)

  private[unsecurity] def requestAuthToken(attemptedMethod: Method, attemptedPath: Uri.Path): Directive[F, String] = {
    for {
      authHeader <- request.header[Authorization]
      token <- authHeader
                .map(_.credentials)
                .collect { case Credentials.Token(ci"Bearer", value) => value }
                .toDirective(HttpProblem.unauthorized(s"Authorization header with Bearer scheme not found [$attemptedMethod, $attemptedPath]").toDirectiveError)
    } yield token
  }

  private def decodedJWT(attemptedMethod: Method, attemptedPath: Uri.Path, token: String): Directive[F, DecodedJWT] = {
    Try(JWT.decode(token)).toSuccess { throwable =>
      Unauthorized(s"Could not extract token from request [$attemptedMethod, $attemptedPath, ${throwable.getMessage}]")
    }
  }

  private def cryptoAlgorithm(attemptedMethod: Method, attemptedPath: Uri.Path, jwtHeader: JwtHeader): Directive[F, Algorithm] = {
    Try {
      val publicKey = jwkProvider.get(jwtHeader.kid).getPublicKey.asInstanceOf[RSAPublicKey]
      Algorithm.RSA256(createPublicKeyProvider(publicKey))
    }.toSuccess { throwable =>
      Unauthorized(s"Could not get public key from jwt hedader [$attemptedMethod, $attemptedPath, ${throwable.getMessage}]")
    }
  }

  private def jwtHeader(attemptedMethod: Method, attemptedPath: Uri.Path, jwtToken: DecodedJWT): Directive[F, JwtHeader] = {
    for {
      decodedHeaderString <- decodeBase64(jwtToken.getHeader)
      header              <- decode[JwtHeader](decodedHeaderString).toDirective(_ => Unauthorized(s"Could not decode jwt header [$attemptedMethod, $attemptedPath]"))
    } yield header
  }

  private def decodeBase64(value: String): Directive[F, String] =
    Directive.success(ByteString.decodeBase64(value).utf8())

  // Private Key is stored at IdP and not in our application, hence exception throwing
  private def createPublicKeyProvider(publicKey: RSAPublicKey): RSAKeyProvider = {
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

  private def verifyAccessToken(attemptedMethod: Method, attemptedPath: Uri.Path, alg: Algorithm, accessToken: String): Directive[F, DecodedJWT] = {
    val verifier = JWT
      .require(alg)
      .withIssuer(issuer)
      .withAudience(audience)
      .build()
    Try {
      verifier.verify(accessToken)
    }.toSuccess { throwable =>
      Unauthorized(s"Could not verify token path: [$attemptedMethod, $attemptedPath, ${throwable.getMessage}]")
    }
  }

  private def jwtToken(attemptedMethod: Method, attemptedPath: Uri.Path, verifiedToken: DecodedJWT): Directive[F, JwtToken] = {
    for {
      base64Token <- decodeBase64(verifiedToken.getPayload)
      jwtToken <- decode[JwtToken](base64Token).toDirective(decodeError =>
                   Unauthorized(s"Unable to decode JWT payload: [$attemptedMethod, $attemptedPath, $decodeError]"))
    } yield {
      jwtToken
    }
  }

  private def checkExpiration(attemptedMethod: Method, attemptedPath: Uri.Path, jwtToken: JwtToken): Directive[F, String] = {
    val expirationTime = OffsetDateTime.from(Instant.ofEpochSecond(jwtToken.exp).atOffset(ZoneOffset.UTC))
    val now            = OffsetDateTime.now(ZoneId.from(ZoneOffset.UTC))
    if (now.isAfter(expirationTime)) {
      Unauthorized(
        s"Token is expired! $now is after expirationTime: $expirationTime for [sub ${jwtToken.sub}, iss ${jwtToken.iss}, aud ${jwtToken.aud}, method $attemptedMethod, path $attemptedPath]")
    } else {
      Directive.success("Valid token")
    }
  }

  private def extractProfile(jwtToken: JwtToken, rawToken: String): Directive[F, OauthAuthenticatedApplication] = {
    Directive.success(
      OauthAuthenticatedApplication(
        ApplicationId(jwtToken.sub),
        jwtToken.scopes,
        rawToken
      ))
  }
}
