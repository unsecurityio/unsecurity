package io.unsecurity.auth.auth0.oidc

import java.net.URI
import java.security.SecureRandom
import java.security.interfaces.RSAPublicKey
import java.util.concurrent.TimeUnit

import cats.effect.Sync
import com.auth0.client.auth.AuthAPI
import com.auth0.jwk.{GuavaCachedJwkProvider, UrlJwkProvider}
import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import com.auth0.jwt.interfaces.DecodedJWT
import io.circe.{Decoder, Encoder, Json}
import io.unsecurity.{SecurityContext, UnsecurityOps}
import io.unsecurity.auth.auth0.AuthConfig
import no.scalabin.http4s.directives.Directive
import okhttp3.{MediaType, OkHttpClient, Request, RequestBody}
import org.apache.commons.codec.binary.Hex
import org.http4s.{RequestCookie, ResponseCookie, Status}
import org.slf4j.{Logger, LoggerFactory}
import io.circe.syntax._
import io.circe.parser.{decode => cDecode}
import io.unsecurity.auth.auth0.oidc.Jwt.JwtHeader
import okio.ByteString

class Auth0OidcSecurityContext[F[_] : Sync, U](val authConfig: AuthConfig,
                                               val sessionStore: SessionStore[OidcAuthenticatedUser],
                                               lookup: OidcAuthenticatedUser => Option[U]
                                              ) extends SecurityContext[F, OidcAuthenticatedUser, U]
  with UnsecurityOps[F] {
  val log: Logger = LoggerFactory.getLogger(classOf[Auth0OidcSecurityContext[F, U]])
  val client: OkHttpClient = new OkHttpClient

  override def transformUser(u: OidcAuthenticatedUser): Option[U] = lookup(u)

  override def authenticate: Directive[F, OidcAuthenticatedUser] = {
    log.trace("trying to authenticate")
    for {
      sessionCookie <- sessionCookie
      userProfile <- userSession(sessionCookie)
    } yield {
      userProfile
    }
  }

  override def xsrfCheck: Directive[F, String] = {
    for {
      xForwardedFor   <- requestHeader("X-Forwarded-For")
      xsrfHeader      <- xsrfHeader(xForwardedFor.map(_.value))
      xsrfCookie      <- xsrfCookie()
      validatedHeader <- validateXsrf(xsrfHeader, xsrfCookie, xForwardedFor.map(_.value))
    } yield {
      validatedHeader
    }
  }

  def validateXsrf(xsrfHeader: String, xsrfCookievalue: String, xForwardedFor: Option[String]): Directive[F, String] = {
    if (xsrfCookievalue == xsrfHeader) {
      Directive.success(xsrfHeader)
    } else {
      log.error(
        s"XsrfCookie does not match Xsrf header, possible CSRF-Attack! X-Forwarded-For: ${xForwardedFor.getOrElse("")}"
      )
      Directive.failure(
        ResponseJson("xsrf check failed", Status.BadRequest)
      )
    }
  }

  def xsrfHeader(xForwardedFor: Option[String]): Directive[F, String] = {
    for {
      header <- requestHeader("x-xsrf-token")
      xsrfToken <- header match {
        case Some(xsrfToken) => Directive.success(xsrfToken.value)
        case None =>
          log.error("No x-xsrf-token header, possible CSRF-attack!")
          Directive.failure(
            ResponseJson(
              s"No x-xsrf-token header found. X-Forwarded-For: ${xForwardedFor.getOrElse("")}",
              Status.BadRequest
            )
          )
      }
    } yield {
      xsrfToken
    }
  }

  def xsrfCookie(): Directive[F, String] = {
    for {
      cookies       <- requestCookies()
      xForwardedfor <- requestHeader("X-Forwarded-For")
      xsrfCookie <- cookies
        .find(c => c.name == "xsrf-token") match {
        case Some(xstrfCookie) => Directive.success(xstrfCookie.content)
        case None =>
          log.error(s"No xsrf-cookie, possible CSRF-Attack from ${xForwardedfor.map(_.value).getOrElse("")}")
          Directive.failure(
            ResponseJson("No xsrf-token cookie found", Status.BadRequest)
          )
      }
    } yield {
      xsrfCookie
    }
  }

  override def rateLimitCheck(authenticatedIdentity: OidcAuthenticatedUser): Directive[F, Int] = {
    log.trace("rateLimit")
    ???
  }

  def userSession(cookie: RequestCookie): Directive[F, OidcAuthenticatedUser] = {
    sessionStore.getSession(cookie.content) match {
      case Some(user) => Directive.success(user)
      case None =>
        log.warn("Could not extract user profile: {}, session timed out")
        Unauthorized("Could not extract user profile from the cookie")
    }
  }

  def createAuth0Url(state: String, auth0CallbackUrl: URI): String = {
    new AuthAPI(authConfig.authDomain, authConfig.clientId, authConfig.clientSecret)
      .authorizeUrl(auth0CallbackUrl.toString)
      .withScope("openid profile email")
      .withState(state)
      .withResponseType("code")
      .build()
  }

  def validateState(stateCookie: RequestCookie, state: String, xForwardedFor: Option[String]): Directive[F, State] = {
    sessionStore.getState(stateCookie.content) match {
      case None =>
        log.error(s"Invalid state, possible CSRF-attack on login. X-Forwarded-For: ${xForwardedFor.getOrElse("")}")
        BadRequest("Invalid state, possible csrf-attack")
      case Some(sessionState) =>
        if (state == sessionState.state) {
          Directive.success(sessionState)
        } else {
          log.error(
            s"State values does not match, possible XSRF-attack! X-Forwarded-For: ${xForwardedFor.getOrElse("")} "
          )
          BadRequest("Illegal state value")
        }
    }
  }

  def isReturnUrlWhitelisted(uri: URI): Boolean = {
    authConfig.returnToUrlDomainWhitelist.contains(uri.getHost)
  }

  def sessionCookie: Directive[F, RequestCookie] = {
    for {
      cookies <- requestCookies()
      cookie <- cookies
        .find(c => c.name == authConfig.cookieName)
        .map(c => Directive.success(c))
        .getOrElse(
          Directive.failure(ResponseJson("Session cookie not found. Please login", Status.Unauthorized))
        )
    } yield {
      cookie
    }
  }

  def fetchTokenFromAuth0(code: String, auth0CallbackUrl: URI): Directive[F, TokenResponse] = {
    val tokenUrl: String = s"https://${authConfig.authDomain}/oauth/token"
    val jsonMediaType: MediaType = MediaType.parse("application/json; charset=utf-8")
    val payload: String = TokenRequest(grantType = "authorization_code",
      clientId = authConfig.clientId,
      clientSecret = authConfig.clientSecret,
      code = code,
      redirectUri = auth0CallbackUrl).asJson.noSpaces
    val req: Request = new okhttp3.Request.Builder()
      .url(tokenUrl)
      .post(RequestBody.create(jsonMediaType, payload))
      .build()

    val resp = client.newCall(req).execute
    val responseCode: Int = resp.code()
    val body: String = resp.body.string
    resp.body.close()

    if (responseCode == 200) {
      cDecode[TokenResponse](body) match {
        case Right(token) => Directive.success(token)
        case Left(e) =>
          log.error("Error parsing token from auth0 {}. Payload : {}", List(e, body): _*)
          InternalServerError(Json.obj("msg" := "Error parsing token from auth0"))
      }
    } else {
      log.error("Invalid response from auth0, got ({}) {}", responseCode, body)
      InternalServerError(Json.obj("msg" := "Invalid response from IDP"))
    }
  }

  def verifyTokenAndGetOidcUser(tokenResponse: TokenResponse): Directive[F, OidcAuthenticatedUser] = {
    def decodeBase64(value: String): String = ByteString.decodeBase64(value).utf8()

    val numberOfKeys = 10

    val provider: UrlJwkProvider = new UrlJwkProvider(s"https://${authConfig.authDomain}/.well-known/jwks.json")
    val cachedProvider = new GuavaCachedJwkProvider(provider, numberOfKeys, 5L, TimeUnit.HOURS)
    val decodedJwt: DecodedJWT = JWT.decode(tokenResponse.idToken)
    val decodedHeaderString = decodeBase64(decodedJwt.getHeader)
    val decodedEitherHeader: Either[String, JwtHeader] = cDecode[JwtHeader](decodedHeaderString).left.map(_.getMessage)

    val eitherUser: Either[String, OidcAuthenticatedUser] = for {
      header <- decodedEitherHeader
      publicKey = cachedProvider.get(header.kid).getPublicKey.asInstanceOf[RSAPublicKey]
      alg = Algorithm.RSA256(TokenVerifier.createPublicKeyProvider(publicKey))
      oidcUser <- TokenVerifier.validateIdToken(alg, authConfig.authDomain, authConfig.clientId, tokenResponse.idToken)
    } yield {
      oidcUser
    }

    eitherUser.fold(
      errorMessage => InternalServerError(errorMessage),
      user => Directive.success(user)
    )
  }

  def randomString(lengthInBytes: Int): String = {
    val secrand: SecureRandom = new SecureRandom()
    val byteArray: Array[Byte] = Array.fill[Byte](lengthInBytes)(0)
    secrand.nextBytes(byteArray)

    Hex.encodeHexString(byteArray)
  }

  object Cookies {

    object Keys {
      val STATE: String = "statecookie"
      val K_SESSION_ID: String = authConfig.cookieName
      val XSRF: String = "xsrf-token"
    }

    def createXsrfCookie(secureCookie: Boolean): ResponseCookie = {
      val xsrfToken: String = randomString(32)
      log.trace("xsrfToken: {}", xsrfToken)

      ResponseCookie(
        name = Keys.XSRF,
        content = xsrfToken,
        secure = secureCookie,
        httpOnly = false,
        path = Some("/"),
        maxAge = Some(authConfig.sessionCookieTtl.toSeconds)
      )
    }

    def createSessionCookie(secureCookie: Boolean): ResponseCookie = {
      ResponseCookie(
        name = Keys.K_SESSION_ID,
        content = randomString(64),
        secure = secureCookie,
        path = Some("/"),
        httpOnly = true,
        maxAge = Some(authConfig.sessionCookieTtl.toSeconds)
      )
    }

    def createStateCookie(secureCookie: Boolean): ResponseCookie = {
      val stateCookieRef: String = randomString(16)
      log.trace("stateCookieRef: {}", stateCookieRef)

      ResponseCookie(name = Cookies.Keys.STATE, content = stateCookieRef, secure = secureCookie)
    }
  }

}

case class TokenRequest(grantType: String, clientId: String, clientSecret: String, code: String, redirectUri: URI)

object TokenRequest {
  implicit val tokenRequestEncoder: Encoder[TokenRequest] = Encoder { tr =>
    Json.obj(
      "grant_type" := tr.grantType,
      "client_id" := tr.clientId,
      "client_secret" := tr.clientSecret,
      "code" := tr.code,
      "redirect_uri" := tr.redirectUri.toString
    )
  }
}

case class TokenResponse(accessToken: String, expiresIn: Long, idToken: String, tokenType: String)

object TokenResponse {
  implicit val tokenResponseDecoder: Decoder[TokenResponse] = Decoder { c =>
    for {
      accessToken <- c.downField("access_token").as[String]
      expiresIn <- c.downField("expires_in").as[Long]
      idToken <- c.downField("id_token").as[String]
      tokenType <- c.downField("token_type").as[String]
    } yield {
      TokenResponse(
        accessToken,
        expiresIn,
        idToken,
        tokenType
      )
    }
  }
}
