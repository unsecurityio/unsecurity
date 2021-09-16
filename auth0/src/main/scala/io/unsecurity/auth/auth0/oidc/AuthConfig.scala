package io.unsecurity.auth.auth0.oidc

import java.net.{URL, URI}

import scala.concurrent.duration.FiniteDuration

case class AuthConfig(clientId: String,
                      clientSecret: String,
                      issuer: String,
                      defaultAuth0CallbackUrl: URL,
                      defaultReturnToUrl: URL,
                      returnToUrlDomainWhitelist: List[String],
                      afterLogoutUrl: URL,
                      sessionCookieTtl: Option[FiniteDuration],
                      cookieName: String)

object AuthConfig {
  def apply(clientId: String,
            clientSecret: String,
            issuer: String,
            defaultAuth0CallbackUrl: URI,
            defaultReturnToUrl: URI,
            returnToUrlDomainWhitelist: List[String],
            afterLogoutUrl: URI,
            sessionCookieTtl: Option[FiniteDuration],
            cookieName: String): AuthConfig = AuthConfig(
    clientId,
    clientSecret,
    issuer,
    defaultAuth0CallbackUrl.toURL,
    defaultReturnToUrl.toURL,
    returnToUrlDomainWhitelist,
    afterLogoutUrl.toURL,
    sessionCookieTtl,
    cookieName)
}