package io.unsecurity.auth.auth0.oidc

import java.net.URI

import scala.concurrent.duration.FiniteDuration

case class AuthConfig(clientId: String,
                      clientSecret: String,
                      issuer: String,
                      defaultAuth0CallbackUrl: URI,
                      defaultReturnToUrl: URI,
                      returnToUrlDomainWhitelist: List[String],
                      afterLogoutUrl: URI,
                      sessionCookieTtl: Option[FiniteDuration],
                      cookieName: String)
