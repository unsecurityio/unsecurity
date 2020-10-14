package io.unsecurity.auth
package auth0
package oidc

import java.net.URI

import cats.effect.Sync
import com.auth0.jwk.JwkProvider
import io.unsecurity.Unsecurity
import io.unsecurity.hlinx.HLinx.HPath
import io.unsecurity.hlinx.ParamConverter
import no.scalabin.http4s.directives.Directive
import org.http4s.{Method, Response, ResponseCookie}
import org.log4s.getLogger
import shapeless.HNil

class Auth0OidcUnsecurity[F[_]: Sync, U](baseUrl: HPath[HNil],
                                         val sc: Auth0OidcSecurityContext[F, U],
                                         jwkProvider: JwkProvider)
    extends Unsecurity[F, OidcAuthenticatedUser, U] {

  private[this] val log = getLogger

  implicit val uriParamConverter = ParamConverter.createSimple(s => new URI(s))

  val login =
    unsecure(
      Endpoint(
        "oidc login endpoint",
        Method.GET,
        baseUrl / "login",
        Produces.Directive.EmptyBody
      )
    ).run(
      _ =>
        for {
          returnToUrlParam      <- queryParamAs[URI]("next")
          _                     = log.trace(s"/login returnToUrlParam: $returnToUrlParam")
          auth0CallbackUrlParam <- queryParamAs[URI]("auth0Callback")
          _                     = log.trace(s"/login auth0CallbackUrlParam: $auth0CallbackUrlParam")
          state                 <- sc.randomString(32).toDirective
          callbackUrl           = auth0CallbackUrlParam.getOrElse(sc.authConfig.defaultAuth0CallbackUrl)
          returnToUrl           = returnToUrlParam.getOrElse(sc.authConfig.defaultReturnToUrl)
          stateCookie <- sc.Cookies
                          .createStateCookie(secureCookie = callbackUrl.getScheme.equalsIgnoreCase("https"))
                          .toDirective
          _        <- sc.sessionStore.storeState(stateCookie.content, State(state, returnToUrl, callbackUrl, "")).toDirective
          auth0Url = sc.createAuth0Url(state, callbackUrl)
          _        <- break(Redirect(auth0Url).addCookie(stateCookie))
        } yield {
          ()
      }
    )

  val callback =
    unsecure(
      Endpoint(
        "oidc callback endpoint",
        method = Method.GET,
        path = baseUrl / "callback",
        Produces.Directive.EmptyBody
      )
    ).run(
      _ =>
        for {
          stateCookie   <- cookie(sc.Cookies.Keys.STATE)
          stateParam    <- requiredQueryParam("state")
          xForwardedFor <- request.header("X-Forwarded-For")
          state         <- sc.validateState(stateCookie, stateParam, xForwardedFor.map(_.value))
          _             = log.trace("/callback state cookie matches state param")
          codeParam     <- requiredQueryParam("code")
          _             = log.trace(s"/callback callbackUrl: ${state.callbackUrl}")
          token         <- sc.fetchTokenFromAuth0(codeParam, state.callbackUrl)
          oidcUser      <- sc.verifyTokenAndGetOidcUser(token, jwkProvider)
          _             = log.trace(s"/callback userProfile: $oidcUser")
          sessionCookie <- sc.Cookies
                            .createSessionCookie(
                              secureCookie = state.callbackUrl.getScheme.equalsIgnoreCase("https")
                            )
                            .toDirective
          _ <- sc.sessionStore.storeSession(sessionCookie.content, oidcUser).toDirective
          returnToUrl = if (sc.isReturnUrlWhitelisted(state.returnToUrl)) {
            state.returnToUrl
          } else {
            log.warn(
              s"/callback returnToUrl (${state.returnToUrl}) not whitelisted; falling back to ${sc.authConfig.defaultReturnToUrl}"
            )
            sc.authConfig.defaultReturnToUrl
          }
          xsrf <- sc.Cookies
                   .createXsrfCookie(secureCookie = returnToUrl.getScheme.equalsIgnoreCase("https"))
                   .toDirective
          _ <- sc.sessionStore.removeState(stateCookie.content).toDirective
          _ <- break(
                Redirect(returnToUrl)
                  .addCookie(ResponseCookie(name = sc.Cookies.Keys.STATE, content = "", maxAge = Option(-1)))
                  .addCookie(sessionCookie)
                  .addCookie(xsrf)
              )
        } yield {
          ()
      }
    )

  val logout =
    secure(
      Endpoint(
        "oidc logout endpoint",
        Method.POST,
        baseUrl / "logout",
        Produces.Directive.EmptyBody
      )
    ).noAuthorization
      .run(
        _ =>
          for {
            cookie <- sc.sessionCookie
            _      <- sc.sessionStore.removeSession(cookie.content).toDirective
            _ <- break(
                  Redirect(sc.authConfig.afterLogoutUrl)
                    .addCookie(
                      ResponseCookie(name = sc.Cookies.Keys.K_SESSION_ID, content = "", maxAge = Option(-1))
                    )
                    .addCookie(
                      ResponseCookie(name = sc.Cookies.Keys.XSRF, content = "", maxAge = Option(-1), httpOnly = false)
                    )
                )
          } yield {
            ()
        }
      )

  val endpoints = List(login, callback, logout)

  def break(response: => Response[F]): Directive[F, Response[F]] = {
    Directive.error[F, Response[F]](response)
  }

}
