package io.unsecurity.auth.auth0.m2m

import cats.effect.{Async, Sync}
import io.unsecurity.Unsecurity

class Auth0M2MUnsecurity[F[_]: Async, U](val sc: Auth0M2MSecurityContext[F, U])
    extends Unsecurity[F, OauthAuthenticatedApplication, U]
