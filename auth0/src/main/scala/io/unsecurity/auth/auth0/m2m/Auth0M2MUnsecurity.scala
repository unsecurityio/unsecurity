package io.unsecurity.auth.auth0.m2m

import cats.effect.Sync
import io.unsecurity.Unsecurity2

class Auth0M2MUnsecurity[F[_]: Sync, U](val sc: Auth0M2MSecurityContext[F])
    extends Unsecurity2[F, OauthAuthenticatedApplication, OauthAuthenticatedApplication]
