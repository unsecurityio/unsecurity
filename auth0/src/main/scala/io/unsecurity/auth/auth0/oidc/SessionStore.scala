package io.unsecurity.auth
package auth0
package oidc

import java.net.URI

trait SessionStore[F[_], A] {
  def storeState(stateRef: String,
                 state: String,
                 returnToUrl: URI,
                 callbackUrl: URI,
                 additionalData: String = "",
                 ttl: Option[Minutes] = None): F[Unit]
  def getState(stateRef: String): F[Option[State]]
  def removeState(stateRef: String): F[Unit]
  def storeSession(key: String, content: A, ttl: Option[Minutes] = None): F[Unit]
  def getSession(key: String): F[Option[A]]
  def removeSession(key: String): F[Unit]
}
