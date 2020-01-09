package io.unsecurity.auth
package auth0
package oidc

import scala.concurrent.duration.FiniteDuration

trait SessionStore[F[_], A] {
  def storeState(stateRef: String, state: State, ttl: Option[FiniteDuration] = None): F[Unit]
  def getState(stateRef: String): F[Option[State]]
  def removeState(stateRef: String): F[Unit]
  def storeSession(sessionKey: String, session: A, ttl: Option[FiniteDuration] = None): F[Unit]
  def getSession(sessionKey: String): F[Option[A]]
  def removeSession(sessionKey: String): F[Unit]
}
