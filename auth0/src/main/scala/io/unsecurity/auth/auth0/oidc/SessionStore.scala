package io.unsecurity.auth
package auth0
package oidc

import java.net.URI

trait SessionStore[A] {
  def storeState(stateRef: String, state: String, returnToUrl: URI, callbackUrl: URI): Unit
  def getState(stateRef: String): Option[State]
  def removeState(stateRef: String): Unit
  def storeSession(key: String, content: A): Unit
  def getSession(key: String): Option[A]
  def removeSession(key: String): Unit
}
