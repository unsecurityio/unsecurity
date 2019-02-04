package io.unsecurity.auth
package auth0

case class Minutes(asInt: Int) {
  def toSeconds: Int = asInt * 60
}
