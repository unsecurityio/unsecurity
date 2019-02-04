package io.unsecurity

import no.scalabin.http4s.directives.Directive

trait SecurityContext[F[_], RU, U] {
  def authenticate: Directive[F, RU]
  def xsrfCheck: Directive[F, String]
  def rateLimitCheck(authenticatedIdentity: RU): Directive[F, Int]
  def transformUser(rawUser: RU): Option[U]
}
