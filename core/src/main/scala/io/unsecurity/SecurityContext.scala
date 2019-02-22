package io.unsecurity

import no.scalabin.http4s.directives.Directive

trait SecurityContext[F[_], RU, U] {
  def authenticate: Directive[F, RU]
  def xsrfCheck: Directive[F, String]
  def transformUser(rawUser: RU): Option[U]
}
