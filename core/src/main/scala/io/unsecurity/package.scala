package io

import org.http4s.Response
import no.scalabin.http4s.directives.{Directive => Http4sDirective}

package object unsecurity {
  type ResponseDirective[F[_]] = Http4sDirective[F, Response[F]]
}
