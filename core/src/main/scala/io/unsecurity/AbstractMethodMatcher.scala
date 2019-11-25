package io.unsecurity

import cats.Monad
import cats.data.NonEmptyList
import no.scalabin.http4s.directives.Directive
import org.http4s.Method
import org.http4s.headers.Allow

abstract class AbstractMethodMatcher[F[_]: Monad] extends AbstractPathMatcher[F] with UnsecurityOps[F] {

  def matchMethod[A](methodMap: Map[Method, A]): Directive[F, A] = {
    def allow(methods: Set[Method]): Allow = Allow(NonEmptyList.fromListUnsafe(methods.toList))

    for {
      req <- Directive.request[F]
      a <- methodMap
            .get(req.method)
            .toSuccess(
              Directive.error(
                HttpProblem
                  .methodNotAllowed("Method not allowed", methodMap.keySet)
                  .toResponse
                  .putHeaders(allow(methodMap.keySet))
              )
            )
    } yield {
      a
    }
  }

}
