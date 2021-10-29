package io.unsecurity

import cats.effect.Sync
import io.unsecurity.hlinx.SimpleLinx
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.http4s.{HttpRoutes, Query, Response, Uri}
import org.log4s.getLogger

import scala.Ordering.Implicits._
import scala.math.Ordering

object Server {

  private[this] val log = getLogger

  def toHttpRoutes[U, F[_]: Sync](endpoints: Complete[F]*): HttpRoutes[F] = toHttpRoutes(endpoints.toList)

  def toHttpRoutes[U, F[_]: Sync](endpoints: List[Complete[F]]): HttpRoutes[F] = {
    type PathMatcher[A] = PartialFunction[(Uri.Path, Query), Http4sDirective[F, A]]

    val linxesToList: Map[(List[SimpleLinx], List[String]), List[Complete[F]]] =
      endpoints.groupBy(e => e.key -> e.queryParams)

    val mergedRoutes: List[Complete[F]] =
      linxesToList.toList
        .map {
          case (_, groupedEndpoints) =>
            groupedEndpoints.reduce(_ merge _)
        }
        .sortBy(r => r.key -> r.queryParams)(
          Ordering.Tuple2(Ordering[List[SimpleLinx]], Ordering[List[String]].reverse)
        )

    log.trace("Ordered and grouped endpoints:")
    mergedRoutes.foreach { r =>
      val query = if r.queryParams.isEmpty then "" else s"?${r.queryParams.mkString("&")}"
      log.info(
        s"""/${r.key.mkString("/")}$query: ${r.methodMap.keys.map { _.name }.mkString(", ")}"""
      )
    }

    val compiledRoutes: List[PathMatcher[Response[F]]] = mergedRoutes.map(_.compile)
    val reducedRoutes: PathMatcher[Response[F]] = compiledRoutes.reduce(_ orElse _)
    val PathMapping = new UnsecurityPlan[F].PathMapping

    HttpRoutes.of[F](PathMapping(reducedRoutes))
  }
}
