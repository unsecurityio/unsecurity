package io.unsecurity

import cats.effect.Sync
import fs2.Stream
import io.circe.{Decoder, Encoder}
import io.unsecurity.hlinx.HLinx._
import io.unsecurity.hlinx.{ReversedTupled, SimpleLinx, TransformParams}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.http4s.EntityEncoder.entityBodyEncoder
import org.http4s.headers.`Content-Type`
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, MediaType, Method, Response, Status}
import org.slf4j.Logger
import shapeless.HList

import scala.Ordering.Implicits._

abstract class AbstractUnsecurity[F[_]: Sync, U] {

  case class Endpoint[P <: HList, R, W](description: String = "",
                                        method: Method,
                                        path: HLinx[P],
                                        accepts: EntityDecoder[F, R],
                                        produces: W => ResponseDirective[F])
  object Endpoint {
    def apply[P <: HList, R, W](desc: String, method: Method, path: HLinx[P]) =
      new Endpoint[P, Unit, Http4sDirective[F, Unit]](desc,
                                                      method,
                                                      path,
                                                      Accepts.EmptyBody,
                                                      Produces.Directive.EmptyBody)

    def apply[P <: HList, W](desc: String, method: Method, path: HLinx[P], produces: W => ResponseDirective[F]) =
      new Endpoint[P, Unit, W](desc, method, path, Accepts.EmptyBody, produces)

    def apply[P <: HList, R](desc: String, method: Method, path: HLinx[P], accepts: EntityDecoder[F, R]) =
      new Endpoint[P, R, Http4sDirective[F, Unit]](desc, method, path, accepts, Produces.Directive.EmptyBody)
  }

  def log: Logger

  type PathMatcher[A] = PartialFunction[String, Http4sDirective[F, A]]

  def secure[P <: HList, R, W, TUP, TUP2](endpoint: Endpoint[P, R, W])(
      implicit revTup: ReversedTupled.Aux[P, TUP],
      transformParams: TransformParams.Aux[TUP, (R, U), TUP2]
  ): Secured[TUP2, W]

  def unsecure[P <: HList, R, W, TUP, TUP2](endpoint: Endpoint[P, R, W])(
      implicit revTup: ReversedTupled.Aux[P, TUP],
      transformParams: TransformParams.Aux[TUP, Tuple1[R], TUP2]
  ): Completable[TUP2, W]

  object Accepts {
    def EmptyBody: EntityDecoder[F, Unit] =
      implicitly[EntityDecoder[F, Unit]]

    def json[R: Decoder]: EntityDecoder[F, R] =
      org.http4s.circe.jsonOf[F, R]

    def raw: EntityDecoder[F, String] =
      implicitly[EntityDecoder[F, String]]
  }

  object Produces {

    def EmptyBody: Unit => ResponseDirective[F] = { _: Unit =>
      Http4sDirective.success(Response[F](Status.NoContent))
    }

    def json[W: Encoder]: W => ResponseDirective[F] =
      w =>
        Http4sDirective.success(
          Response[F](Status.Ok)
            .withContentType(`Content-Type`(MediaType.application.json))
            .withEntity(w)(org.http4s.circe.jsonEncoderOf[F, W])
      )

    def jsonStream[W: Encoder]: Stream[F, W] => ResponseDirective[F] =
      (s: Stream[F, W]) => {
        val encoder                = org.http4s.circe.jsonEncoderOf[F, W]
        val value: Stream[F, Byte] = s.flatMap(w => encoder.toEntity(w).body)

        Http4sDirective.success(
          Response[F](Status.Ok)
            .withEntity(value)
        )
      }

    def stream[W](implicit encoder: EntityEncoder[F, Stream[F, W]]): Stream[F, W] => ResponseDirective[F] =
      s => {
        Http4sDirective.success(
          Response[F](Status.Ok)
            .withEntity(s)
        )
      }

    object F {
      def json[W: Encoder]: F[W] => ResponseDirective[F] = f => {
        Http4sDirective
          .liftF(f)
          .map(
            w =>
              Response[F](Status.Ok)
                .withEntity(w)(org.http4s.circe.jsonEncoderOf[F, W]))
      }
    }

    object Directive {
      def json[E: Encoder]: Http4sDirective[F, E] => ResponseDirective[F] = { eDir: Http4sDirective[F, E] =>
        eDir.map(
          e =>
            Response[F](Status.Ok)
              .withEntity(e)(org.http4s.circe.jsonEncoderOf[F, E]))

      }

      val EmptyBody: Http4sDirective[F, Unit] => ResponseDirective[F] = { unitDir =>
        unitDir.map { _: Unit =>
          Response[F](Status.NoContent)
        }
      }
    }
  }

  trait Completable[C, W] {
    def map[C2](f: C => C2): Completable[C2, W]
    def mapF[C2](f: C => F[C2]): Completable[C2, W]
    def run(f: C => W): Complete
  }

  trait Secured[C, W] {
    def map[C2](f: C => C2): Secured[C2, W]
    def mapF[C2](f: C => F[C2]): Secured[C2, W]
    def authorization(predicate: C => Boolean): Completable[C, W]
    def noAuthorization: Completable[C, W]
  }

  trait Complete {
    def key: List[SimpleLinx]
    def merge(other: AbstractUnsecurity[F, U]#Complete): AbstractUnsecurity[F, U]#Complete
    def methodMap: Map[Method, Any => ResponseDirective[F]]
    def compile: PathMatcher[Response[F]]
  }

  def toHttpRoutes(endpoints: List[AbstractUnsecurity[F, U]#Complete]): HttpRoutes[F] = {
    val linxesToList: Map[List[SimpleLinx], List[AbstractUnsecurity[F, U]#Complete]] = endpoints.groupBy(_.key)

    val mergedRoutes: List[AbstractUnsecurity[F, U]#Complete] =
      linxesToList.toList
        .map {
          case (_, groupedEndpoints) => groupedEndpoints.reduce(_ merge _)
        }
        .sortBy(_.key)

    log.trace("Ordered and grouped endpoints:")
    mergedRoutes.foreach { r =>
      log.info(
        s"""/${r.key.mkString("/")}: ${r.methodMap.keys.map { _.name }.mkString(", ")}"""
      )
    }

    val compiledRoutes: List[PathMatcher[Response[F]]] =
      mergedRoutes.map(_.compile)

    val reducedRoutes: PathMatcher[Response[F]] = compiledRoutes.reduce(_ orElse _)

    val PathMapping = UnsecurityPlan[F](log).PathMapping

    val service: HttpRoutes[F] = HttpRoutes.of[F](
      PathMapping(reducedRoutes)
    )

    service
  }
}
