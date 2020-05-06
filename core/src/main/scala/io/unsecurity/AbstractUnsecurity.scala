package io.unsecurity

import cats.effect.Sync
import fs2.Stream
import io.circe.{Decoder, Encoder}
import io.unsecurity.hlinx.HLinx._
import io.unsecurity.hlinx.{ReversedTupled, SimpleLinx, TransformParams}
import no.scalabin.http4s.directives.{Directive => Http4sDirective}
import org.http4s.EntityEncoder.entityBodyEncoder
import org.http4s._
import org.http4s.headers.`Content-Type`
import shapeless.HList

abstract class AbstractUnsecurity[F[_]: Sync, U] extends AbstractContentTypeMatcher {

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

    def jsonWithMediaType[R: Decoder](mediaType: MediaType): EntityDecoder[F, R] =
      org.http4s.circe.jsonOfWithMedia[F, R](mediaType)

    def raw: EntityDecoder[F, String] =
      implicitly[EntityDecoder[F, String]]
  }

  object Produces {

    def EmptyBody: Unit => ResponseDirective[F] = { _: Unit =>
      Http4sDirective.success(Response[F](Status.NoContent))
    }

    def json[W: Encoder]: W => ResponseDirective[F] = json(Status.Ok)

    def json[W: Encoder] (status: Status): W => ResponseDirective[F] = jsonWithContentType(`Content-Type`(MediaType.application.json), status)

    def jsonWithContentType[W: Encoder](contentType: `Content-Type`, status:Status = Status.Ok): W => ResponseDirective[F] =
      w =>
        Http4sDirective.success(
          Response[F](status)
            .withEntity(w)(org.http4s.circe.jsonEncoderOf[F, W])
            .withContentType(contentType)
      )

    def jsonStream[W: Encoder](status: Status = Status.Ok): Stream[F, W] => ResponseDirective[F] =
      (s: Stream[F, W]) => {
        val encoder                = org.http4s.circe.jsonEncoderOf[F, W]
        val value: Stream[F, Byte] = s.flatMap(w => encoder.toEntity(w).body)

        Http4sDirective.success(
          Response[F](status)
            .withEntity(value)
        )
      }

    def stream[W](status: Status = Status.Ok) (implicit encoder: EntityEncoder[F, Stream[F, W]]): Stream[F, W] => ResponseDirective[F] =
      s => {
        Http4sDirective.success(
          Response[F](status)
            .withEntity(s)
        )
      }

    object F {
      def json[W: Encoder]: F[W] => ResponseDirective[F] = json(Status.Ok)

      def json[W: Encoder](status: Status): F[W] => ResponseDirective[F] = jsonWithContentType(`Content-Type`(MediaType.application.json), status)

      def jsonWithContentType[W: Encoder](contentType: `Content-Type`, status: Status = Status.Ok): F[W] => ResponseDirective[F] = f => {
        Http4sDirective
          .liftF(f)
          .map(
            w =>
              Response[F](status)
                .withEntity(w)(org.http4s.circe.jsonEncoderOf[F, W])
                .withContentType(contentType))
      }

    }

    object Directive {

      def json[E: Encoder]: Http4sDirective[F, E] => ResponseDirective[F] = json[E](Status.Ok)

      def json[E: Encoder](status: Status): Http4sDirective[F, E] => ResponseDirective[F] = jsonWithContentType(`Content-Type`(MediaType.application.json), status)

      def jsonWithContentType[E: Encoder](
          contentType: `Content-Type`, status: Status = Status.Ok): Http4sDirective[F, E] => ResponseDirective[F] = { eDir: Http4sDirective[F, E] =>
        eDir.map(
          e =>
            Response[F](status)
              .withEntity(e)(org.http4s.circe.jsonEncoderOf[F, E])
              .withContentType(contentType))
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
    def mapD[C2](f: C => Http4sDirective[F, C2]): Completable[C2, W]
    def run(f: C => W): Complete
  }

  trait Secured[C, W] {
    def map[C2](f: C => C2): Secured[C2, W]
    def mapF[C2](f: C => F[C2]): Secured[C2, W]
    def mapD[C2](f: C => Http4sDirective[F, C2]): Secured[C2, W]
    def authorization(predicate: C => Boolean,
                      ifUnauthorized: => HttpProblem = HttpProblem.forbidden("Forbidden")): Completable[C, W]
    def noAuthorization: Completable[C, W]
  }

  trait Complete {
    def key: List[SimpleLinx]
    def merge(other: AbstractUnsecurity[F, U]#Complete): AbstractUnsecurity[F, U]#Complete
    def compile: PathMatcher[Response[F]]
    def consumes: Set[MediaRange]
    def methodMap: Map[Method, MediaRangeMap[Any => ResponseDirective[F]]]
  }

}
case class MediaRangeMap[A](mr2a2rdf: List[(Set[MediaRange], A)]) {
  def merge(other: MediaRangeMap[A]): MediaRangeMap[A] = {
    // TODO legg til kollisjonsdeteksjon
    MediaRangeMap(mr2a2rdf ++ other.mr2a2rdf)
  }

  def supportedMediaRanges: Set[MediaRange] = mr2a2rdf.flatMap(_._1.toList).toSet

  def get(mediaRange: MediaRange): Either[Set[MediaRange], A] = {
    mr2a2rdf
      .find {
        case (mrs, _) =>
          mrs.exists { mr =>
            mr.satisfiedBy(mediaRange) && mediaRange.extensions.forall{case (prop, value) => mr.extensions.get(prop).contains(value)}
          }
      }
      .map(_._2)
      .toRight(supportedMediaRanges)
  }
}
