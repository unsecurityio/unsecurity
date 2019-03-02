package io
package unsecurity

import cats.effect.Sync
import io.unsecurity.hlinx.HLinx.HLinx
import io.unsecurity.hlinx.{ReversedTupled, SimpleLinx, TransformParams}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, Method, Response, Status}
import org.slf4j.{Logger, LoggerFactory}
import shapeless.HList

abstract class Unsecurity[F[_]: Sync, RU, U] extends AbstractUnsecurity[F, U] with UnsecurityOps[F] {

  override val log: Logger = LoggerFactory.getLogger(classOf[Unsecurity[F, RU, U]])

  def sc: SecurityContext[F, RU, U]

  case class MySecured[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: W => ResponseDirective[F]
  ) extends Secured[C, W] {
    override def authorization(predicate: C => Boolean): Completable[C, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues(
          a2dc =>
            a2dc.andThen(
              dc =>
                Directive.commit(
                  dc.filter(
                    c => predicate(c).orF(Sync[F].pure(Response[F](Status.Forbidden)))
                  ))
          )),
        entityEncoder = entityEncoder
      )
    }
    override def resolve[C2](f: C => C2): Secured[C2, W] = {
      MySecured(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues { a2dc =>
          a2dc.andThen { dc =>
            dc.map(c => f(c))
          }
        },
        entityEncoder = entityEncoder,
      )
    }
    def noAuthorization: Completable[C, W] =
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap,
        entityEncoder = entityEncoder
      )
  }

  override def secure[P <: HList, R, W, TUP, TUP2](endpoint: Endpoint[P, R, W])(
      implicit reversedTupled: ReversedTupled.Aux[P, TUP],
      transformParams: TransformParams.Aux[TUP, (R, U), TUP2]
  ): Secured[TUP2, W] = {
    MySecured[TUP2, W](
      key = endpoint.path.toSimple.reverse,
      pathMatcher = createPathMatcher(endpoint.path).asInstanceOf[PathMatcher[Any]],
      methodMap = Map(
        endpoint.method -> { tup: TUP =>
          val checkXsrfOrNothing: Directive[F, String] =
            if (endpoint.method == Method.PUT ||
                endpoint.method == Method.POST ||
                endpoint.method == Method.DELETE) {
              sc.xsrfCheck
            } else {
              Directive.success("xsrf not checked")
            }

          implicit val entityDecoder: EntityDecoder[F, R] = endpoint.accepts
          for {
            _       <- checkXsrfOrNothing
            r       <- request.bodyAs[F, R]
            rawUser <- sc.authenticate
            user    <- sc.transformUser(rawUser).toSuccess(Directive.error(Response[F](Status.Unauthorized)))
          } yield {
            transformParams(tup, (r, user))
          }
        }.asInstanceOf[Any => Directive[F, TUP2]]
      ),
      entityEncoder = endpoint.produces
    )
  }

  override def unsecure[P <: HList, R, W, TUP, TUP2](endpoint: Endpoint[P, R, W])(
      implicit revGen: ReversedTupled.Aux[P, TUP],
      transformParam: TransformParams.Aux[TUP, Tuple1[R], TUP2]
  ): Completable[TUP2, W] = {
    MyCompletable[TUP2, W](
      key = endpoint.path.toSimple.reverse,
      pathMatcher = createPathMatcher[P, TUP](endpoint.path).asInstanceOf[PathMatcher[Any]],
      methodMap = Map(
        endpoint.method -> { tup: TUP =>
          implicit val entityDecoder: EntityDecoder[F, R] = endpoint.accepts
          for {
            r <- request.bodyAs[F, R]
          } yield {
            transformParam(tup, Tuple1(r))
          }
        }.asInstanceOf[Any => Directive[F, TUP2]]
      ),
      entityEncoder = endpoint.produces
    )
  }

  case class MyCompletable[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: W => ResponseDirective[F]
  ) extends Completable[C, W] {
    override def run(f: C => W): Complete = {
      MyComplete(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues { a2dc =>
          a2dc.andThen { dc =>
            for {
              c <- dc
              w <- entityEncoder(f(c))
            } yield {
              w
            }
          }
        }
      )
    }

    override def resolve[C2](f: C => C2): Completable[C2, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues { a2dc =>
          a2dc.andThen { dc =>
            dc.map(c => f(c))
          }
        },
        entityEncoder = entityEncoder
      )
    }
  }

  case class MyComplete(
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[Any],
      methodMap: Map[Method, Any => ResponseDirective[F]]
  ) extends Complete {
    override def merge(other: AbstractUnsecurity[F, U]#Complete): AbstractUnsecurity[F, U]#Complete = {
      this.copy(
        methodMap = this.methodMap ++ other.methodMap
      )
    }
    override def compile: PathMatcher[Response[F]] = {
      def allow(methods: List[Method]): Allow = Allow(methods.head, methods.tail: _*)

      pathMatcher.andThen { pathParamsDirective =>
        for {
          req        <- Directive.request
          pathParams <- pathParamsDirective
          res <- if (methodMap.isDefinedAt(req.method)) methodMap(req.method)(pathParams)
                else Directive.error(Response[F](Status.MethodNotAllowed).putHeaders(allow(methodMap.keySet.toList)))
        } yield {
          res
        }
      }
    }
  }

  def createPathMatcher[PathParams <: HList, TUP](route: HLinx[PathParams])(
      implicit revTup: ReversedTupled.Aux[PathParams, TUP]): PathMatcher[TUP] =
    new PartialFunction[String, Directive[F, TUP]] {
      override def isDefinedAt(x: String): Boolean = {
        if (route.capture(x).isDefined) {
          log.trace(s"""Match: "$x" = /${route.toSimple.reverse.mkString("/")}""")
          true
        } else {
//          log.trace(s"""Not match: "$x" != ${route.toSimple.reverse.mkString("/")}""")
          false
        }
      }

      override def apply(v1: String): Directive[F, TUP] = {
        val value: Either[String, TUP] = route.capture(v1).get

        value match {
          case Left(errorMsg) =>
            Directive.failure(
              Response(Status.BadRequest)
                .withEntity(errorMsg)
            )

          case Right(params) =>
            Directive.success(params)

        }
      }
    }
}
