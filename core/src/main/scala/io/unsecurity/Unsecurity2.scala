package io
package unsecurity

import cats.Monad
import cats.effect.Sync
import io.unsecurity.hlinx.HLinx.{HLinx, HList, SimpleLinx}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, Method, Response, Status}
import org.slf4j.{Logger, LoggerFactory}

abstract class Unsecurity2[F[_]: Sync, RU, U] extends AbstractUnsecurity2[F, U] with UnsecurityOps[F] {

  override val log: Logger = LoggerFactory.getLogger(classOf[Unsecurity2[F, RU, U]])

  def sc: SecurityContext[F, RU, U]

  case class MySecured[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: EntityEncoder[F, W]
  ) extends Secured[C, W] {
    override def authorization(predicate: C => Boolean): Completable[C, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues(
          a2dc =>
            a2dc.andThen(
              dc =>
                dc.flatMap(
                  c =>
                    if (predicate(c)) {
                      Directive.success(c)
                    } else {
                      Directive.error(
                        Response[F]()
                          .withStatus(Status.Forbidden)
                      )
                  }
              )
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
    override def run(f: C => Directive[F, W]): Complete = {
      MyComplete(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues(
          a2dc =>
            a2dc.andThen(
              dc =>
                for {
                  c <- dc
                  w <- f(c)
                } yield {
                  Response[F]()
                    .withEntity(w)(entityEncoder)
              }
          )
        )
      )
    }
  }

  override def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Secured[(P, R, U), W] = {
    MySecured[(P, R, U), W](
      key = endpoint.path.toSimple.reverse,
      pathMatcher = createPathMatcher(endpoint.path).asInstanceOf[PathMatcher[F, Any]],
      methodMap = Map(
        endpoint.method -> { pp: P =>
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
            (pp, r, user)
          }
        }.asInstanceOf[Any => Directive[F, (P, R, U)]]
      ),
      entityEncoder = endpoint.produces
    )
  }

  override def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[(P, R), W] = {
    MyCompletable[(P, R), W](
      key = endpoint.path.toSimple.reverse,
      pathMatcher = createPathMatcher[F, P](endpoint.path).asInstanceOf[PathMatcher[F, Any]],
      methodMap = Map(
        endpoint.method -> { pp: P =>
          implicit val entityDecoder: EntityDecoder[F, R] = endpoint.accepts
          for {
            r <- request.bodyAs[F, R]
          } yield {
            (pp, r)
          }
        }.asInstanceOf[Any => Directive[F, (P, R)]]
      ),
      entityEncoder = endpoint.produces
    )
  }

  case class MyCompletable[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: EntityEncoder[F, W]
  ) extends Completable[C, W] {
    override def run(f: C => Directive[F, W]): Complete = {
      MyComplete(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues {
          //noinspection ScalaUnnecessaryParentheses
          a2dc: (Any => Directive[F, C]) =>
            a2dc.andThen { dc =>
              for {
                c <- dc
                w <- f(c)
              } yield {
                Response[F]()
                  .withEntity(w)(entityEncoder)
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
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => ResponseDirective[F]]
  ) extends Complete {
    override def merge(other: AbstractUnsecurity2[F, U]#Complete): AbstractUnsecurity2[F, U]#Complete = {
      this.copy(
        methodMap = this.methodMap ++ other.methodMap
      )
    }
    override def compile: PathMatcher[F, Response[F]] = {
      def allow(methods: List[Method]): Allow = Allow(methods.head, methods.tail: _*)

      pathMatcher.andThen { pathParamsDir =>
        for {
          req        <- Directive.request
          pathParams <- pathParamsDir
          res <- if (methodMap.isDefinedAt(req.method)) methodMap(req.method)(pathParams)
                else Directive.error(Response[F](Status.MethodNotAllowed).putHeaders(allow(methodMap.keySet.toList)))
        } yield {
          res
        }
      }
    }
  }

  def createPathMatcher[F[_]: Monad, PathParams <: HList](route: HLinx[PathParams]): PathMatcher[F, PathParams] =
    new PartialFunction[String, Directive[F, PathParams]] {
      override def isDefinedAt(x: String): Boolean = {
        if (route.capture(x).isDefined) {
          log.trace(s"""Match: "$x" = /${route.toSimple.reverse.mkString("/")}""")
          true
        } else {
//          log.trace(s"""Not match: "$x" != ${route.toSimple.reverse.mkString("/")}""")
          false
        }
      }

      override def apply(v1: String): Directive[F, PathParams] = {
        val value: Either[String, PathParams] = route.capture(v1).get

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
