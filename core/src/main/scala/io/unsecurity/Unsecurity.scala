package io
package unsecurity

import cats.effect.Sync
import io.unsecurity.hlinx.{ReversedTupled, SimpleLinx, TransformParams}
import no.scalabin.http4s.directives.Directive
import org.http4s.headers.Allow
import org.http4s.{DecodeFailure, MediaRange, Method, Response}
import shapeless.HList

abstract class Unsecurity[F[_]: Sync, RU, U] extends AbstractUnsecurity[F, U] {

  def sc: SecurityContext[F, RU, U]

  case class MySecured[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[Any],
      consumes: Set[MediaRange],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: W => ResponseDirective[F]
  ) extends Secured[C, W] {
    override def authorization(
        predicate: C => Boolean,
        ifUnauthorized: () => HttpProblem = () => HttpProblem.forbidden("Forbidden")): Completable[C, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method ->
              a2dc.andThen(
                dc =>
                  Directive.commit(
                    dc.filter(
                      c => predicate(c).orF(ifUnauthorized().toResponseF)
                    ))
              )
        },
        entityEncoder = entityEncoder
      )
    }
    override def mapD[C2](f: C => Directive[F, C2]): Secured[C2, W] = {
      MySecured(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c))
            }
        },
        entityEncoder = entityEncoder,
      )
    }
    override def map[C2](f: C => C2): Secured[C2, W] = {
      MySecured(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.map(c => f(c))
            }
        },
        entityEncoder = entityEncoder,
      )
    }
    override def mapF[C2](f: C => F[C2]): Secured[C2, W] = {
      MySecured(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c).successF)
            }
        },
        entityEncoder = entityEncoder,
      )
    }
    def noAuthorization: Completable[C, W] =
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
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
      consumes = endpoint.accepts.consumes,
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

          for {
            _       <- checkXsrfOrNothing
            rawUser <- sc.authenticate
            user <- Directive.commit(
                     Directive.getOrElseF(
                       sc.transformUser(rawUser),
                       HttpProblem.unauthorized("Unauthorized").toResponseF[F]
                     )
                   )
            r <- request.bodyAs[R] { error: DecodeFailure =>
                  HttpProblem.handleAndLogError(error).toResponse[F]
                }(endpoint.accepts, Sync[F])
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
      consumes = endpoint.accepts.consumes,
      methodMap = Map(
        endpoint.method -> { tup: TUP =>
          for {
            r <- request.bodyAs[R] { error: DecodeFailure =>
                  HttpProblem.handleAndLogError(error).toResponse[F]
                }(endpoint.accepts, Sync[F])
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
      consumes: Set[MediaRange],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: W => ResponseDirective[F]
  ) extends Completable[C, W] {
    override def run(f: C => W): Complete = {
      MyComplete(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> MediaRangeMap(List((consumes, a2dc.andThen { dc =>
              for {
                c <- dc
                w <- entityEncoder(f(c))
              } yield {
                w
              }
            })))
        }
      )
    }

    override def map[C2](f: C => C2): Completable[C2, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.map(c => f(c))
            }
        },
        entityEncoder = entityEncoder
      )
    }

    override def mapD[C2](f: C => Directive[F, C2]): Completable[C2, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c))
            }
        },
        entityEncoder = entityEncoder
      )
    }

    override def mapF[C2](f: C => F[C2]): Completable[C2, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c).successF)
            }
        },
        entityEncoder = entityEncoder
      )
    }
  }

  case class MyComplete(
      override val key: List[SimpleLinx],
      pathMatcher: PathMatcher[Any],
      override val consumes: Set[MediaRange],
      override val methodMap: Map[Method, MediaRangeMap[Any => ResponseDirective[F]]]
  ) extends Complete {
    override def merge(other: AbstractUnsecurity[F, U]#Complete): AbstractUnsecurity[F, U]#Complete = {
      this.copy(
        consumes = this.consumes ++ other.consumes,
        methodMap = {
          val v: Map[Method, List[(Method, MediaRangeMap[Any => ResponseDirective[F]])]] =
            (this.methodMap.toList ++ other.methodMap.toList)
              .groupBy(_._1)

          val v2: Map[Method, List[MediaRangeMap[Any => ResponseDirective[F]]]] =
            v.map {
              case (method, l: List[(Method, MediaRangeMap[Any => ResponseDirective[F]])]) =>
                method ->
                  l.map((t: (Method, MediaRangeMap[Any => ResponseDirective[F]])) => t._2)
            }

          val v3: Map[Method, MediaRangeMap[Any => ResponseDirective[F]]] = v2.map {
            case (method, mrms) =>
              method ->
                mrms.reduce((a, b) => a.merge(b))
          }

          v3
        }
      )
    }
    override def compile: PathMatcher[Response[F]] = {
      def allow(methods: Set[Method]): Allow = Allow(methods)

      val f: PathMatcher[Response[F]] = pathMatcher.andThen { pathParamsDirective =>
        for {
          req           <- Directive.request
          pathParams    <- pathParamsDirective
          mediaRangeMap <- matchMethod(methodMap)
          a2rdf         <- matchContentType(mediaRangeMap)
          res           <- a2rdf(pathParams)
        } yield {
          res
        }
      }
      f
    }
  }

}
