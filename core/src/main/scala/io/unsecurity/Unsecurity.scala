package io
package unsecurity

import cats.effect.{Async, Concurrent, Sync}
import io.unsecurity.hlinx.{Reversed, SimpleLinx, TransformParams, UnwrapTuple1}
import no.scalabin.http4s.directives.Directive
import org.http4s.{DecodeFailure, MediaRange, Method, Response}


abstract class Unsecurity[F[_]: Async, RU, U] extends AbstractUnsecurity[F, U] {

  def sc: SecurityContext[F, RU, U]

  case class MySecured[C, W](
      key: List[SimpleLinx],
      queryParams: List[String],
      pathMatcher: PathMatcher[Any],
      consumes: Set[MediaRange],
      methodMap: Map[Method, Any => Directive[F, C]],
      produces: Produces[W]
  ) extends Secured[C, W] {
    override def authorization(
        predicate: C => Boolean,
        ifUnauthorized: => HttpProblem = HttpProblem.forbidden("Forbidden")): Completable[C, W] = {
      MyCompletable(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method ->
              a2dc.andThen(
                dc =>
                  Directive.commit(
                    dc.filter(
                      c => predicate(c).orF(ifUnauthorized.toResponseF)
                    ))
              )
        },
        producer = produces
      )
    }
    override def mapD[C2](f: C => Directive[F, C2]): Secured[C2, W] = {
      MySecured(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c))
            }
        },
        produces = produces,
      )
    }
    override def map[C2](f: C => C2): Secured[C2, W] = {
      MySecured(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.map(c => f(c))
            }
        },
        produces = produces,
      )
    }
    override def mapF[C2](f: C => F[C2]): Secured[C2, W] = {
      MySecured(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c).toDirective)
            }
        },
        produces = produces,
      )
    }
    def noAuthorization: Completable[C, W] =
      MyCompletable(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap,
        producer = produces
      )
  }

  override def secure[P <: Tuple, R, W](endpoint: Endpoint[P, R, W]): Secured[TransformParams[Reversed[P], (R, U)], W] = {
    MySecured[TransformParams[Reversed[P], (R, U)], W](
      key = endpoint.path.toSimple.reverse,
      queryParams = endpoint.path.params,
      pathMatcher = createPathMatcher(endpoint.path).asInstanceOf[PathMatcher[Any]],
      consumes = endpoint.consumes.consumes,
      methodMap = Map(
        endpoint.method -> { (tup: Reversed[P]) =>
          val checkXsrfOrNothing: Directive[F, String] =
            if endpoint.method == Method.PUT ||
                endpoint.method == Method.POST ||
                endpoint.method == Method.DELETE then {
              sc.xsrfCheck
            } else {
              Directive.success("xsrf not checked")
            }

          for
            _       <- checkXsrfOrNothing
            rawUser <- sc.authenticate
            user <- Directive.commit(
                     Directive.getOrElseF(
                       sc.transformUser(rawUser),
                       HttpProblem.forbidden("Forbidden").toResponseF[F]
                     )
                   )
            r <- request.bodyAs[R] { (error: DecodeFailure) =>
                  HttpProblem.handleError(error).toResponse[F]
                }(endpoint.consumes, Sync[F])
          yield {
            TransformParams(tup, (r, user))
          }
        }.asInstanceOf[Any => Directive[F, TransformParams[Reversed[P], (R, U)]]]
      ),
      produces = endpoint.produces
    )
  }

  override def unsecure[P <: Tuple, R, W](endpoint: Endpoint[P, R, W]):
  Completable[TransformParams[Reversed[P], Tuple1[R]], W] = {
    MyCompletable[TransformParams[Reversed[P], Tuple1[R]], W](
      key = endpoint.path.toSimple.reverse,
      queryParams = endpoint.path.params,
      pathMatcher = createPathMatcher[P](endpoint.path).asInstanceOf[PathMatcher[Any]],
      consumes = endpoint.consumes.consumes,
      methodMap = Map(
        endpoint.method -> { (tup: Reversed[P]) =>
          for
            r <- request.bodyAs[R] { (error: DecodeFailure) =>
                  HttpProblem.handleError(error).toResponse[F]
                }(endpoint.consumes, Sync[F])
          yield {
            TransformParams(tup, Tuple1(r))
          }
        }.asInstanceOf[Any => Directive[F, TransformParams[Reversed[P], Tuple1[R]]]]
      ),
      producer = endpoint.produces
    )
  }

  case class MyCompletable[C, W](
      key: List[SimpleLinx],
      queryParams: List[String],
      pathMatcher: PathMatcher[Any],
      consumes: Set[MediaRange],
      methodMap: Map[Method, Any => Directive[F, C]],
      producer: Produces[W]
  ) extends Completable[C, W] {
    override def run(f: C => W): Complete = {
      MyComplete(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> MediaRangeMap(List(MediaRangeItem(consumes, producer.contentType, a2dc.andThen { dc =>
              for
                c <- dc
                w <- producer.response(f(c))
              yield {
                w
              }
            })))
        }
      )
    }

    override def map[C2](f: C => C2): Completable[C2, W] = {
      MyCompletable(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.map(c => f(c))
            }
        },
        producer = producer
      )
    }

    override def mapD[C2](f: C => Directive[F, C2]): Completable[C2, W] = {
      MyCompletable(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c))
            }
        },
        producer = producer
      )
    }

    override def mapF[C2](f: C => F[C2]): Completable[C2, W] = {
      MyCompletable(
        key = key,
        queryParams = queryParams,
        pathMatcher = pathMatcher,
        consumes = consumes,
        methodMap = methodMap.map {
          case (method, a2dc) =>
            method -> a2dc.andThen { dc =>
              dc.flatMap(c => f(c).toDirective)
            }
        },
        producer = producer
      )
    }
  }

  case class MyComplete(
      override val key: List[SimpleLinx],
      override val queryParams: List[String],
      pathMatcher: PathMatcher[Any],
      override val consumes: Set[MediaRange],
      override val methodMap: Map[Method, MediaRangeMap[Any => ResponseDirective[F]]]
  ) extends Complete {
    override def merge(other: Complete): Complete = {
      this.copy(
        consumes = this.consumes ++ other.consumes,
        methodMap = (this.methodMap.toList ++ other.methodMap.toList)
          .groupMapReduce[Method, MediaRangeMap[Any => ResponseDirective[F]]](_._1)(_._2)((a, b) => a.merge(b))
      )
    }
    override def compile: PathMatcher[Response[F]] = {

      pathMatcher.andThen { pathParamsDirective =>
        for
          pathParams      <- pathParamsDirective
          mediaRangeMap   <- matchMethod(methodMap)
          mediaRangeItems <- matchRequestContentType(mediaRangeMap)
          a2rdf           <- matchAcceptContentType(mediaRangeItems)
          res             <- a2rdf(pathParams)
        yield {
          res
        }
      }
    }
  }

}
