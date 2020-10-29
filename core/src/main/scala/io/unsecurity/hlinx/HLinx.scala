package io.unsecurity.hlinx

import java.net.URLDecoder

import shapeless.{::, HList, HNil}

import scala.util.Try

object HLinx {
  def param[A: ParamConverter](name: String): Param[A] = Param(name, ParamConverter[A])

  implicit class StringPimp[S <: Singleton with String](s: S) {
    def as[A: ParamConverter]: Param[A] = Param(s, ParamConverter[A])
  }

  sealed trait CaptureFailure {
    def errorMessage: String = this match {
      case MissingPathParam(name)              => s"Missing required path parameter [$name]"
      case MissingStaticPath(name)             => s"Missing required static path segment [$name]"
      case MissingQueryParam(name)             => s"Missing required query parameter [$name]"
      case PathParamConvertFailure(name, msg)  => s"Could not convert path parameter $name"
      case QueryParamConvertFailure(name, msg) => s"Could not convert query parameter $name"
      case EmptyPath                           => "Could not match on an empty path"
    }
  }
  sealed trait ContinueMatching                                    extends CaptureFailure
  sealed trait StopMatching                                        extends CaptureFailure
  case class MissingPathParam(name: String)                        extends StopMatching
  case class MissingStaticPath(name: String)                       extends StopMatching
  case object EmptyPath                                            extends StopMatching
  case class PathParamConvertFailure(name: String, error: String)  extends ContinueMatching
  case class MissingQueryParam(name: String)                       extends ContinueMatching
  case class QueryParamConvertFailure(name: String, error: String) extends ContinueMatching

  sealed trait HLinx[T <: HList] {
    self =>
    final def capture[TUP](s: String)(implicit revTup: ReversedTupled.Aux[T, TUP]): Either[CaptureFailure, TUP] = {
      val (paths, queryParams) = splitPathAndQueryParams(s)
      extract(paths.reverse, queryParams).map(e => revTup(e))
    }
    def extract(path: List[String], queryParams: Map[String, List[String]]): Either[CaptureFailure, T]
    def toSimple: List[SimpleLinx]

    def renderString = {
      val (path, params) = self.toSimple.reverse.partition {
        case _: SimpleParams => false
        case _ => true
      }
      s"/${path.mkString("/")}?${params.mkString("&")}"
    }
  }


  trait HPath[T <: HList] extends HLinx[T] {
    self =>
    def /(element: String): Static[T] = {
      splitPath(element).tail
        .foldLeft(Static(this, splitPath(element).head)) { (acc, e) =>
          Static[T](acc, e)
        }
    }
    def /[H](h: Param[H]): Variable[H, T]     = Variable(this, h.converter, h.name)
    def :?[A](h: Param[A]): QueryParam[A, T]  = QueryParam(this, ParamsConverter.singleParamConverter(h.converter), h.name)
    def :?[A](h: Params[A]): QueryParam[A, T] = QueryParam[A, T](this, h.converter, h.name)

    def overlaps[O <: HList](other: HPath[O]): Boolean

  }

  case object Root extends HPath[HNil] {
    override def extract(path: List[String], queryParams: Map[String, List[String]]): Either[CaptureFailure, HNil] =
      if (path.isEmpty) Right(HNil) else Left(EmptyPath)

    override def overlaps[O <: HList](other: HPath[O]): Boolean =
      other match {
        case Root => true
        case _    => false
      }
    override def toSimple: List[SimpleLinx] = Nil
  }

  case class Static[A <: HList](parent: HPath[A], element: String) extends HPath[A] {
    import shapeless.HList.ListCompat.::
    override def extract(s: List[String], queryParams: Map[String, List[String]]): Either[CaptureFailure, A] = s match {
      case `element` :: rest => parent.extract(rest, queryParams)
      case _                 => Left(MissingStaticPath(element))
    }
    override def overlaps[O <: HList](other: HPath[O]): Boolean =
      other match {
        case Root                              => false
        case Static(otherParent, otherElement) => element == otherElement && parent.overlaps(otherParent)
        case Variable(otherParent, _, _)       => parent.overlaps(otherParent)
      }

    def toSimple: List[SimpleLinx] =
      SimpleStatic(element) :: parent.toSimple
  }
  case class Variable[H, T <: HList](parent: HPath[T], P: ParamConverter[H], element: String) extends HPath[H :: T] {
    import shapeless.HList.ListCompat.::
    override def extract(path: List[String], queryParams: Map[String, List[String]]): Either[CaptureFailure, H :: T] =
      path match {
        case h :: rest =>
          parent
            .extract(rest, queryParams)
            .flatMap { (t: T) =>
              P.convert(URLDecoder.decode(h, "UTF-8")).left.map(msg => PathParamConvertFailure(element, msg)).map(_ :: t)
            }
        case _ => Left(MissingPathParam(element))
      }

    def overlaps[A <: HList](hlinx: HPath[A]): Boolean = {
      hlinx match {
        case Root                        => false
        case Static(otherParent, _)      => parent.overlaps(otherParent)
        case Variable(otherParent, _, _) => parent.overlaps(otherParent)
      }
    }
    override def toSimple: List[SimpleLinx] =
      SimpleVariable(element) :: parent.toSimple
  }

  case class QueryParam[H, T <: HList](parent: HLinx[T], P: ParamsConverter[H], field: String) extends HLinx[H :: T] {
    def &[A](h: Param[A]): QueryParam[A, H :: T]  = QueryParam(this, ParamsConverter.singleParamConverter(h.converter), h.name)
    def &[A](h: Params[A]): QueryParam[A, H :: T] = QueryParam[A, H :: T](this, h.converter, h.name)

    override def extract(path: List[String], queryParams: Map[String, List[String]]): Either[CaptureFailure, H :: T] = {
      def urlDecode(undecoded: String): Either[QueryParamConvertFailure, String] =
        Try {
          URLDecoder.decode(undecoded, "UTF-8")
        }.toEither.left.map(t => QueryParamConvertFailure(field, t.getMessage))

      parent.extract(path, queryParams).flatMap { t =>
        import cats.implicits._
        val values: List[Either[QueryParamConvertFailure, String]] = queryParams.getOrElse(field, Nil).map(urlDecode)
        val bah: Either[QueryParamConvertFailure, List[String]]    = values.sequence
        val convertedValue: Either[QueryParamConvertFailure, H]    = bah.flatMap(vs => P.convert(vs).left.map(msg => QueryParamConvertFailure(field, msg)))
        (values, convertedValue) match {
          case (Nil, Left(_))    => Left(MissingQueryParam(field))
          case (_, Left(cpcf))   => Left(cpcf)
          case (_, Right(value)) => Right(value :: t)
        }
      }
    }

    def toSimple: List[SimpleLinx] =
      SimpleParams(field) :: parent.toSimple

  }
  private def splitPathAndQueryParams(s: String): (List[String], Map[String, List[String]]) = {
    val arr = s.split("\\?", 2)
    (splitPath(arr.head), splitQueryParams(arr.tail.headOption.getOrElse("")))
  }
  private def splitPath(path: String): List[String] = path.split("/").toList.filter(_.nonEmpty)
  private def splitQueryParams(s: String): Map[String, List[String]] = {
    s.split("&")
      .toList
      .map(qs => {
        val qa = qs.split("=", 2)
        qa.head -> qa.tail.headOption
      })
      .groupMapReduce(_._1)(_._2.toList)(_ ++ _)
  }
}
