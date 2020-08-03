package io.unsecurity.hlinx

import java.net.URLDecoder
import cats.implicits._

import shapeless.{::, HList, HNil}

object HLinx {
  def param[A: ParamConverter](name: String) = Param(name, ParamConverter[A])

  @deprecated(message = "Symbol is deprecated in scala 2.13 use String instead", since = "2.0")
  implicit class SymbolPimp(s: Symbol) {
    def as[A: ParamConverter] = Param(s.name, ParamConverter[A])
  }

  implicit class StringPimp[S <: Singleton with String](s: S) {
    def as[A: ParamConverter] = Param(s, ParamConverter[A])
  }

  sealed trait HLinx[T <: HList] {
    final def capture[TUP](s: String)(implicit revTup: ReversedTupled.Aux[T, TUP]): Option[Either[String, TUP]] = {
      val (paths, queryParams) = splitPathAndQueryParams(s)
      println(s"(paths=$paths, queryParams=$queryParams)")
      extract(paths.reverse, queryParams)
        .map(e => e.map(t => revTup(t)))
    }
    def extract(path: List[String], queryParams: Map[String, List[String]]): Option[Either[String, T]]
    def toSimple: List[SimpleLinx]
  }
  trait HPath[T <: HList] extends HLinx[T] {
    self =>
    def /(element: String): Static[T] = {
      splitPath(element).tail
        .foldLeft(Static(this, splitPath(element).head)) { (acc, e) =>
          Static[T](acc, e)
        }
    }
    def /[H](h: Param[H])   = Variable(this, h.converter, h.name)
    def :?[A](h: Param[A])  = QueryParam(this, ParamsConverter.singleParamConverter(h.converter), h.name)
    def :?[A](h: Params[A]) = QueryParam[A, T](this, h.converter, h.name)

    def overlaps[O <: HList](other: HPath[O]): Boolean

  }

  case object Root extends HPath[HNil] {
    override def extract(path: List[String], queryParams: Map[String, List[String]]): Option[Either[String, HNil]] =
      if (path.isEmpty) Some(Right(HNil)) else None

    override def overlaps[O <: HList](other: HPath[O]): Boolean =
      other match {
        case Root => true
        case _    => false
      }
    override def toSimple: List[SimpleLinx] = Nil
  }

  case class Static[A <: HList](parent: HPath[A], element: String) extends HPath[A] {
    import shapeless.HList.ListCompat.::
    override def extract(s: List[String], queryParams: Map[String, List[String]]): Option[Either[String, A]] = s match {
      case `element` :: rest => parent.extract(rest, queryParams)
      case _                 => None
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
    override def extract(path: List[String], queryParams: Map[String, List[String]]): Option[Either[String, H :: T]] =
      path match {
        case h :: rest =>
          parent
            .extract(rest, queryParams)
            .map(t =>
              for {
                hlist          <- t
                convertedParam <- P.convert(URLDecoder.decode(h, "UTF-8"))
              } yield {
                convertedParam :: hlist
            })
        case _ => None
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
    def &[A](h: Param[A])  = QueryParam(this, ParamsConverter.singleParamConverter(h.converter), h.name)
    def &[A](h: Params[A]) = QueryParam[A, H :: T](this, h.converter, h.name)

    override def extract(path: List[String], queryParams: Map[String, List[String]]): Option[Either[String, H :: T]] = {
      val value          = queryParams.getOrElse(field, Nil)
      val convertedValue = P.convert(value)
      (value, convertedValue) match {
        case (Nil, Left(_)) => None
        case _ =>
          parent
            .extract(path, queryParams)
            .map(
              _.flatMap(t =>
                convertedValue
                  .map(h => h :: t)))
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
  private def splitQueryParams(s: String): Map[String, List[String]] =
    s.split("&")
      .toList
      .map(qs => {
        val qa = qs.split("=", 2)
        qa.head -> qa.tail.headOption
      })
      .groupMapReduce(_._1)(_._2.toList)(_ ++ _)
}
