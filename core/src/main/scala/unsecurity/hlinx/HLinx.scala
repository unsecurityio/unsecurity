package unsecurity
package hlinx

import java.net.URLDecoder

import shapeless.{::, HList, HNil}

object HLinx {
  def param[A: ParamConverter](name: String): Param[A] = Param(name, ParamConverter[A])

  @deprecated(message = "Symbol is deprecated in scala 2.13 use String instead", since = "2.0")
  implicit class SymbolPimp(s: Symbol) {
    def as[A: ParamConverter]: Param[A] = Param(s.name, ParamConverter[A])
  }

  implicit class StringPimp(s: String) {
    def as[A: ParamConverter]: Param[A] = Param(s, ParamConverter[A])
  }

  sealed trait HLinx[T <: HList] {
    def /(element: String): Static[T] = {
      splitPath(element).tail
        .foldLeft(Static(this, splitPath(element).head)) { (acc, e) =>
          Static(acc, e)
        }
    }
    def /[H](h: Param[H]): Variable[H, T] = Variable(this, h.converter, h.name)
    def capture[TUP](s: String)(implicit revTup: ReversedTupled.Aux[T, TUP]): Option[Either[String, TUP]] = {
      extract(splitPath(s).reverse)
        .map(e => e.map(t => revTup(t)))
    }

    def extract(s: List[String]): Option[Either[String, T]]
    def overlaps[O <: HList](other: HLinx[O]): Boolean
    def toSimple: List[SimpleLinx]
  }

  case object Root extends HLinx[HNil] {
    override def extract(s: List[String]): Option[Either[String, HNil]] =
      if (s.isEmpty) Some(Right(HNil)) else None

    override def overlaps[O <: HList](other: HLinx[O]): Boolean =
      other match {
        case Root => true
        case _    => false
      }
    override def toSimple: List[SimpleLinx] = Nil
  }

  case class Static[A <: HList](parent: HLinx[A], element: String) extends HLinx[A] {
    import shapeless.HList.ListCompat.::
    override def extract(s: List[String]): Option[Either[String, A]] = s match {
      case `element` :: rest => parent.extract(rest)
      case _                 => None
    }
    override def overlaps[O <: HList](other: HLinx[O]): Boolean =
      other match {
        case Root                              => false
        case Static(otherParent, otherElement) => element == otherElement && parent.overlaps(otherParent)
        case Variable(otherParent, _, _)       => parent.overlaps(otherParent)
      }

    def toSimple: List[SimpleLinx] =
      SimpleStatic(element) :: parent.toSimple
  }
  case class Variable[H, T <: HList](parent: HLinx[T], P: ParamConverter[H], element: String) extends HLinx[H :: T] {
    import shapeless.HList.ListCompat.::
    override def extract(s: List[String]): Option[Either[String, H :: T]] = s match {
      case h :: rest =>
        parent
          .extract(rest)
          .map(t =>
            for {
              hlist          <- t
              convertedParam <- P.convert(URLDecoder.decode(h, "UTF-8"))
            } yield {
              convertedParam :: hlist
          })
      case _ => None
    }

    def overlaps[A <: HList](hlinx: HLinx[A]): Boolean = {
      hlinx match {
        case Root                        => false
        case Static(otherParent, _)      => parent.overlaps(otherParent)
        case Variable(otherParent, _, _) => parent.overlaps(otherParent)
      }
    }
    override def toSimple: List[SimpleLinx] =
      SimpleVariable(element) :: parent.toSimple
  }

  private def splitPath(path: String): List[String] = path.split("/").toList.filter(_.nonEmpty)
}
