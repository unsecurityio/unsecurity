package io.unsecurity.hlinx

import shapeless.{::, HList, HNil}

object HLinx {
  def param[A](name: String)(implicit ppc: PathParamConverter[A])   = Param(name, ppc)
  def qParam[A](name: String)(implicit qpc: QueryParamConverter[A]) = QueryParam[A](name)

  implicit class QpOps[A](qp: QueryParam[A]) {
    def &[B](other: QueryParam[B]): QueryParam[B] :: QueryParam[A] :: HNil =
      other :: qp :: HNil
  }

  implicit class HListOfQpOps[A <: HList](qp: A) {
    def &[B](other: QueryParam[B]) =
      other :: qp
  }

  // TODO move overlaps functionality into this
  // TODO good error messages when overlapping
  // TODO wrapper class instead of List
  sealed trait SimpleLinx extends Ordered[SimpleLinx] {
    override def compare(that: SimpleLinx): Int =
      (this, that) match {
        case (a: SimpleStatic, b: SimpleStatic)       => a.segment.compare(b.segment)
        case (a: SimpleStatic, b: SimplePathParam)    => -1
        case (a: SimplePathParam, b: SimpleStatic)    => 1
        case (a: SimplePathParam, b: SimplePathParam) => a.name.compare(b.name)
      }
  }
  case class SimpleStatic(segment: String) extends SimpleLinx {
    override def toString: String = segment
  }
  case class SimplePathParam(name: String) extends SimpleLinx {
    override def toString: String = s"{$name}"
  }

  def splitPath(path: String): List[String] = path.split("/").toList.filter(_.nonEmpty)

  sealed trait HLinx[T <: HList] {
    def /(element: String): Static[T] = {
      splitPath(element).tail
        .foldLeft(Static(this, splitPath(element).head)) { (acc, e) =>
          Static(acc, e)
        }
    }
    def /[H](h: Param[H]) = Variable(this, h.converter, h.name)
    def capture[R <: HList, TUP](s: String)(
        implicit revTup: ReversedTupled.Aux[T, TUP]): Option[Either[String, TUP]] = {
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
  case class Variable[H, T <: HList](parent: HLinx[T], P: PathParamConverter[H], element: String)
      extends HLinx[H :: T] {
    import shapeless.HList.ListCompat.::
    override def extract(s: List[String]): Option[Either[String, H :: T]] = s match {
      case h :: rest =>
        parent
          .extract(rest)
          .map(t =>
            for {
              hlist          <- t
              convertedParam <- P.convert(h)
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
      SimplePathParam(element) :: parent.toSimple
  }
}
