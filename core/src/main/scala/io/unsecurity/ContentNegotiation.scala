package io.unsecurity

import cats.data.NonEmptyList
import org.http4s.{MediaRange, MediaType, QValue, Request}
import org.http4s.headers.{Accept, MediaRangeAndQValue}

object ContentNegotiation {

  def accept[F[_], A](
      accepts: List[MediaRangeAndQValue],
      nel: NonEmptyList[(MediaType, A)]
  ): Option[A] = {
    val scoreResult = score(accepts, nel).maxByOption(_._1)
    scoreResult.map(_._2)
  }

  def score[A](accepts: List[MediaRangeAndQValue], provided: NonEmptyList[(MediaType, A)]): List[(QValue, A)] =
    provided.toList.flatMap {
      case (mediaType, response) =>
        def exact =
          accepts.collectFirst {
            case MediaRangeAndQValue(`mediaType`, qValue) =>
              (qValue, response)
          }
        def tpe =
          accepts.collectFirst {
            case MediaRangeAndQValue(mediaRange, qValue) if mediaRange.extensions.isEmpty && mediaType.satisfiedBy(mediaRange) =>
              (qValue, response)
          }
        def range =
          accepts.collectFirst {
            case MediaRangeAndQValue(mediaRange, qValue)
              if mediaRange.extensions.isEmpty && mediaRange != MediaRange.`*/*` && mediaRange.satisfiedBy(mediaType) =>
              (qValue, response)
          }
        def wildcard =
          accepts.collectFirst {
            case MediaRangeAndQValue(mediaRange, qValue) if mediaRange == MediaRange.`*/*` =>
              (qValue, response)
          }

        exact orElse tpe orElse range orElse wildcard
    }
}
