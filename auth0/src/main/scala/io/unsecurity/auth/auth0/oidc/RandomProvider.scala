package io.unsecurity.auth.auth0.oidc

import java.security.SecureRandom

import cats.effect.Sync
import scodec.bits.BitVector

trait RandomProvider[F[_]] {
  def nextBytes(size: Int): F[BitVector]
}

object RandomProvider {
  implicit def SecureR[F[_]: Sync]: RandomProvider[F] = {
    val secrand = new SecureRandom() //TODO: Expensive to create, should be moved to instance field (side effect as well)
    size: Int =>
      Sync[F].delay {
        val byteArray = Array.fill[Byte](size)(0)
        secrand.nextBytes(byteArray)
        BitVector(byteArray)
      }
  }
}
