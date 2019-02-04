package io.unsecurity.auth

import java.time.OffsetDateTime

trait AuthenticatedUser {
  def userId: UserId
}

trait AuthenticatedUserWithMeta extends AuthenticatedUser{
  def email: String
  def picture: String
  def name: String
  def updatedAt: OffsetDateTime
}

