/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import leon.web.utils.Hash
import leon.web.shared.Provider

import securesocial.core._

case class User(
  userId: User.UserId,
  identities: Set[Identity],
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[User.Email],
  avatarUrl: Option[String],
  authMethod: AuthenticationMethod,
  oAuth2Info: Option[OAuth2Info] = None) {

  def identity(provider: Provider): Option[Identity] =
    identities.find(_.provider == provider)

  def nameOrEmail: Option[String] = {
    lazy val firstLast =
      firstName
        .zip(lastName)
        .headOption
        .map { case (f, l) => s"$f $l" }

    fullName orElse firstLast orElse email.map(_.value)
  }
}

object User {

  case class UserId(value: String) extends AnyVal
  case class Email(value: String)  extends AnyVal

  def toProfile(u: User, provider: Provider): BasicProfile =
    BasicProfile(
      u.identity(provider).map(_.fullId).getOrElse("???"),
      u.userId.value,
      u.firstName, u.lastName, u.fullName,
      u.email.map(_.value), u.avatarUrl,
      u.authMethod, None, u.oAuth2Info, None
    )

  def fromProfile(p: BasicProfile): User = {
    val userId = Hash.hash(p.providerId + "-" + p.userId, 0)

    User(
      UserId(userId),
      Set(),
      p.firstName, p.lastName, p.fullName,
      p.email.map(Email), p.avatarUrl,
      p.authMethod, p.oAuth2Info
    )
  }

}

