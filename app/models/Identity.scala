/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import securesocial.core._

import leon.web.utils.Hash
import leon.web.shared.Provider

case class Identity(
  userId: User.UserId,
  provider: Provider,
  serviceUserId: Identity.ServiceUserId,
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[Identity.Email],
  avatarUrl: Option[String],
  authMethod: AuthenticationMethod,
  oAuth2Info: Option[OAuth2Info]
) {

  def fullId = s"${provider.id}-${serviceUserId.value}"

  def nameOrEmail: Option[String] = {
    lazy val firstLast =
      firstName
        .zip(lastName)
        .headOption
        .map { case (f, l) => s"$f $l" }

    fullName orElse firstLast orElse email.map(_.value)
  }
}

object Identity {
  case class ServiceUserId(value: String) extends AnyVal
  case class Email(value: String)  extends AnyVal

  def toProfile(i: Identity): BasicProfile = {
    BasicProfile(
      i.fullId, i.userId.value,
      i.firstName, i.lastName, i.fullName,
      i.email.map(_.value), i.avatarUrl,
      i.authMethod, None, i.oAuth2Info, None
    )
  }

  def fromProfile(p: BasicProfile, forUser: Option[User.UserId] = None): Identity = {
    val userId = forUser.getOrElse {
      val hash = Hash.hash(p.providerId + "-" + p.userId, 0)
      User.UserId(hash)
    }

    Identity(
      userId, Provider(p.providerId), ServiceUserId(p.userId),
      p.firstName, p.lastName, p.fullName,
      p.email.map(Email), p.avatarUrl,
      p.authMethod, p.oAuth2Info
    )
  }
}

