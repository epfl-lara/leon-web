/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package models

import securesocial.core._

case class UserId(value: String)     extends AnyVal
case class ProviderId(value: String) extends AnyVal
case class Email(value: String)      extends AnyVal

case class User(
  providerId: ProviderId,
  userId: UserId,
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[Email],
  avatarUrl: Option[String],
  authMethod: AuthenticationMethod,
  oAuth2Info: Option[OAuth2Info] = None) {

  def fullId: String          = s"${providerId.value}-${userId.value}"
  def toProfile: BasicProfile = User.toProfile(this)
}

object User {

  def toProfile(u: User): BasicProfile = BasicProfile(
    u.providerId.value, u.userId.value,
    u.firstName, u.lastName, u.fullName,
    u.email.map(_.value), u.avatarUrl,
    u.authMethod, None, u.oAuth2Info, None
  )

  def fromProfile(p: BasicProfile): User = User (
    ProviderId(p.providerId), UserId(p.userId),
    p.firstName, p.lastName, p.fullName,
    p.email.map(Email), p.avatarUrl,
    p.authMethod, p.oAuth2Info
  )

}

