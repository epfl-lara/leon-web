/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import play.api.libs.json._
import securesocial.core._
import leon.web.utils.Hash
import leon.web.shared.{Identity => SharedIdentity, _}
import play.api.libs.json.Json.toJsFieldJsValueWrapper

case class Identity(
  i: SharedIdentity,
  userId: UserId,
  authMethod: AuthenticationMethod,
  oAuth2Info: Option[OAuth2Info]) {
}

object Identity {
  def toProfile(i: Identity): BasicProfile = {
    BasicProfile(
      i.i.fullId, i.i.serviceUserId.value,
      i.i.firstName, i.i.lastName, i.i.fullName,
      i.i.email.map(_.value), i.i.avatarUrl,
      i.authMethod, None, i.oAuth2Info, None
    )
  }

  def fromProfile(p: BasicProfile, forUser: Option[UserId] = None): Identity = {
    val userId = forUser.getOrElse {
      val hash = Hash.hash(p.providerId + "-" + p.userId, 0)
      UserId(hash)
    }

    Identity(SharedIdentity(
      ServiceUserId(p.userId), Provider(p.providerId), 
      p.firstName, p.lastName, p.fullName,
      p.email.map(Email), p.avatarUrl),
      userId, 
      p.authMethod, p.oAuth2Info
    )
  }

  implicit val identityWrites = new Writes[Identity] {
    def writes(id: Identity) = Json.obj(
      "serviceUserId"    -> id.i.serviceUserId.value,
      "provider"  -> id.i.provider.id,
      "firstName" -> id.i.firstName,
      "lastName"  -> id.i.lastName,
      "fullName"  -> id.i.fullName,
      "email"     -> id.i.email.map(_.value),
      "avatarUrl" -> id.i.avatarUrl
    )
  }
}

