/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import play.api.libs.json._
import securesocial.core._
import leon.web.utils.Hash
import leon.web.shared.{Identity => SharedIdentity, _}
import play.api.libs.json.Json.toJsFieldJsValueWrapper

case class Identity(i: SharedIdentity, 
  authMethod: AuthenticationMethod,
  oAuth2Info: Option[OAuth2Info]) {
}

object Identityr {
  def toProfile(i: Identity): BasicProfile = {
    BasicProfile(
      i.i.fullId, i.i.userId.value,
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
      userId, Provider(p.providerId), ServiceUserId(p.userId),
      p.firstName, p.lastName, p.fullName,
      p.email.map(Email), p.avatarUrl),
      p.authMethod, p.oAuth2Info
    )
  }/*

  implicit val identityWrites = new Writes[Identity] {
    def writes(id: Identity) = Json.obj(
      "userId"    -> id.serviceUserId.value,
      "provider"  -> id.provider.id,
      "firstName" -> id.firstName,
      "lastName"  -> id.lastName,
      "fullName"  -> id.fullName,
      "email"     -> id.email.map(_.value),
      "avatarUrl" -> id.avatarUrl
    )
  }*/

}

