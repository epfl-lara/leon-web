/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import play.api.libs.json._
import securesocial.core._
import leon.web.utils.Hash
import leon.web.shared.{Identity => PublicIdentity, _}
import play.api.libs.json.Json.toJsFieldJsValueWrapper

case class Identity(
  publicId   : PublicIdentity,
  userId     : UserId,
  authMethod : AuthenticationMethod,
  oAuth2Info : Option[OAuth2Info]
)

object Identity {
  def toProfile(i: Identity): BasicProfile = {
    BasicProfile(
      i.publicId.fullId, i.publicId.serviceUserId.value,
      i.publicId.firstName, i.publicId.lastName, i.publicId.fullName,
      i.publicId.email.map(_.value), i.publicId.avatarUrl,
      i.authMethod, None, i.oAuth2Info, None
    )
  }

  def fromProfile(p: BasicProfile, forUser: Option[UserId] = None): Identity = {
    val userId = forUser.getOrElse {
      val hash = Hash.hash(p.providerId + "-" + p.userId, 0)
      UserId(hash)
    }

    Identity(
      PublicIdentity(
        ServiceUserId(p.userId),
        Provider(p.providerId),
        p.firstName, p.lastName, p.fullName,
        p.email.map(Email), p.avatarUrl
      ),
      userId, p.authMethod, p.oAuth2Info
    )
  }

  implicit val identityWrites = new Writes[Identity] {
    def writes(id: Identity) = Json.obj(
      "serviceUserId" -> id.publicId.serviceUserId.value,
      "provider"      -> id.publicId.provider.id,
      "firstName"     -> id.publicId.firstName,
      "lastName"      -> id.publicId.lastName,
      "fullName"      -> id.publicId.fullName,
      "email"         -> id.publicId.email.map(_.value),
      "avatarUrl"     -> id.publicId.avatarUrl
    )
  }

}

