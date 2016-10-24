/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import play.api.libs.json._
import leon.web.shared.{User => SharedUser, _}
import securesocial.core._

case class User(
  userId: UserId,
  main: Identity,
  identities: Set[Identity]
) {

  def identity(provider: Provider): Option[Identity] =
    identities.find(_.publicId.provider == provider)

  lazy val github  = identity(Provider.GitHub)
  lazy val tequila = identity(Provider.Tequila)

  def unlink(id: Identity): User = {
    require (identities.size >= 2)

    val newIds  = identities - id
    val newMain = if (main === id) newIds.head else main

    User(userId, newMain, newIds)
  }

  def toShared: SharedUser = {
    SharedUser(userId, Some(main.publicId), identities.map(_.publicId))
  }
}

object User {
  import Identity._

  def apply(userId: UserId, main: Provider, ids: Set[Identity]): User =
    User(userId, ids.find(_.publicId.provider === main).get, ids)

  def fromProfile(p: BasicProfile): User = {
    val id = Identity.fromProfile(p)
    User(id.userId, id, Set(id))
  }

  implicit val userWrites = new Writes[User] {
    def writes(user: User): JsValue= {
      val ids = user.identities.toSeq.map(i => i.publicId.provider.id -> i).toMap

      Json.obj(
        "id"         -> user.userId.value,
        "main"       -> user.main,
        "identities" -> ids
      )
    }
  }

}

