/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package models

import leon.web.shared.Provider

import securesocial.core._

case class User(
  userId: User.UserId,
  main: Identity,
  identities: Set[Identity]
) {

  def identity(provider: Provider): Option[Identity] =
    identities.find(_.provider == provider)

  lazy val github  = identity(Provider.GitHub)
  lazy val tequila = identity(Provider.Tequila)

  def unlink(id: Identity): User = {
    require(id =!= main)

    User(
      userId,
      main,
      identities.filter(_ =!= id)
    )
  }

}

object User {

  type Email = Identity.Email

  case class UserId(value: String) extends AnyVal

  def apply(userId: UserId, ids: Set[Identity], mainProvider: Provider): User =
    new User(userId, ids.find(_.provider === mainProvider).get, ids)

  def fromProfile(p: BasicProfile): User = {
    val id = Identity.fromProfile(p)
    User(id.userId, id, Set(id))
  }

  import play.api.libs.json.JsValue

  def toJson(user: User): JsValue = {
    import play.api.libs.json._

    Json.obj(
      "id"         -> user.userId.value,
      "main"       -> user.main.provider.id,
      "identities" -> Json.obj(
        "github"  -> user.github.map(Identity.toJson),
        "tequila" -> user.tequila.map(Identity.toJson)
      )
    )
  }

}

