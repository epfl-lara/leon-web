package leon.web
package data

import scala.scalajs.js
import js.Dynamic.global
import js.annotation.ScalaJSDefined

import leon.web.shared.Provider

case class User(
  id: String,
  main: Identity,
  identities: Map[String, Identity]
)

object User {

  private
  lazy val _user: Option[User.Raw] = {
    val user = global._leon_user.asInstanceOf[User.Raw]
    Option(user)
  }

  lazy val isDefined: Boolean =
    current.isDefined

  lazy val current: Option[User] =
    _user map apply

  def apply(u: User.Raw): User = {
    val ids = u.identities.toMap.mapValues(Identity(_))
    User(u.id, ids(u.main), ids)
  }

  @ScalaJSDefined
  trait Raw extends js.Object {
    val id:         String
    val main:       String
    val identities: js.Dictionary[Identity.Raw]
  }

}

