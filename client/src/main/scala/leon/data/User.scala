package leon.web
package client
package data

import scala.scalajs.js
import js.Dynamic.global
import js.annotation.ScalaJSDefined

import leon.web.shared.Provider

case class User(
  id: String,
  main: Identity,
  identities: Map[Provider, Identity]
) {

  lazy val github  = identities get Provider.GitHub
  lazy val tequila = identities get Provider.Tequila

}

object User {

  private
  lazy val _initial: Option[User.Raw] = {
    val user = global._leon_user.asInstanceOf[User.Raw]
    Option(user)
  }

  lazy val initial: Option[User] =
    _initial map User.apply

  def apply(u: User.Raw): User = {
    val ids = u.identities.toMap.map { case (p, i) =>
      (Provider(p), Identity(i))
    }

    User(u.id, ids(Provider(u.main)), ids)
  }

  @ScalaJSDefined
  trait Raw extends js.Object {
    val id:         String
    val main:       String
    val identities: js.Dictionary[Identity.Raw]
  }

}

