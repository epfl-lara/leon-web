package leon.web
package client
package data

import scalajs.js
import js.annotation._
import js.Dynamic.{global => g}

import leon.web.shared._

object UserManager {

  @ScalaJSDefined
  trait LeonIdentity extends js.Object {
    val serviceUserId: String
    val provider: String
    val firstName: String
    val lastName: String
    val fullName: String
    val email: String
    val avatarUrl: String
  }

  @ScalaJSDefined
  trait LeonUser extends js.Object {
    val id: String
    val main: LeonIdentity
    val identities: js.Dictionary[LeonIdentity]
  }

  def parseIdentities(ids: js.Dictionary[LeonIdentity]): Set[Identity] = {
    (for((k, i) <- ids) yield parseIdentity(i)).toSet
  }

  def parseIdentity(id: LeonIdentity): Identity = {
    Identity(
      serviceUserId = ServiceUserId(id.serviceUserId),
      provider      = Provider(id.provider),
      firstName     = Option(id.firstName),
      lastName      = Option(id.lastName),
      fullName      = Option(id.fullName),
      email         = Option(id.email).map(Email),
      avatarUrl     = Option(id.avatarUrl)
    )
  }

  private
  lazy val _initial: Option[User] =
    if (g._leon_user == null) None else {
      val userJs = g._leon_user.asInstanceOf[LeonUser]

      val user = User(
        userId = UserId(userJs.id),
        main   = Option(userJs.main).map(parseIdentity),
        identities = Option(userJs.identities).map(parseIdentities).getOrElse(Set())
      )

      Option(user)
    }

  lazy val initial: Option[User] = _initial
}

