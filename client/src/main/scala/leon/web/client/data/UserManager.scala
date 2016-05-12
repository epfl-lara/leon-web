package leon.web
package client
package data

import scalajs.js.Dynamic.global
import leon.web.shared._

object UserManager {
  private
  lazy val _initial: Option[User] = {
    val user = global._leon_user.asInstanceOf[User]
    Option(user)
  }

  lazy val initial: Option[User] = _initial
}

