/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package controllers

import play.api.mvc._

import securesocial.core._

import leon.web.models.User
import leon.web.shared.Provider

class Account(override implicit val env: RuntimeEnvironment[User]) extends SecureSocial[User] {

  def redirect(path: String) = Action { implicit req =>
    req.flash.get("error") match {
      case Some(err) => Redirect(path).flashing(
        "error" -> err
      )
      case None =>
        Redirect(path)
    }
  }

  def login(provider: String) = Action {
    if (Provider.map contains provider)
      Redirect(securesocial.controllers.routes.ProviderController.authenticate(provider, None))
    else
      Redirect(routes.Interface.index("")).flashing(
        "error" -> s"Unknown provider '$provider'."
      )
  }

}

