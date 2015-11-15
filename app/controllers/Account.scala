/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package controllers

import scala.concurrent.Future

import play.api._
import play.api.mvc._

import securesocial.core._

import leon.web.models.User

class Account(override implicit val env: RuntimeEnvironment[User]) extends SecureSocial[User] {

  def login = Action {
    Redirect(securesocial.controllers.routes.ProviderController.authenticate("github", None))
  }

}

