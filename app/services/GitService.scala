/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import java.io.File
import play.Play

import leon.web.models.{User, GitWorkingCopy}

object GitService {

  lazy val root = {
    Play.application.configuration.getString("repositories.path")
  }

  def getWorkingCopy(user: User, owner: String, name: String, token: Option[String] = None): GitWorkingCopy = {
    val path = new File(s"$root/${user.userId.value}/$owner/$name")
    new GitWorkingCopy(path, user, token)
  }

}

