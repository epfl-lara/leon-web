/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import java.io.File
import play.Play
import leon.web.models.{User, RepositoryInfos}
import leon.web.shared.{Project}
import leon.web.shared.messages.git.GitOperation

object RepositoryService {

  lazy val root = {
    Play.application.configuration.getString("repositories.path")
  }

  def repositoryFor(user: User, owner: String, name: String, token: Option[String] = None): RepositoryInfos = {
    val path = new File(s"$root/${user.fullId}/$owner/$name")
    new RepositoryInfos(path, user, token)
  }

}

