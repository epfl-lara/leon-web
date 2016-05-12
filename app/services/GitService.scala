/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import java.io.File
import play.Play

import leon.web.models.{User, GitWorkingCopy}
import leon.web.shared.{Repository, RepositoryDesc, _}

object GitService {

  import RepositoryDesc._

  lazy val root = {
    Play.application.configuration.getString("repositories.path")
  }

  def getWorkingCopy(user: User, repoDesc: RepositoryDesc, token: Option[String] = None): GitWorkingCopy = repoDesc match {
    case LocalRepositoryDesc(name) =>
      val path = new File(s"$root/${user.userId.value}/local/$name")
      new GitWorkingCopy(path, user)

    case GitHubRepositoryDesc(owner, name) =>
      val path = new File(s"$root/${user.userId.value}/$owner/$name")
      new GitWorkingCopy(path, user, token)
  }

}

