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

  def getWorkingCopy(user: User, desc: RepositoryDesc, token: Option[String] = None): GitWorkingCopy = desc match {
    case RepositoryDesc.Local(name) =>
      val path = new File(s"$root/${desc.provider.id}/${user.userId.value}/$name")
      new GitWorkingCopy(path, user)

    case RepositoryDesc.Tequila(sciper, name) =>
      val path = new File(s"$root/${desc.provider.id}/$sciper/$name")
      new GitWorkingCopy(path, user)

    case RepositoryDesc.GitHub(owner, name) =>
      val path = new File(s"$root/${desc.provider.id}/${user.userId.value}/$owner/$name")
      new GitWorkingCopy(path, user, token)
  }

}

