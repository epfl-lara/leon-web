/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import scala.concurrent.{Future, ExecutionContext}

import leon.web.shared.{RepositoryDesc, Repository}

class RepositoryService(user: User, github: GitHubService) {

  import RepositoryDesc._

  def fromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[Repository] = desc match {
    case Local(path) =>
      Future.successful(GitService.getLocalRepository(path))

    case GitHub(owner, name) =>
      github.getRepository(owner, name)
  }

}

