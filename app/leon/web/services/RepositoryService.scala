/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import scala.concurrent.{Future, ExecutionContext}
import leon.web.models.User
import leon.web.shared.{RepositoryDesc, Repository, RepositoryType, _}

class RepositoryService(user: User, ghService: Option[GitHubService]) {

  import RepositoryDesc._
  import shared._

  def fromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[Repository] = desc match {
    case RepositoryDesc.Local(path) =>
      Future.successful(getLocalRepository(s"local/$path"))

    case RepositoryDesc.Tequila(sciper, name) =>
      Future.successful(getLocalRepository(s"tequila/$sciper/$name"))

    case RepositoryDesc.GitHub(owner, name) => ghService match {
      case Some(service) => service.getRepository(owner, name)
      case None          => Future.failed(new Throwable("Missing GitHub oAuth token."))
    }
  }

  def getLocalRepository(path: String): Repository =
    LocalRepository(
      name          = path,
      path          = path,
      defaultBranch = "master",
      branches      = Seq()
    )

}

object RepositoryService {

  def apply(user: User) = {
    val token     = user.github.flatMap(_.oAuth2Info).map(_.accessToken)
    val ghService = token.map(GitHubService(_))

    new RepositoryService(user, ghService)
  }

}

