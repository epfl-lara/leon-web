/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import scala.concurrent.{Future, ExecutionContext}
import leon.web.models.User
import leon.web.shared.{RepositoryDesc, Repository, RepositoryType, _}

class RepositoryService(user: User, ghService: Option[GitHubService]) {

  import RepositoryDesc._
  import shared._

  def fromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[Repository] = {
    val provider = RepositoryProvider.forUser(user, desc.provider)
    provider.getRepoFromDesc(desc)
  }

}

object RepositoryService {

  def apply(user: User) = {
    val token     = user.github.flatMap(_.oAuth2Info).map(_.accessToken)
    val ghService = token.map(GitHubService(_))

    new RepositoryService(user, ghService)
  }

}

