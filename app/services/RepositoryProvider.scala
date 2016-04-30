/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.User
import leon.web.shared.Provider

trait RepositoryProvider {

  type Repository

  def provider: Provider

  def isAvailable: Boolean

  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[Repository]]

}

class GitHubRepositoryProvider(user: User) extends RepositoryProvider {

  import leon.web.models.github

  type Repository = github.Repository

  private def token =
    user.github.flatMap(_.oAuth2Info).map(_.accessToken)

  override def provider =
    Provider.GitHub

  override def isAvailable =
    token.isDefined

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    GitHubService(token.get).listUserRepositories()
  }

}

class TequilaRepositoryProvider(user: User) extends RepositoryProvider {

  type Repository = Nothing

  override def provider =
    Provider.Tequila

  override def isAvailable =
    user.tequila.isDefined

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    Future.successful(Seq())
  }

}

