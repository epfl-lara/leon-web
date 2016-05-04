/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import java.io.File

import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.User
import leon.web.shared._

trait RepositoryProvider {

  type Repository

  def provider: Provider
  def ofType: RepositoryType

  def isAvailable: Boolean

  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[Repository]]

}

object RepositoryProvider {

  def forUser(user: User)(implicit ec: ExecutionContext): Future[Map[Provider, Seq[Repository]]] =
    Future.successful(Map())
}

class GitHubRepositoryProvider(user: User) extends RepositoryProvider {

  type Repository = GitHubRepository

  private def token =
    user.github.flatMap(_.oAuth2Info).map(_.accessToken)

  override def provider: Provider =
    Provider.GitHub

  override def ofType: RepositoryType =
    RepositoryType.GitHub

  override def isAvailable =
    token.isDefined

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    GitHubService(token.get).listUserRepositories()
  }

}

class LocalRepositoryProvider(rootDir: String) extends RepositoryProvider {

  type Repository = LocalRepository

  private lazy val root = new File(rootDir)

  override def provider: Provider =
    Provider.Unknown

  override def ofType: RepositoryType =
    RepositoryType.Local

  override def isAvailable =
    root.exists && root.isDirectory && root.canRead

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    Future.successful(Seq())
  }

}

class TequilaRepositoryProvider(user: User, rootDir: String) extends LocalRepositoryProvider(rootDir) {

  override def provider: Provider =
    Provider.Tequila

  override def isAvailable =
    user.tequila.isDefined && super.isAvailable

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    Future.successful(Seq())
  }

}
