/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.Play

import java.io.File

import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.User
import leon.web.shared._

trait RepositoryProvider {

  type R <: Repository

  def provider: Provider

  def isAvailable: Boolean

  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[R]]

}

object RepositoryProvider {

  private
  val tequilaRootDir = {
    Play.application.configuration.getString("repositories.path") + "/tequila/"
  }

  def forUser(user: User)(implicit ec: ExecutionContext): Seq[RepositoryProvider] = {
    val gh  = new GitHubRepositoryProvider(user)
    val teq = new TequilaRepositoryProvider(user, tequilaRootDir)

    Seq(gh, teq).filter(_.isAvailable)
  }
}

class GitHubRepositoryProvider(user: User) extends RepositoryProvider {

  type R = GitHubRepository

  private def token =
    user.github.flatMap(_.oAuth2Info).map(_.accessToken)

  override def provider: Provider =
    Provider.GitHub

  override def isAvailable =
    token.isDefined

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    GitHubService(token.get).listUserRepositories()
  }

}

class LocalRepositoryProvider(val rootDir: String) extends RepositoryProvider {

  type R = LocalRepository

  private lazy val root = new File(rootDir)

  override def provider: Provider =
    Provider.Local

  override def isAvailable =
    root.exists && root.isDirectory && root.canRead

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    val dirs = listDirsInDir(rootDir)

    // TODO: Only keep dirs with a .git folder
    val repos = dirs.map { dir =>
      LocalRepository(
        name          = dir.getName,
        path          = dir.getPath,
        defaultBranch = "master",
        branches      = Seq()
      )
    }

    Future.successful(repos)
  }

  private
  def listDirsInDir(dir: String): Seq[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isDirectory).toSeq
    } else {
      Nil
    }
  }

}

class TequilaRepositoryProvider(val user: User, val tequilaDir: String) extends RepositoryProvider {

  type R = TequilaRepository

  val rootDir = s"$tequilaDir/${user.tequila.get.i.serviceUserId.value}"

  lazy val localProvider = new LocalRepositoryProvider(rootDir)

  override def provider: Provider =
    Provider.Tequila

  override def isAvailable = {
    user.tequila.isDefined && localProvider.isAvailable
  }

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    localProvider.listRepositories().map(_.map { repo =>
      TequilaRepository(
        user.tequila.get.i.serviceUserId.value,
        repo.name,
        repo.defaultBranch,
        repo.branches
      )
    })
  }

}

