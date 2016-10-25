/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.Play

import java.io.File

import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.{User, GitWorkingCopy}
import leon.web.shared._

trait RepositoryProvider {

  type R <: Repository

  def provider: Provider

  def isAvailable: Boolean

  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[R]]

  def getRepoFromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[R]

}

object RepositoryProvider {

  private
  val tequilaRootDir = {
    Play.application.configuration.getString("repositories.tequila.path")
  }

  // FIXME: Return Option
  def forUser(user: User, provider: Provider): RepositoryProvider = provider match {
    case Provider.GitHub  => new GitHubRepositoryProvider(user)
    case Provider.Tequila => new TequilaRepositoryProvider(user, tequilaRootDir)
  }

  def forUser(user: User): Set[RepositoryProvider] = {
    Provider.all.map(forUser(user, _)).filter(_.isAvailable)
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

  override def getRepoFromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext) = {
    require(isAvailable)

    val RepositoryDesc.GitHub(owner, name) = desc
    GitHubService(token.get).getRepository(owner, name)
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

    val dirs  = listDirsInDir(rootDir)
    val repos = dirs.map(repoFromDir)

    Future.successful(repos)
  }

  private def repoFromDir(dir: File): LocalRepository = {
    // TODO: Make GitWorkingCopy take an optional user
    val wc = new GitWorkingCopy(dir, null)
    val defBranch = wc.branchName()
    val branches = wc.branches().toSeq.map(refToBranch)

    LocalRepository(
      name          = dir.getName,
      path          = dir.getPath,
      defaultBranch = defBranch,
      branches      = branches
    )
  }

  override def getRepoFromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext) = {
    require(isAvailable)

    val RepositoryDesc.Local(path) = desc

    Future.successful {
      repoFromDir(new File(path))
    }
  }

  import org.eclipse.jgit.lib.Ref

  def refToBranch(ref: Ref): Branch =
    Branch(ref.getName, ref.getObjectId.toString)

  private
  def hasGitFolder(dir: File): Boolean = {
    dir.listFiles.filter(_.isDirectory).exists(_.getName == ".git")
  }

  private
  def listDirsInDir(dir: String): Seq[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(f => f.isDirectory && hasGitFolder(f)).toSeq
    } else {
      Nil
    }
  }

}

class TequilaRepositoryProvider(val user: User, val tequilaDir: String) extends RepositoryProvider {

  type R = TequilaRepository

  val rootDir = user.tequila.map(t => s"$tequilaDir/${t.publicId.serviceUserId.value}")

  val localProvider = rootDir.map(new LocalRepositoryProvider(_))

  override def provider: Provider =
    Provider.Tequila

  override def isAvailable = {
    user.tequila.isDefined && localProvider.exists(_.isAvailable)
  }

  override def listRepositories()(implicit ec: ExecutionContext) = {
    require(isAvailable)

    localProvider.get.listRepositories().map(_.map { repo =>
      TequilaRepository(
        user.tequila.get.publicId.serviceUserId.value,
        repo.name,
        repo.defaultBranch,
        repo.branches
      )
    })
  }

  override def getRepoFromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext) = {
    require(isAvailable)

    val RepositoryDesc.Tequila(sciper, name) = desc

    localProvider.get.getRepoFromDesc(RepositoryDesc.Local(s"$sciper/$name")).map { repo =>
      TequilaRepository(
        user.tequila.get.publicId.serviceUserId.value,
        repo.name,
        repo.defaultBranch,
        repo.branches
      )
    }
  }

}

