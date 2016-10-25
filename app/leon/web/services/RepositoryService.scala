/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.Play

import java.io.File
import java.nio.file.Paths

import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.{User, GitWorkingCopy}
import leon.web.shared._

// WARNING: Work-in-progress, to be refactored soon.
trait RepositoryService {

  type R <: Repository

  def provider: Provider

  def isAvailable: Boolean

  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[R]]

  def getRepoFromDesc(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[R]

  def getWorkingCopy(desc: RepositoryDesc): GitWorkingCopy

}

object RepositoryService {

  private
  val tequilaRootDir = {
    Play.application.configuration.getString("repositories.tequila.path")
  }

  private
  val githubRootDir = {
    Play.application.configuration.getString("repositories.github.path")
  }

  // FIXME: Return Option
  def forUser(user: User, provider: Provider): RepositoryService = provider match {
    case Provider.GitHub  => new GitHubRepositoryService(user, githubRootDir)
    case Provider.Tequila => new TequilaRepositoryService(user, tequilaRootDir)
  }

  def forUser(user: User): Set[RepositoryService] = {
    Provider.all.map(forUser(user, _)).filter(_.isAvailable)
  }

  def getWorkingCopy(user: User, desc: RepositoryDesc): GitWorkingCopy =
    forUser(user, desc.provider).getWorkingCopy(desc)

}

class GitHubRepositoryService(user: User, rootDir: String) extends RepositoryService {

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

  override def getWorkingCopy(desc: RepositoryDesc): GitWorkingCopy = {
    require(isAvailable)

    val RepositoryDesc.GitHub(owner, name) = desc

    val path = Paths.get(rootDir, user.userId.value, owner, name)
    new GitWorkingCopy(path.toFile, user, token)
  }

}

class LocalRepositoryService(user: User, rootDir: String) extends RepositoryService {

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
    val wc        = new GitWorkingCopy(dir, null)
    val defBranch = wc.branchName()
    val branches  = wc.branchesNamesAndRef(false).map(r => Branch(r._1, r._2)).toSeq

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
      repoFromDir(Paths.get(rootDir, path).toFile)
    }
  }

  override def getWorkingCopy(desc: RepositoryDesc): GitWorkingCopy = {
    require(isAvailable)

    val RepositoryDesc.Local(path) = desc

    val file = Paths.get(rootDir, path).toFile
    new GitWorkingCopy(file, user)
  }

  import org.eclipse.jgit.lib.Ref

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

class TequilaRepositoryService(user: User, tequilaDir: String) extends RepositoryService {

  type R = TequilaRepository

  val rootDir = user.tequila.map(t => s"$tequilaDir/${t.publicId.serviceUserId.value}")

  val localProvider = rootDir.map(new LocalRepositoryService(user, _))

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

    localProvider.get.getRepoFromDesc(RepositoryDesc.Local(name)).map { repo =>
      TequilaRepository(
        user.tequila.get.publicId.serviceUserId.value,
        repo.name,
        repo.defaultBranch,
        repo.branches
      )
    }
  }

  override def getWorkingCopy(desc: RepositoryDesc): GitWorkingCopy = {
    require(isAvailable)

    val RepositoryDesc.Tequila(sciper, name) = desc

    val file = Paths.get(rootDir.get, name).toFile
    new GitWorkingCopy(file, user)
  }

}

