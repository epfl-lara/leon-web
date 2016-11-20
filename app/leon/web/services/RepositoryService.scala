/* Copyright 2009-2016 EPFL, Lausanne */

package leon.web
package services

import play.api.Play
import play.api.Play.current

import java.io.File
import java.nio.file.{Path, Paths}

import scala.util.Try
import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.{User, GitWorkingCopy}
import leon.web.config.RepositoryService.Config
import leon.web.shared._

abstract class RepositoryService(val provider: Provider, val rootDir: File) {

  type R <: Repository

  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[R]]

  def getRepository(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[R]

  def getWorkingCopy(desc: RepositoryDesc): Option[GitWorkingCopy]

}

case object ServiceUnavailable extends Exception("Service unavailable")

object RepositoryService {

  private
  val providers = Seq(Provider.GitHub, Provider.Tequila)

  def apply(user: User, provider: Provider)(implicit config: Config): Option[RepositoryService] = provider match {
    case Provider.GitHub  => GitHubRepositoryService(user, config.githubDir)
    case Provider.Tequila => TequilaRepositoryService(user, config.tequilaDir)
    case _                => None
  }

  def listRepositoriesByProvider(user: User)
                                (implicit config: Config, ec: ExecutionContext): Future[Map[Provider, Seq[Repository]]] = {
    val future = Future.sequence {
      providers
        .map(RepositoryService(user, _))
        .filter(_.isDefined).map(_.get)
        .map { service => 
          service.listRepositories().map { repos =>
            (service.provider -> repos)
          }
        }
    }

    future.map(_.toMap)
  }

  def getRepository(user: User, desc: RepositoryDesc)
                   (implicit config: Config, ec: ExecutionContext): Future[Repository] = {

    RepositoryService(user, desc.provider)
      .map(_.getRepository(desc))
      .getOrElse(Future.failed(ServiceUnavailable))
  }

  def getWorkingCopy(user: User, desc: RepositoryDesc)(implicit config: Config): Option[GitWorkingCopy] = {
    RepositoryService(user, desc.provider).flatMap(_.getWorkingCopy(desc))
  }

}

class GitHubRepositoryService(user: User, rootDir: File, val token: String)
  extends RepositoryService(Provider.GitHub, rootDir) {

  type R = GitHubRepository

  private
  val ghService = GitHubService(token)

  override
  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[GitHubRepository]] = {
    ghService.listUserRepositories()
  }

  override
  def getRepository(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[GitHubRepository] = {
    val RepositoryDesc.GitHub(owner, name) = desc
    ghService.getRepository(owner, name)
  }

  override
  def getWorkingCopy(desc: RepositoryDesc): Option[GitWorkingCopy] = desc match {
    case RepositoryDesc.GitHub(owner, name) =>
      val path = Paths.get(rootDir.getPath, user.userId.value, owner, name)
      Some(new GitWorkingCopy(path.toFile, user, Some(token)))

    case _ => None
  }

}

object GitHubRepositoryService {

  def apply(user: User, rootDir: File): Option[GitHubRepositoryService] =
    for {
      token <- user.github.flatMap(_.oAuth2Info).map(_.accessToken)
      if RootDir.isValid(rootDir)
    }
    yield new GitHubRepositoryService(user, rootDir, token)

}

class LocalRepositoryService(user: User, rootDir: File)
  extends RepositoryService(Provider.Local, rootDir) {

  type R = LocalRepository

  override
  def listRepositories()(implicit ec: ExecutionContext) = {
    val dirs  = listDirsIn(rootDir)
    val repos = dirs.map(repoFromDir)

    Future.successful(repos)
  }

  override
  def getRepository(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[LocalRepository] = {
    val RepositoryDesc.Local(path) = desc

    Future.successful {
      repoFromDir(Paths.get(rootDir.getPath, path).toFile)
    }
  }

  override
  def getWorkingCopy(desc: RepositoryDesc): Option[GitWorkingCopy] = desc match {
    case RepositoryDesc.Local(name) =>
      val path = Paths.get(rootDir.getPath, name)
      Some(new GitWorkingCopy(path.toFile, user))

    case _ => None
  }

  import org.eclipse.jgit.lib.Ref

  private
  def hasGitFolder(dir: File): Boolean = {
    dir.listFiles.filter(_.isDirectory).exists(_.getName == ".git")
  }

  private
  def listDirsIn(dir: File): Seq[File] = {
    if (dir.exists && dir.isDirectory)
      dir.listFiles.filter(f => f.isDirectory && hasGitFolder(f)).toSeq
    else
      Nil
  }

  private
  def repoFromDir(dir: File): LocalRepository = {
    val wc        = new GitWorkingCopy(dir, user)
    val defBranch = wc.branchName()
    val branches  = wc.branchesNamesAndRef(false).map(r => Branch(r._1, r._2)).toSeq
    val origin    = wc.getOrigin()

    LocalRepository(
      name          = dir.getName,
      path          = dir.getPath,
      defaultBranch = defBranch,
      branches      = branches,
      remote        = origin.map(o => Remote(o.name, o.url))
    )
  }

}

object LocalRepositoryService {

  def apply(user: User, rootDir: File): Option[LocalRepositoryService] =
    for {
      dir <- Some(rootDir)
      if RootDir.isValid(dir)
    }
    yield new LocalRepositoryService(user, dir)

}

class TequilaRepositoryService(user: User, rootDir: File, localService: LocalRepositoryService)
  extends RepositoryService(Provider.Tequila, rootDir) {

  type R = TequilaRepository

  override
  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[TequilaRepository]] = {
    localService.listRepositories() map { repos =>
      repos map { repo =>
        TequilaRepository(
          user.tequila.get.publicId.serviceUserId.value,
          repo.name,
          repo.defaultBranch,
          repo.branches
        )
      }
    }
  }

  override
  def getRepository(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[TequilaRepository] = {
    val RepositoryDesc.Tequila(sciper, name) = desc

    localService.getRepository(RepositoryDesc.Local(name)) map { repo =>
      TequilaRepository(
        user.tequila.get.publicId.serviceUserId.value,
        repo.name,
        repo.defaultBranch,
        repo.branches,
        repo.remote
      )
    }
  }

  override
  def getWorkingCopy(desc: RepositoryDesc): Option[GitWorkingCopy] = desc match {
    case RepositoryDesc.Tequila(sciper, name) =>
      val path = Paths.get(rootDir.getPath, name)
      Some(new GitWorkingCopy(path.toFile, user))

    case _ => None
  }

}

object TequilaRepositoryService {

  def apply(user: User, tequilaDir: File): Option[TequilaRepositoryService] =
    for {
      tequila <- user.tequila
      tequilaId = tequila.publicId.serviceUserId.value
      rootDir = Paths.get(tequilaDir.getPath, tequilaId).toFile
      if RootDir.isValid(rootDir)
      localService <- LocalRepositoryService(user, rootDir)
    }
    yield new TequilaRepositoryService(user, rootDir, localService)

}

private
object RootDir {
  def isValid(d: File): Boolean = {
    d.exists && d.isDirectory && d.canWrite
  }
}

