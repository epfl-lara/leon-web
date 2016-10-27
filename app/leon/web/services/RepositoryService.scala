/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package services

import play.Play

import java.io.File
import java.nio.file.Paths

import scala.concurrent.{Future, ExecutionContext}

import leon.web.models.{User, GitWorkingCopy}
import leon.web.shared._

abstract class RepositoryService(val provider: Provider, val rootDir: File) {

  type R <: Repository

  def listRepositories()(implicit ec: ExecutionContext): Future[Seq[R]]

  def getRepository(desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[R]

}

case object ServiceUnavailable extends Exception("Service unavailable")

object RepositoryService {

  private
  val tequilaRootDir = new File(
    Play.application.configuration.getString("repositories.tequila.path")
  )

  private
  val githubRootDir = new File(
    Play.application.configuration.getString("repositories.github.path")
  )

  private
  val providers = Seq(Provider.GitHub, Provider.Tequila)

  def apply(user: User, provider: Provider): Option[RepositoryService] = provider match {
    case Provider.GitHub  => GitHubRepositoryService(user, githubRootDir)
    case Provider.Tequila => TequilaRepositoryService(user, tequilaRootDir)
    case _                => None
  }

  def apply(user: User, desc: RepositoryDesc): Option[RepositoryService] = {
    apply(user, desc.provider)
  }

  def listRepositoriesByProvider(user: User)(implicit ec: ExecutionContext): Future[Map[Provider, Seq[Repository]]] = {
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

  def getRepository(user: User, desc: RepositoryDesc)(implicit ec: ExecutionContext): Future[Repository] = {
    RepositoryService(user, desc)
      .map(_.getRepository(desc))
      .getOrElse(Future.failed(ServiceUnavailable))
  }

  def getWorkingCopy(user: User, desc: RepositoryDesc): Option[GitWorkingCopy] =
    desc match {
      case RepositoryDesc.GitHub(owner, name) =>
        for {
          service <- GitHubRepositoryService(user, githubRootDir)
          rootDir = service.rootDir.getPath
          token   = service.token
          path    = Paths.get(rootDir, user.userId.value, owner, name)
        }
        yield new GitWorkingCopy(path.toFile, user, Some(token))

      case RepositoryDesc.Tequila(sciper, name) =>
        for {
          service <- TequilaRepositoryService(user, tequilaRootDir)
          rootDir = service.rootDir.getPath
          path    = Paths.get(rootDir, name)
        }
        yield new GitWorkingCopy(path.toFile, user)

      case _ =>
        None
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

    LocalRepository(
      name          = dir.getName,
      path          = dir.getPath,
      defaultBranch = defBranch,
      branches      = branches
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
        repo.branches
      )
    }
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

