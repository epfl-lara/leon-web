/* Copyright 2009-2015 EPFL, Lausanne */

package leon.web
package models

import scala.util.control._

import play.api._
import play.api.libs.json._
import play.api.Play.current
import play.api.Logger

import java.io.File
import java.io.InputStream
import java.io.ByteArrayOutputStream

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import org.eclipse.jgit.lib._
import org.eclipse.jgit.api._
import org.eclipse.jgit.api.errors._
import org.eclipse.jgit.revwalk._
import org.eclipse.jgit.storage.file._
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.api.ListBranchCommand.ListMode
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider
import org.eclipse.jgit.transport.RemoteRefUpdate

/** Provides a type-safe wrapper around a subset of the JGit API.
  *
  * @author Etienne Kneuss (etienne.kneuss@epfl.ch)
  */
class RepositoryInfos(val path: File, user: User, token: Option[String] = None) {
  import scala.collection.JavaConversions._

  case class Walker(tw: TreeWalk) {
    def map[T](f : TreeWalk => T): List[T] = {
      var res: List[T] = Nil

      while(tw.next) {
        res = f(tw) :: res
      }

      res.reverse
    }
  }

  lazy val repo = {
    new FileRepositoryBuilder().setWorkTree(path).build()
  }

  lazy val git = {
    new Git(repo)
  }

  lazy val credentials = token map { value =>
    new UsernamePasswordCredentialsProvider("token", value)
  }

  private def withCredentials[A <: TransportCommand[_, _]](cmd: A): A = {
    credentials foreach { p => cmd.setCredentialsProvider(p) }
    cmd
  }

  def branches(all: Boolean = true): Iterable[Ref] = {
    val mode = if (all) ListMode.ALL else null

    git.branchList().setListMode(mode).call()
  }

  def branch(): Ref =
    repo.getRef(Constants.HEAD).getTarget()

  def branchName(full: Boolean = false): String =
    if (full) repo.getFullBranch() else repo.getBranch()

  def getLastCommits(n: Int = 5): Iterable[Commit] = {
    try {
      val log = git.log;
      branches(true).foreach { b =>
        log.add(b.getObjectId())
      }
      log.setMaxCount(n).call().map(Commit(_))
    } catch {
      case e: GitAPIException =>
        Nil
    }
  }

  def getCommit(hash: String): Option[Commit] = {
    try {
      Some(Commit(new RevWalk(repo).parseCommit(repo.resolve(hash))))
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        None
    }
  }

  def getTreeIterator(commit: String) = {
    val id = repo.resolve(commit)
    val p  = new CanonicalTreeParser()
    val or = repo.newObjectReader()
    try {
      p.reset(or, new RevWalk(repo).parseTree(id))
    } finally {
      // or.release()
    }
    p
  }

  def diff(from: Option[String], to: Option[String]): Option[String] = {
    try {
      val out = new ByteArrayOutputStream()
      val cmd = git.diff().setOutputStream(out)

      from.foreach(ref => cmd.setOldTree(getTreeIterator(ref)))
      to.foreach(ref => cmd.setNewTree(getTreeIterator(ref)))

      cmd.call()

      Some(out.toString)
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        None
    }
  }

  def getFile(commit: String, name: String): Option[(Array[Byte], Commit, String)] = {
    try {
      val id = repo.resolve(commit)
      val rw = new RevWalk(repo).parseCommit(id)
      val c = Commit(rw)
      val tree = rw.getTree()
      val tw = TreeWalk.forPath(repo, name, tree)

      Some((repo.open(tw.getObjectId(0)).getBytes, c, tw.getPathString))
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        None
    }
  }

  def getFiles(commit: String): Option[List[String]] = {
    try {
      val id = repo.resolve(commit)
      val tree = new RevWalk(repo).parseCommit(id).getTree()
      val tw = new TreeWalk(repo)
      tw.addTree(tree)
      tw.setRecursive(true);

      Some(Walker(tw).map(tw => tw.getPathString))
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        None
    }
  }

  def exists: Boolean = {
    path.isDirectory
  }

  def cloneRepo(remoteURI: String, progressMonitor: Option[ProgressMonitor] = None): Boolean = {
    if (exists) {
      throw new Exception("Repository "+path+" already exists")
    }
    try {
      val cmd = Git.cloneRepository()
      cmd.setDirectory(path)
         .setRemote("origin")
         .setCloneAllBranches(true)
         .setURI(remoteURI)

      progressMonitor.foreach(cmd.setProgressMonitor(_))

      withCredentials(cmd).call()
      true
    }
    catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }
  }

  def setOrigin(origin: String): Boolean = {
    try {
      val config = git.getRepository().getConfig();
      config.setString("remote", "origin", "url", origin);
      config.save();
      true
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }
  }

  def status(): Option[Status] = {
    try {
      Some(git.status().call())
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        None
    }
  }

  def pull(progressMonitor: Option[ProgressMonitor] = None): Boolean = {
    try {
      val config = git.getRepository().getConfig();
      config.setString("branch", "master", "remote", "origin");
      config.setString("branch", "master", "merge", "refs/heads/master");
      config.save();

      val cmd = git.pull().setRebase(false)
      progressMonitor.foreach(cmd.setProgressMonitor(_))
      withCredentials(cmd).call()
      true
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }
  }

  def push(force: Boolean = false): Boolean = {
    try {
      val cmd      = git.push().setRemote("origin").setForce(force)
      val results  = withCredentials(cmd).call()
      val statuses = results.map { result =>
        result.getRemoteUpdates().forall(_.getStatus() match {
          case RemoteRefUpdate.Status.OK         => true
          case RemoteRefUpdate.Status.UP_TO_DATE => true
          case _                                 => false
        })
      }

      statuses.forall(identity)

    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }

  }

  def reset(ref: String = "HEAD", hard: Boolean = false): Boolean = {
    try {
      val mode = if (hard) ResetCommand.ResetType.HARD else ResetCommand.ResetType.SOFT
      git.reset()
         .setRef(ref)
         .setMode(mode)
         .call()
      true
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }
  }

  def add(path: String): Boolean = {
    try {
      git.add().addFilepattern(path).call()
      true
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }
  }

  def rm(path: String): Boolean = {
    try {
      git.rm().addFilepattern(path).call()
      true
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }
  }

  def commit(msg: String): Boolean = {
    try {
      val userId = user.userId.value
      val name   = user.nameOrEmail.getOrElse(userId)
      val email  = user.email.map(_.value).getOrElse(userId)

      git.commit()
         .setAll(true)
         .setCommitter(name, email)
         .setMessage(msg)
         .call()

      true
    } catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage, e)
        false
    }

  }

  def branchExists(branch: String): Boolean =
    git.getRepository().getRef(branch) =!= null

  def checkout(branch: String, force: Boolean = false): Boolean = {
    try {
      if (force) reset("HEAD", true)
      git.checkout()
         .setName(branch)
         .setForce(force)
         .call()
      true
    }
    catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage(), e)
        false
    }
  }

  def checkoutRemote(branch: String, force: Boolean = false): Boolean = {
    try {
      if (force) reset("HEAD", true)
      git.checkout()
         .setName(branch)
         .setForce(force)
         .setCreateBranch(true)
         .setUpstreamMode(CreateBranchCommand.SetupUpstreamMode.TRACK)
         .setStartPoint(s"origin/$branch")
         .call()

      true
    }
    catch {
      case NonFatal(e) =>
        Logger.error(e.getMessage(), e)
        false
    }
  }

}

case class Commit(revc: RevCommit) {
  def hash          = ObjectId.toString(revc)
  def shortHash     = hash.substring(0,10)
  def shortMessage  = revc.getShortMessage
  def fullMessage   = revc.getFullMessage
  def commitTime    = new DateTime(revc.getCommitTime*1000l)
  def commitTimeStr = {
    commitTime.toString(DateTimeFormat.forPattern("dd.MM.YYYY, HH:mm:ss"))
  }
  def author        = revc.getAuthorIdent().getName
  def committer     = revc.getCommitterIdent().getName
  def desc          = shortHash+" - "+shortMessage


  def toJson: JsValue = {
    Json.toJson(Map(
      "hash" -> hash,
      "shortHash" -> shortHash,
      "shortMessage" -> shortMessage,
      "fullMessage" -> fullMessage,
      "commitTime" -> commitTimeStr,
      "author" -> author,
      "committer" -> committer,
      "desc" -> desc
    ))
  }
  
  def toCommit: shared.messages.Commit = shared.messages.Commit(
      hash,
      shortHash,
      shortMessage,
      fullMessage,
      commitTimeStr,
      author,
      committer,
      desc
  )
}
