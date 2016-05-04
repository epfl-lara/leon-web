
package leon.web
package workers

import scala.io.Source
import scala.concurrent.Future
import scala.collection.JavaConverters._

import akka.actor._
import akka.pattern._

import play.api._
import play.api.libs.json._
import play.api.libs.json.Json._

import play.api.libs.concurrent.Execution.Implicits._

import leon.web.services._
import leon.web.models._
import leon.web.shared._
import leon.web.utils.String._

class RepositoryWorker(session: ActorRef, user: Option[User])
  extends BaseActor with Actor with RepositoryWorkerHelpers {

  import ConsoleProtocol._
  import StandaloneJsonWrites._

  import context.dispatcher

  var currentUser: Option[User] = user

  def receive = {

    case OnClientEvent(_, event) =>

      (event \ "action").as[String] match {

        case Action.doUpdateCodeInProject => withUser { user =>
          val branch   = (event \ "branch"   ) .as[String]
          val file     = (event \ "file"     ) .as[String]
          val code     = (event \ "code"     ) .as[String]

          val repo    = RepositoryService.parseRepositoryDesc(event \ "repo").get
          val project = Project(repo, branch, file)

          self ! UpdateCode(code, Some(user), Some(project))
        }

        case Action.loadRepositories => withUser { user =>
          self ! LoadRepositories(user)
        }

        case Action.loadRepository => withUser { user =>
          val repo = RepositoryService.parseRepositoryDesc(event \ "repo").get
          self ! LoadRepository(user, repo)
        }

        case Action.loadFile => withUser { user =>
          val file = (event \ "file").as[String]
          val repo = RepositoryService.parseRepositoryDesc(event \ "repo").get

          self ! LoadFile(user, repo, file)
        }

        case Action.switchBranch => withUser { user =>
          val branch = (event \ "branch").as[String]
          val repo   = RepositoryService.parseRepositoryDesc(event \ "repo").get

          self ! SwitchBranch(user, repo, branch)
        }

        case Action.doGitOperation => withUser { user =>
          val project = Project(
            repo    = RepositoryService.parseRepositoryDesc(event \ "repo").get,
            branch  = (event \ "project" \ "branch").as[String],
            file    = (event \ "project" \ "file"  ).as[String]
          )

          val op = (event \ "op").as[String] match {
            case GitOperation.STATUS => GitOperation.Status
            case GitOperation.PULL   => GitOperation.Pull
            case GitOperation.RESET  => GitOperation.Reset

            case GitOperation.LOG    =>
              val count = (event \ "data" \ "count").as[Int]
              GitOperation.Log(count)

            case GitOperation.PUSH   =>
              val force = (event \ "data" \ "force").as[Boolean]
              GitOperation.Push(force)

            case GitOperation.COMMIT =>
              val msg = (event \ "data" \ "msg").as[String]
              GitOperation.Commit(msg)
          }

          self ! DoGitOperation(user, project, op)
        }
      }

    case UserUpdated(user) =>
      currentUser = user

    case LoadRepositories(user) =>
      clientLog(s"Fetching repositories list...")

      val result = RepositoryProvider.forUser(user)

      result onSuccess { case repos =>
        clientLog(s"=> DONE")

        event("repositories_loaded", Map(
          "repos" -> toJson(repos map { case (p, v) => (p.id, v) })
        ))
      }

      result onFailure { case err =>
        notifyError(s"Failed to load repositories. Reason: '${err.getMessage}'")
      }

    case LoadRepository(user, repoDesc) =>
      clientLog(s"Fetching repository information...")

      val rs     = RepositoryService(user)
      val result = rs.fromDesc(repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$repoDesc'. Reason: '${err.getMessage}'");
      }

      // FIXME: Is it really asynchronous/concurrent "enough"?
      result onSuccess { case repo =>
        clientLog(s"=> DONE")

        val wc = GitService.getWorkingCopy(user, repoDesc)

        val progressActor = context.actorOf(Props(
          classOf[JGitProgressWorker],
          "git_progress", self
        ))

        val progressMonitor = new JGitProgressMonitor(progressActor)

        val future = Future {
          if (!wc.exists) {
            clientLog(s"Cloning repository '$repoDesc'...")
            wc.cloneRepo(repo.cloneURL, Some(progressMonitor))
            clientLog(s"=> DONE")
          }
          else {
            clientLog(s"Pulling repository '$repoDesc'...")
            wc.pull(Some(progressMonitor))
            clientLog(s"=> DONE")
          }

          RepositoryLoaded(user, repo, wc.branchName())
        }

        future pipeTo self
      }

    case RepositoryLoaded(user, repo, currentBranch) =>
      val wc = GitService.getWorkingCopy(user, repo.desc)

      clientLog(s"Listing files in '${repo.desc}'...")

      val future = Future {
        wc.getFiles(currentBranch)
          .getOrElse(Seq[String]())
          .filter(_.extension === "scala")
      }

      future foreach { files =>
        clientLog(s"=> DONE")
        event("repository_loaded", Map(
          "repository"    -> toJson(repo),
          "files"         -> toJson(files),
          "branches"      -> toJson(repo.branches),
          "currentBranch" -> toJson(currentBranch)
        ))
      }

    case LoadFile(user, repoDesc, file) =>
      clientLog(s"Loading file '$file'...")

      val rs     = RepositoryService(user)
      val result = rs.fromDesc(repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$repoDesc'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case repo =>
        val wc = GitService.getWorkingCopy(user, repoDesc)

        if (!wc.exists) {
          logInfo(s"Could not find a working copy for repository '$repoDesc'")
          notifyError(s"Could not find a working copy for repository '$repoDesc', please load it again.")
        }
        else {
          wc.getFile("HEAD", file) match {
            case None =>
              notifyError(s"Could not find file '$file' in '$repoDesc'.")

            case Some((_, _, path)) =>
              val filePath = s"${wc.path}/$path"
              val content  = Source.fromFile(filePath).mkString

              clientLog(s"=> DONE")

              event("file_loaded", Map(
                "file"    -> toJson(file),
                "content" -> toJson(content)
              ))
          }
        }
      }

    case SwitchBranch(user, repoDesc, branch) =>
      clientLog(s"Checking out branch '$branch'...")

      val rs     = RepositoryService(user)
      val result = rs.fromDesc(repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$repoDesc'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case repo =>
        val wc = GitService.getWorkingCopy(user, repoDesc)

        if (!wc.exists) {
          logInfo(s"Could not find a working copy for repository '$repoDesc'")
          notifyError(s"Could not find a working copy for repository '$repoDesc', please load it again.")
        }
        else {
          val future = Future {
            val switched =
              if (wc.branchExists(branch))
                wc.checkout(branch)
              else
                wc.checkoutRemote(branch)

            if (switched) {
              val files = wc.getFiles(branch)
                            .getOrElse(Seq[String]())
                            .filter(_.extension === "scala")

              Some(files)
            }
            else
              None
          }

          future onSuccess {
            case Some(files) =>
              clientLog(s"=> DONE")
              event("branch_changed", Map(
                "success" -> toJson(true),
                "branch"  -> toJson(branch),
                "files"   -> toJson(files)
              ))

            case None =>
              val error = s"Failed to checkout branch '$branch', please commit " +
                          s"or reset your changes and try again."

              clientLog(s"=> ERROR: $error")
              notifyError(error)

              event("branch_changed", Map(
                "success" -> toJson(true),
                "error"   -> toJson(error)
              ))
          }
        }
      }

    case DoGitOperation(user, project, op) =>
      clientLog(s"Performing Git operation: $op")

      val repoDesc = project.repo

      val rs     = RepositoryService(user)
      val result = rs.fromDesc(repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '${repoDesc}'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case _ =>
        val wc = GitService.getWorkingCopy(user, repoDesc)

        if (!wc.exists) {
          logInfo(s"Could not find a working copy for repository '$repoDesc'")
          notifyError(s"Could not find a working copy for repository '$repoDesc', please load it again.")
        }
        else {
          op match {
            case GitOperation.Status =>
              val status = wc.status()
              val diff   = wc.diff(Some("HEAD"), None)
              clientLog(s"=> DONE")

              status match {
                case Some(status) =>
                  val statusData = Map(
                    "added"       -> status.getAdded(),
                    "changed"     -> status.getChanged(),
                    "modified"    -> status.getModified(),
                    "removed"     -> status.getRemoved(),
                    "conflicting" -> status.getConflicting(),
                    "missing"     -> status.getMissing(),
                    "untracked"   -> status.getUntracked()
                  )

                  val diffData = diff.getOrElse("")

                  event("git_operation_done", Map(
                    "op"      -> toJson(op.name),
                    "success" -> toJson(true),
                    "data"    -> toJson(Map(
                      "status" -> toJson(statusData.mapValues(_.asScala.toSet)),
                      "diff"   -> toJson(diffData)
                    ))
                  ))

                case None =>
                  event("git_operation_done", Map(
                    "op"      -> toJson(op.name),
                    "success" -> toJson(false)
                  ))
              }

            case GitOperation.Push(force) =>
              val success = wc.push(force)

              clientLog(s"=> DONE")
              event("git_operation_done", Map(
                "op"      -> toJson(op.name),
                "success" -> toJson(success)
              ))

            case GitOperation.Pull =>
              val progressActor = context.actorOf(Props(
                classOf[JGitProgressWorker],
                "git_progress", self
              ))

              val progressMonitor = new JGitProgressMonitor(progressActor)

              val success = wc.pull(Some(progressMonitor))

              clientLog(s"=> DONE")
              event("git_operation_done", Map(
                "op"      -> toJson(op.name),
                "success" -> toJson(success)
              ))

            case GitOperation.Reset =>
              val success = wc.reset(hard = true)

              clientLog(s"=> DONE")
              event("git_operation_done", Map(
                "op"      -> toJson(op.name),
                "success" -> toJson(success)
              ))

            case GitOperation.Commit(message) =>
              val success = wc.add(project.file) && wc.commit(message)

              clientLog(s"=> DONE")
              event("git_operation_done", Map(
                "op"      -> toJson(op.name),
                "success" -> toJson(success)
              ))

            case GitOperation.Log(count) =>
              val commits = wc.getLastCommits(count)

              clientLog(s"=> DONE")
              event("git_operation_done", Map(
                "op"      -> toJson(op.name),
                "success" -> toJson(commits.nonEmpty),
                "data"    -> toJson(commits.map(_.toJson))
              ))
          }
        }
      }

    case DoCancel =>
      sender ! Cancelled(this)

    case msg =>
      clientLog(s"RepositoryActor received an unknown message: $msg")

  }

  def pushMessage(v: JsValue): Unit =
    session ! NotifyClient(v)

}

trait RepositoryWorkerHelpers { self: RepositoryWorker =>

  def withUser(f: User => Unit): Unit = currentUser match {
    case Some(user) =>
      f(user)

    case None =>
      notifyError("You need to log-in to perform this operation.")
      logInfo("Cannot perform this operation when user is not logged-in.")
  }

  def withGitHubUser(f: User => Unit): Unit = currentUser match {
    case Some(user) if user.github.isDefined =>
      f(user)

    case None =>
      notifyError("You need to log-in with GitHub to perform this operation.")
      logInfo("Cannot perform this operation when user is not logged-in with GitHub.")
  }

  def withGitHubToken(user: User)(f: String => Unit): Unit = {
    val token = user.github.flatMap(_.oAuth2Info).map(_.accessToken)

    token match {
      case Some(token) =>
        f(token)

      case None =>
        notifyError("You need to log-in again with GitHub to perform this operation.")
        logInfo("Cannot perform this operation when user has no OAuth token.")
    }
  }

  def withTequilaUser(f: User => Unit): Unit = currentUser match {
    case Some(user) if user.tequila.isDefined =>
      f(user)

    case None =>
      notifyError("You need to log-in with Tequila to perform this operation.")
      logInfo("Cannot perform this operation when user is not logged-in with GitHub.")
  }

}