
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
import leon.web.shared.{User => SharedUser, Identity => SharedIdentity, _ }
import leon.web.utils.String._
import leon.web.models.StandaloneJsonWrites
import shared.git._

class RepositoryWorker(session: ActorRef, user: Option[User])
  extends BaseActor with Actor with RepositoryWorkerHelpers {

  import ConsoleProtocol._
  import StandaloneJsonWrites._

  import context.dispatcher

  var currentUser: Option[User] = user

  import shared.messages.{DoCancel => _, _}
  
  def receive = {

    case OnClientEvent(_, event: RepositoryModule) =>

      event match {
        case DoUpdateCodeInProject(repo, file, branch, code) => withUser { user =>
          val project = Project(repo, branch, file)
          self ! ConsoleProtocol.UpdateCode(code, Some(user), Some(project))
        }
        case LoadRepositories => withUser { user =>
          self ! ULoadRepositories(user)
        }
        case LoadRepository(repo) => withUser { user =>
          self ! ULoadRepository(user, repo)
        }
        case LoadFile(repo, file) => withUser { user =>
          self ! ULoadFile(user, repo, file)
        }
        case SwitchBranch(repo, branch) => withUser { user =>
          self ! USwitchBranch(user, repo, branch)
        }
        case DoGitOperation(op, project) => withUser { user =>
          self ! UDoGitOperation(user, project, op)
        }
      }

    case OnClientEvent(_, event) =>
      notifyError(s"Could not process $event from RepositoryWorker")
      
    case UUserUpdated(user) =>
      currentUser = user

    case ULoadRepositories(user) =>
      clientLog(s"Fetching repositories list...")

      val providers = RepositoryProvider.forUser(user)
      val result = Future.sequence {
        providers.map { p =>
          p.listRepositories().map(p.provider -> _)
        }
      }

      result onSuccess { case repos =>
        clientLog(s"=> DONE")

        event(RepositoriesLoaded(repos.toMap))
      }

      result onFailure { case err =>
        notifyError(s"Failed to load repositories. Reason: '${err.getMessage}'")
      }

    case ULoadRepository(user, repoDesc) =>
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

          URepositoryLoaded(user, repo, wc.branchName())
        }

        future pipeTo self
      }

    case URepositoryLoaded(user, repo, currentBranch) =>
      val wc = GitService.getWorkingCopy(user, repo.desc)

      clientLog(s"Listing files in '${repo.desc}'...")

      val future = Future {
        wc.getFiles(currentBranch)
          .getOrElse(Seq[String]())
          .filter(_.extension === "scala")
      }

      future foreach { files =>
        clientLog(s"=> DONE")
        event(RepositoryLoaded(
          repo          = repo,
          files         = files.toArray,
          branches      = repo.branches.toArray,
          currentBranch = currentBranch)
        )
      }

    case ULoadFile(user, repoDesc, file) =>
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

              event(FileLoaded(file, content))
          }
        }
      }

    case USwitchBranch(user, repoDesc, branch) =>
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
              event(BranchChanged(
                success = true,
                branch  = Some(branch),
                files   = Some(files.toArray)
              ))

            case None =>
              val error = s"Failed to checkout branch '$branch', please commit " +
                          s"or reset your changes and try again."

              clientLog(s"=> ERROR: $error")
              notifyError(error)

              event(BranchChanged(
                success = true,
                error   = Some(error)
              ))
          }
        }
      }

    case UDoGitOperation(user, project, op) => 
      clientLog(s"Performing Git operation: $op")
      project.repo match {
        case repo: GitHubRepository=>
          withGitHubToken(user) { token =>
            val rs     = RepositoryService(user)
            val repoDesc = repo.desc
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
                  case GitStatus =>
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
      
                        event(GitOperationDone(
                          op      = op,
                          success = true,
                          data    = GitStatusDiff(
                            status = statusData.mapValues(_.asScala.toSet),
                            diff   = diffData
                          )
                        ))
      
                      case None =>
                        event(GitOperationDone(
                          op      = op,
                          success = false
                        ))
                    }
      
                  case GitPush(force) =>
                    val success = wc.push(force)
      
                    clientLog(s"=> DONE")
                    event(GitOperationDone(
                      op      = op,
                      success = success
                    ))
      
                  case GitPull =>
                    val progressActor = context.actorOf(Props(
                      classOf[JGitProgressWorker],
                      "git_progress", self
                    ))
      
                    val progressMonitor = new JGitProgressMonitor(progressActor)
      
                    val success = wc.pull(Some(progressMonitor))
      
                    clientLog(s"=> DONE")
                    event(GitOperationDone(
                      op      = op,
                      success = success
                    ))
      
                  case GitReset =>
                    val success = wc.reset(hard = true)
      
                    clientLog(s"=> DONE")
                    event(GitOperationDone(
                      op      = op,
                      success = success
                    ))
      
                  case GitCommit(message) =>
                    val success = wc.add(project.file) && wc.commit(message)
      
                    clientLog(s"=> DONE")
                    event(GitOperationDone(
                      op      = op,
                      success = success
                    ))
      
                  case GitLog(count) =>
                    val commits = wc.getLastCommits(count)
      
                    clientLog(s"=> DONE")
                    event(GitOperationDone(
                      op      = op,
                      success = commits.nonEmpty,
                      data    = GitCommits(commits.map(_.toSharedCommit).toSeq)
                    ))
                }
              }
            }
          }
        case project: LocalRepository =>
          // TODO/ 
      }

    case DoCancel =>
      sender ! Cancelled(this)

    case msg =>
      clientLog(s"RepositoryActor received an unknown message: $msg")

  }

  def pushMessage(v: Array[Byte]) = session ! NotifyClientBin(v)

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
