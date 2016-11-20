
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
import shared.git._

class RepositoryWorker(session: ActorRef, user: Option[User]) extends BaseActor with Actor {

  import ConsoleProtocol._

  import context.dispatcher
  import shared.messages.{DoCancel => _, _}

  var currentUser: Option[User] = user

  def withUser(f: User => Unit): Unit = currentUser match {
    case Some(user) =>
      f(user)

    case None =>
      notifyError("You need to log-in to perform this operation.")
      logInfo("Cannot perform this operation when user is not logged-in.")
  }

  def receive = {

    case OnClientEvent(_, event: RepositoryModule) => event match {

      case DoUpdateCodeInRepository(code, state, requestId) => withUser { user =>
        session ! UpdateCode(code, Some(user), Some(state), requestId)
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

      val result = RepositoryService.listRepositoriesByProvider(user)

      result onSuccess { case repos =>
        clientLog(s"=> DONE")

        event(RepositoriesLoaded(repos))
      }

      result onFailure { case err =>
        notifyError(s"Failed to load repositories. Reason: '${err.getMessage}'")
      }

    case ULoadRepository(user, repoDesc) =>
      clientLog(s"Fetching repository information...")

      val result = RepositoryService.getRepository(user, repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$repoDesc'. Reason: '${err.getMessage}'");
      }

      result onSuccess {
        case repo: GitHubRepository =>
          clientLog(s"=> DONE")

          val wc = RepositoryService.getWorkingCopy(user, repoDesc).get

          val progressActor = context.actorOf(Props(
            classOf[JGitProgressWorker],
            "git_progress", session
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

        case repo =>
          val wc = RepositoryService.getWorkingCopy(user, repoDesc).get

          if (wc.exists) {
            clientLog(s"=> DONE")
            self ! URepositoryLoaded(user, repo, wc.branchName())
          } else {
            notifyError(s"Failed to load repository '$repoDesc'. Reason: 'Repository does not exists'");
          }
      }

    case URepositoryLoaded(user, repo, currentBranch) =>
      val wc = RepositoryService.getWorkingCopy(user, repo.desc).get

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
          currentBranch = currentBranch
        ))
      }

    case ULoadFile(user, repoDesc, file) =>
      clientLog(s"Loading file '$file'...")

      val result = RepositoryService.getRepository(user, repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$repoDesc'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case repo =>
        val wc = RepositoryService.getWorkingCopy(user, repoDesc).get

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

      val result = RepositoryService.getRepository(user, repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$repoDesc'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case repo =>
        val wc = RepositoryService.getWorkingCopy(user, repoDesc).get

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

      val repo     = project.repo
      val repoDesc = repo.desc
      val result   = RepositoryService.getRepository(user, repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '${repoDesc}'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case _ =>
        val wc = RepositoryService.getWorkingCopy(user, repoDesc).get

        if (!wc.exists) {
          logInfo(s"Could not find a working copy for repository '$repoDesc'")
          notifyError(s"Could not find a working copy for repository '$repoDesc', please load it again.")
        }
        else {
          op match {
            case GitStatus =>
              val status = wc.status()
              val diff   = wc.diff(Some("HEAD"), None)

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

                  clientLog(s"=> DONE")
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

    case DoCancel =>
      sender ! Cancelled(this)

    case msg =>
      clientLog(s"RepositoryWorker received an unknown message: $msg")

  }

  def pushMessage(v: Array[Byte]) = session ! NotifyClientBin(v)

}

