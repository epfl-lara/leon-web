
package leon.web
package workers

import akka.actor._

import play.api._
import play.api.libs.json._
import play.api.libs.json.Json._

import leon.web.services._
import leon.web.models._
import leon.web.shared._

class RepositoryWorker(session: ActorRef) extends BaseActor with JsonWrites {

  import ConsoleProtocol._

  // implicit val workerExecutionContext = akkaSystem.dispatchers.lookup("repository-worker")

  import play.api.libs.concurrent.Execution.Implicits._

  private var currentUser: Option[User] = None

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

  def receive = {

    case UserUpdated(user) =>
      currentUser = user

    case LoadRepositories(user) =>
      clientLog(s"Fetching repositories list...")

      val result = RepositoryProvider.forUser(user)

      result onSuccess { case repos =>
        clientLog(s"=> DONE")

        event("repositories_loaded", Map(
          "repos" -> toJson(repos)
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

        val wc = GitService.getWorkingCopy(user, repo, Some(token))

        val progressActor = Akka.system.actorOf(Props(
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
      val (owner, name) = (repo.owner, repo.name)
      val wc = GitService.getWorkingCopy(user, owner, name)

      clientLog(s"Listing files in '$owner/$name'...")

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

    case LoadFile(user, repoDesc, file) => withGitHubToken(user) { token =>
      clientLog(s"Loading file '$file'...")

      val gh     = GitHubService(token)
      val result = gh.getRepository(repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$owner/$name'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case repo =>
        val (owner, name) = (repo.owner, repo.name)
        val wc = GitService.getWorkingCopy(user, owner, name)

        if (!wc.exists) {
          logInfo(s"Could not find a working copy for repository '$owner/$name'")
          notifyError(s"Could not find a working copy for repository '$owner/$name', please load it again.")
        }
        else {
          wc.getFile("HEAD", file) match {
            case None =>
              notifyError(s"Could not find file '$file' in '$owner/$name'.")

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
    }

    case SwitchBranch(user, repoDesc, branch) => withGitHubToken(user) { token =>
      clientLog(s"Checking out branch '$branch'...")

      val gh     = GitHubService(token)
      val result = gh.getRepository(repoDesc)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$owner/$name'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case repo =>
        val (owner, name) = (repo.owner, repo.name)
        val wc = GitService.getWorkingCopy(user, owner, name)

        if (!wc.exists) {
          logInfo(s"Could not find a working copy for repository '$owner/$name'")
          notifyError(s"Could not find a working copy for repository '$owner/$name', please load it again.")
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
    }

    case DoGitOperation(user, project, op) => withGitHubToken(user) { token =>
      clientLog(s"Performing Git operation: $op")

      val (owner, name) = (project.owner, project.repo)
      val gh            = GitHubService(token)
      val result        = gh.getRepository(owner, name)

      result onFailure { case err =>
        notifyError(s"Failed to load repository '$owner/$name'. Reason: '${err.getMessage}'");
      }

      result onSuccess { case _ =>
        val wc = GitService.getWorkingCopy(user, owner, name, Some(token))

        if (!wc.exists) {
          logInfo(s"Could not find a working copy for repository '$owner/$name'")
          notifyError(s"Could not find a working copy for repository '$owner/$name', please load it again.")
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
              val progressActor = Akka.system.actorOf(Props(
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
    }

    case DoCancel =>
      sender ! Cancelled(this)

    case msg =>
      clientLog(s"RepositoryActor received an unknown message: $msg")

  }

  def pushMessage(v: JsValue): Unit =
    session ! NotifyClient(v)

}

