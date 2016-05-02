package leon.web

package models

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.util.Try
import scala.io.Source
import scala.collection.JavaConverters._

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import akka.pattern._

import play.api.Play.current

import leon.frontends.scalac._
import leon.utils.TemporaryInputPhase
import leon.utils.InterruptManager
import leon.utils.PreprocessingPhase

import leon.web.workers._
import leon.web.stores.{PermalinkStore, UserStore}
import leon.web.services.{GitService, GitHubService}

import leon.web.shared.{Action, Module, Project, Provider, GitOperation}
import leon.web.shared.{Repository, RepositoryType}
import leon.web.utils.String._

import java.io.File
import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicBoolean

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class ConsoleSession(remoteIP: String, user: Option[User]) extends Actor with BaseActor with JsonWrites {
  import context.dispatcher
  import ConsoleProtocol._

  val githubServiceTimeout =
    Play.current.configuration.getInt("services.github.timeout").getOrElse(10).seconds

  val (enumerator, channel) = Concurrent.broadcast[JsValue]
  var reporter: WSReporter = _

  def pushMessage(v: JsValue) = channel.push(v)

  var lastCompilationState: CompilationState = CompilationState.unknown

  def assumeCompiled[A](f: CompilationState => A) = {
    lastCompilationState match {
      case cstate if cstate.isCompiled =>
        f(cstate)
      case _ =>
        notifyError("Not compiled ?!")
        logInfo("Not compiled ?!")
    }
  }

  var currentUser = user

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

  case class ModuleContext(name: String, actor: ActorRef, var isActive: Boolean = false)

  var modules = Map[String, ModuleContext]()
  var cancelledWorkers = Set[WorkerActor]()
  var interruptManager: InterruptManager = _

  object ModuleEntry {
    def apply(name: String, worker: =>WorkerActor): (String, ModuleContext) = {
      name -> ModuleContext(name, Akka.system.actorOf(Props(worker)))
    }
  }
  
  def receive = {
    case Init =>
      reporter = new WSReporter(channel)
      sender ! InitSuccess(enumerator)


      interruptManager = new InterruptManager(reporter)

      modules += ModuleEntry(Module.verification  , new VerificationWorker(self, interruptManager))
      modules += ModuleEntry(Module.termination   , new TerminationWorker(self, interruptManager))
      modules += ModuleEntry(Module.synthesis     , new SynthesisWorker(self, interruptManager))
      modules += ModuleEntry(Module.disambiguation, new DisambiguationWorker(self, interruptManager))
      modules += ModuleEntry(Module.execution     , new ExecutionWorker(self, interruptManager))
      modules += ModuleEntry(Module.repair        , new RepairWorker(self, interruptManager))
      modules += ModuleEntry(Module.invariant     , new OrbWorker(self, interruptManager))

      logInfo("New client")

    case DoCancel =>
      cancelledWorkers = Set()
      logInfo("Starting Cancel Procedure...")
      interruptManager.interrupt()
      modules.values.foreach(_.actor ! DoCancel)

    case Cancelled(wa: WorkerActor)  =>
      cancelledWorkers += wa

      logInfo(cancelledWorkers.size+"/"+modules.size+": Worker "+wa.getClass+" notified its cancellation")
      if (cancelledWorkers.size === modules.size) {
        logInfo("All workers got cancelled, resuming normal operations")
        interruptManager.recoverInterrupt()
      }

    case NotifyClient(event) =>
      pushMessage(event)

    case ProcessClientEvent(event) =>
      try {
        logInfo("[<] "+(event \ "action").as[String] + " for " + (event \ "module").as[String])

        (event \ "module").as[String] match {
          case "main" =>
            (event \ "action").as[String] match {
              case Action.doCancel =>
                self ! DoCancel

              case Action.doUpdateCode =>
                self ! UpdateCode((event \ "code").as[String], None, None)

              case Action.doUpdateCodeInProject => withUser { user =>
                val repoDesc = (event \ "repoDesc" ) .as[String]
                val repoType = (event \ "repoType" ) .as[String]
                val branch   = (event \ "branch"   ) .as[String]
                val file     = (event \ "file"     ) .as[String]
                val code     = (event \ "code"     ) .as[String]

                val repo    = RepositoryDesc(repoDesc, RepositoryType(repoType))
                val project = Project(repo, branch, file)

                self ! UpdateCode(code, Some(user), Some(project))
              }

              case Action.storePermaLink =>
                self ! StorePermaLink((event \ "code").as[String])

              case Action.accessPermaLink =>
                self ! AccessPermaLink((event \ "link").as[String])

              case Action.loadRepositories => withGitHubUser { user =>
                self ! LoadRepositories(user)
              }

              case Action.loadRepository => withGitHubUser { user =>
                val repoDesc = (event \ "repoDesc").as[String]
                val repoType = RepositoryType((event \ "repoType").as[String])

                val repo = Repository(repoDesc, repoType)

                self ! LoadRepository(user, repo)
              }

              case Action.loadFile => withUser { user =>
                val repoDesc = (event \ "repoDesc").as[String]
                val repoType = RepositoryType((event \ "repoType").as[String])
                val file     = (event \ "file").as[String]

                val repo = Repository(repoDesc, repoType)

                self ! LoadFile(user, repo, file)
              }

              case Action.switchBranch => withUser { user =>
                val repoDesc = (event \ "repoDesc").as[String]
                val repoType = RepositoryType((event \ "repoType").as[String])
                val branch = (event \ "branch").as[String]

                val repo = Repository(repoDesc, repoType)

                self ! SwitchBranch(user, repo, branch)
              }

              case Action.doGitOperation => withUser { user =>
                val repoDesc = (event \ "project" \ "repoDesc").as[String]
                val repoType = RepositoryType((event \ "repoType").as[String])

                val project = Project(
                  repo    = Repository(repoDesc, repoType),
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

              case Action.unlinkAccount => withUser { user =>
                val provider = Provider((event \ "provider").as[String])
                self ! UnlinkAccount(user, provider)
              }

              case Action.featureSet =>
                val f      = (event \ "feature").as[String]
                val active = (event \ "active").as[Boolean]

                if (modules contains f) {
                  if (active) {
                    modules(f).isActive = true
                  } else {
                    modules(f).isActive = false
                  }
                }
            }

          case m if modules contains m =>
            if (modules(m).isActive) {
              modules(m).actor ! OnClientEvent(lastCompilationState, event)
            }

          case m =>
            notifyError("Module "+m+" not available.")

        }
      } catch {
        case t: Throwable =>
          notifyError("Could not process event: "+t.getMessage)
      }

    case DispatchTo(m: String, msg: Any) =>
      modules.get(m) match {
        case Some(m) if m.isActive =>
          m.actor ! msg
        case _ =>
      }

    case StorePermaLink(code) =>
      PermalinkStore.store(Code(code)) match {
        case Some(Permalink(link, _)) =>
          event("permalink", Map("link" -> toJson(link.value)))
        case _ =>
          notifyError("Could not create permalink")
      }

    case AccessPermaLink(link) =>
      PermalinkStore.get(Link(link)) match {
        case Some(Permalink(_, code)) =>
          event("replace_code", Map("newCode" -> toJson(code.value)))
        case None =>
          notifyError("Link not found ?!?: "+link)
      }

      case LoadRepositories(user) => withGitHubToken(user) { token =>
        clientLog(s"Fetching repositories list...")

        val gh     = GitHubService(token)
        val result = gh.listUserRepositories()

        result onSuccess { case repos =>
          clientLog(s"=> DONE")

          event("repositories_loaded", Map(
            "repos" -> toJson(repos)
          ))
        }

        result onFailure { case err =>
          notifyError(s"Failed to load repositories. Reason: '${err.getMessage}'")
        }
      }

    case LoadRepository(user, repoDesc) => withGitHubToken(user) { token =>
      clientLog(s"Fetching repository information...")

      val gh     = GitHubService(token)
      val result = gh.getRepository(repoDesc)

      result onFailure { case err =>
          notifyError(s"Failed to load repository '$owner/$name'. Reason: '${err.getMessage}'");
      }

      // FIXME: Is it really asynchronous/concurrent "enough"?
      result onSuccess { case repo =>
        clientLog(s"=> DONE")

        val (owner, name) = (repo.owner, repo.name)
        val wc = GitService.getWorkingCopy(user, owner, name, Some(token))

        val progressActor = Akka.system.actorOf(Props(
          classOf[JGitProgressWorker],
          "git_progress", self
        ))

        val progressMonitor = new JGitProgressMonitor(progressActor)

        val future = Future {
          if (!wc.exists) {
            clientLog(s"Cloning repository '$owner/$name'...")
            wc.cloneRepo(repo.cloneURL, Some(progressMonitor))
            clientLog(s"=> DONE")
          }
          else {
            clientLog(s"Pulling repository '$owner/$name'...")
            wc.pull(Some(progressMonitor))
            clientLog(s"=> DONE")
          }

          RepositoryLoaded(user, repo, wc.branchName())
        }

        future pipeTo self
      }
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

    case UnlinkAccount(user, provider) =>
      clientLog(s"Unlinking account '${provider.id}'...")

      user.identity(provider) match {
        case Some(id) =>
          import play.api.db._

          implicit val c = DB.getConnection()
          val newUser = UserStore.unlinkIdentity(user, id)
          currentUser = Some(newUser)

          clientLog("=> DONE")

          event("user_updated", Map(
            "user" -> toJson(newUser)
          ))

        case None =>
          clientLog("=> ERROR: No such account found.")
      }


    case UpdateCode(code, user, project) =>
      if (lastCompilationState.project =!= project ||
          lastCompilationState.code =!= Some(code)) {

        clientLog("Compiling...")
        logInfo(s"Code updated:\n$code")

        val savedFile = project match {
          case None =>
            saveCode(code)

          case Some(p) =>
            val path = {
              val wc   = GitService.getWorkingCopy(user.get, p.owner, p.repo)

              wc.getFile(p.branch, p.file)
                .map(_._3)
                .map(filePath => s"${wc.path.getAbsolutePath()}/$filePath")
            }

            saveCode(code, path.map(new File(_)))
        }

        val compReporter = new CompilingWSReporter(channel)
        var compContext  = leon.Main.processOptions(Nil).copy(reporter = compReporter)

        val optProgram = try {
          val pipeline = ExtractionPhase andThen
                         (new PreprocessingPhase(false))

        // We need both a logged-in user and a project to
        // load files from the repository
         val files = user.zip(project).headOption match {
            case None =>
              savedFile.getAbsolutePath() :: Nil

            case Some((user, Project(repo, branch, file, _))) =>
              val wc = GitService.getWorkingCopy(user, repo)

              wc.getFiles(branch)
                .getOrElse(Seq[String]())
                .filter(_.extension === "scala")
                // replace the path to the file currently loaded
                // in the editor with the path to the temp file
                // `saveCode` just wrote.
                .map { f =>
                  if (f === file)
                    savedFile.getAbsolutePath()
                  else
                    s"${wc.path.getAbsolutePath()}/$f"
                }
                .toList
          }

         println(files)

          val (_, program) = pipeline.run(compContext, files)

          compReporter.terminateIfError

          Some(program)
        }
        catch {
          case e: java.nio.channels.ClosedChannelException =>
            logInfo("Channel closed")
            None

          case t: Throwable =>
            logInfo("Failed to compile and/or extract "+t)
            None
        }

        optProgram match {
          case Some(program) =>

            val cstate = CompilationState(
              optProgram = Some(program),
              code       = Some(code),
              compResult = "success",
              wasLoop    = Set(),
              project    = project,
              savedFile  = Some(savedFile.getName())
            )

            lastCompilationState = cstate

            event("compilation", Map("status" -> toJson("success")))

            clientLog("Compilation successful!")

            notifyMainOverview(cstate)

            lazy val isOnlyInvariantActivated = modules.values.forall(m =>
                ( m.isActive && m.name === Module.invariant) ||
                (!m.isActive && m.name =!= Module.invariant))

            lazy val postConditionHasQMark =
              program.definedFunctions.exists { funDef =>
                funDef.postcondition match {
                  case Some(postCondition) =>
                  import leon.purescala._
                  import Expressions._
                  ExprOps.exists {
                    case FunctionInvocation(callee, _) =>
                      leon.purescala.DefOps.fullName(callee.fd)(program) === "leon.invariant.?"
                    case _ =>
                      false
                  }(postCondition)
                  case None => false
                }
              }

            if (isOnlyInvariantActivated || postConditionHasQMark) {
              modules(Module.invariant).actor ! OnUpdateCode(cstate)
            } else {
              modules.values.filter(e => e.isActive && e.name =!= Module.invariant).foreach (_.actor ! OnUpdateCode(cstate))
            }

          case None =>
            for ((l,e) <- compReporter.errors) {
              logInfo(s"  ${e mkString "\n  "}")
            }

            clientLog("Compilation failed!")
            event("compilation", Map("status" -> toJson("failure")))

            lastCompilationState = CompilationState.failure(
              code, project, Some(savedFile.getName())
            )
        }

        val annotations = {
          compReporter.errors.map{ case (l,e) =>
            CodeAnnotation(l, 0, e.mkString("\n"), CodeAnnotationError)
          }.toSeq ++
          compReporter.warnings.map{ case (l,e) =>
            CodeAnnotation(l, 0, e.mkString("\n"), CodeAnnotationWarning)
          }.toSeq
        }.filter(_.line >= 0)

        notifyAnnotations(annotations)
      }
      else {
        val cstate = lastCompilationState
        event("compilation", Map("status" -> toJson(cstate.compResult)))
      }

    case Quit =>

    case msg =>
      clientLog("Unknown Actor Message: "+msg)
  }

  def notifyMainOverview(cstate: CompilationState): Unit = {
    def decodeName(name: String): String = {
      scala.reflect.NameTransformer.decode(name).replaceAll("\\$", ".")
    }
    if (cstate.isCompiled) {
      val facts = for (fd <- cstate.functions) yield {
        toJson(Map(
          "name"        -> toJson(fd.id.name),
          "displayName" -> toJson(decodeName(fd.id.name)),
          "line"        -> toJson(fd.getPos.line),
          "column"      -> toJson(fd.getPos.col)
        ))
      }

      event("update_overview", Map("module" -> toJson("main"), "overview" -> toJson(facts)))
    }

  }

  def saveCode(code: String, file: Option[File] = None): File = file match {
    case None =>

      val format   = DateTimeFormat.forPattern("YYYY-MM-dd_HH-mm-ss.SS")
      val dateTime = new DateTime().toString(format)
      val file     = new File(s"logs/inputs/$dateTime.scala")

      saveCode(code, Some(file))

    case Some(file) =>
      val w = new PrintWriter(file , "UTF-8")

      try {
        w.print(code)
      } finally {
        w.close
      }

      file
  }

  def notifyAnnotations(annotations: Seq[CodeAnnotation]): Unit = {
    event("editor", Map("annotations" -> toJson(annotations.map(_.toJson))))
  }

}

