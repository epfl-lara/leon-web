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
import leon.web.stores._
import leon.web.services._

import leon.web.shared._
import leon.web.utils.String._

import java.io.File
import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicBoolean

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class ConsoleSession(remoteIP: String, user: Option[User]) extends Actor with BaseActor {
  import context.dispatcher
  import ConsoleProtocol._

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

  case class ModuleContext(name: String, actor: ActorRef, var isActive: Boolean = false)

  var modules = Map[String, ModuleContext]()
  var cancelledWorkers = Set[BaseActor]()
  var interruptManager: InterruptManager = _

  object ModuleEntry {
    def apply(name: String, worker: => BaseActor, isActive: Boolean = false): (String, ModuleContext) = {
      name -> ModuleContext(name, context.actorOf(Props(worker)), isActive)
    }
  }

  private var currentUser: Option[User] = user

  def withUser(f: User => Unit): Unit = currentUser match {
    case Some(user) =>
      f(user)

    case None =>
      notifyError("You need to log-in to perform this operation.")
      logInfo("Cannot perform this operation when user is not logged-in.")
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
      modules += ModuleEntry(Module.repository    , new RepositoryWorker(self, currentUser), true)

      logInfo("New client")

    case DoCancel =>
      cancelledWorkers = Set()
      logInfo("Starting Cancel Procedure...")
      interruptManager.interrupt()
      modules.values.foreach(_.actor ! DoCancel)

    case Cancelled(wa)  =>
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

              case Action.storePermaLink =>
                self ! StorePermaLink((event \ "code").as[String])

              case Action.accessPermaLink =>
                self ! AccessPermaLink((event \ "link").as[String])

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

          self ! DispatchTo(Module.repository, UserUpdated(currentUser))

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
              val wc = GitService.getWorkingCopy(user.get, p.repo)

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

