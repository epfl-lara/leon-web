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
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import akka.pattern._
import play.api.Play.current
import leon.frontends.scalac._
import leon.utils.TemporaryInputPhase
import leon.utils.InterruptManager
import leon.utils.PreprocessingPhase
import leon.web.workers._
import leon.web.stores._
import leon.web.services._
import leon.web.shared.{Action, Project}
import leon.web.shared.Module
import leon.web.utils.String._
import java.io.File
import java.io.PrintWriter
import java.util.concurrent.atomic.AtomicBoolean

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import java.nio.ByteBuffer

import leon.web.websitebuilder.memory.Memory

class ConsoleSession(remoteIP: String, user: Option[User]) extends Actor with BaseActor {
  import context.dispatcher
  import ConsoleProtocol._

  val (enumerator, channel) = Concurrent.broadcast[Array[Byte]]
  var reporter: WSReporter = _

  def pushMessage(v: Array[Byte]) = channel.push(v)

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

  case class ModuleContext(name: Module, actor: ActorRef, var isActive: Boolean = false)

  var modules = Map[Module, ModuleContext]()
  var cancelledWorkers = Set[BaseActor]()
  var interruptManager: InterruptManager = _

  object ModuleEntry {
    def apply(name: Module, worker: => BaseActor, isActive: Boolean = false): (Module, ModuleContext) = {
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

  import shared.messages.{DoCancel => MDoCancel, _}
  import shared._

  def receive = {
    case Init =>
      reporter = new WSReporter(channel)
      sender ! InitSuccess(enumerator)

      interruptManager = new InterruptManager(reporter)

      modules += ModuleEntry(Verification      , new VerificationWorker(self, interruptManager))
      modules += ModuleEntry(Termination       , new TerminationWorker(self, interruptManager))
      modules += ModuleEntry(Synthesis         , new SynthesisWorker(self, interruptManager))
      modules += ModuleEntry(Disambiguation    , new DisambiguationWorker(self, interruptManager))
      modules += ModuleEntry(Execution         , new ExecutionWorker(self, interruptManager))
      modules += ModuleEntry(Repair            , new RepairWorker(self, interruptManager))
      modules += ModuleEntry(Invariant         , new OrbWorker(self, interruptManager))
      modules += ModuleEntry(WebsiteBuilder    , new WebBuilderWorker(self, interruptManager))
      modules += ModuleEntry(RepositoryHandler , new RepositoryWorker(self, currentUser), true)

      Memory.reinitialiseSourceMapsVariablesAndClarificationSession()

      logInfo("New client")

    case DoCancel =>
      cancelledWorkers = Set()
      logInfo("Starting Cancel Procedure...")
      interruptManager.interrupt()
      modules.values.foreach(_.actor ! DoCancel)
      
    case message@USetCommandFlags(flags) =>
      modules.values.foreach(_.actor ! message)

    case Cancelled(wa)  =>
      cancelledWorkers += wa

      logInfo(cancelledWorkers.size+"/"+modules.size+": Worker "+wa.getClass+" notified its cancellation")
      if (cancelledWorkers.size === modules.size) {
        logInfo("All workers got cancelled, resuming normal operations")
        interruptManager.recoverInterrupt()
      }

    case NotifyClientBin(binData) =>
      pushMessage(binData)

    case NotifyClient(event) =>
      import boopickle.Default._
      import shared.messages._
      import shared.messages.MessageFromServer._
      pushMessage(Pickle.intoBytes[MessageFromServer](event).array())

    case ProcessClientEvent(event) =>
      import boopickle.Default._
      import shared.messages.MessageToServer._

      val message = Unpickle[MessageToServer].fromBytes(ByteBuffer.wrap(event))

      try {
        logInfo("[<] " + message.getClass.getName)
        message match {
          case message: MainModule => message match {
            case MDoCancel => self ! DoCancel
            case DoUpdateCode(code, requestId) => self ! ConsoleProtocol.UpdateCode(code, None, None, requestId)
            // case DoUpdateCodeInProject(owner, repo, file, branch, code, requestId) => withUser { user =>
            //   val project = Project(owner, repo, branch, file)
            //   self ! ConsoleProtocol.UpdateCode(code, Some(user), Some(project), requestId)
            // }
            case StorePermaLink(code) => self ! message
            case AccessPermaLink(link) => self ! message
            case FeatureSet(f, active) =>
              if (modules contains f) {
                if (active) {
                  modules(f).isActive = true
                } else {
                  modules(f).isActive = false
                }
              }
            case SetCommandFlags(flags) =>
              self ! USetCommandFlags(flags)
            case UnlinkAccount(provider) => withUser { user =>
              self ! UUnlinkAccount(user, provider)
            }
          }
          case message if modules contains message.module =>
            val m = message.module
            if (modules(m).isActive) {
              modules(m).actor ! ConsoleProtocol.OnClientEvent(lastCompilationState, message)
            }
          case m => notifyError("Module "+m+" not available.")
        }
      } catch {
        case t: Throwable => notifyError("Could not process event: "+t.getMessage)
      }

    case DispatchTo(m, msg: Any) =>
      modules.get(m) match {
        case Some(m) if m.isActive =>
          m.actor ! msg
        case _ =>
      }

    case StorePermaLink(code) =>
      PermalinkStore.store(Code(code)) match {
        case Some(Permalink(Link(link), _)) =>
          event(GotPermalink(link))
        case _ =>
          notifyError("Could not create permalink")
      }

    case AccessPermaLink(link) =>
      PermalinkStore.get(Link(link)) match {
        case Some(Permalink(_, Code(value))) =>
          event(HReplaceCode(newCode=value))
        case None =>
          notifyError("Link not found ?!?: "+link)
      }

    case UUnlinkAccount(user, provider) =>
      clientLog(s"Unlinking account '${provider.id}'...")

      user.identity(provider) match {
        case Some(id) =>
          import play.api.db._

          implicit val c = DB.getConnection()
          val newUser = UserStore.unlinkIdentity(user, id)
          currentUser = Some(newUser)

          clientLog("=> DONE")

          event(UserUpdated(user = newUser.toShared))

          self ! DispatchTo(RepositoryHandler, UUserUpdated(currentUser))
          
        case None =>
          clientLog("=> ERROR: No such account found.")
      }

    case UpdateCode(code, user, project, requestId) =>
      Memory.clearClarificationSession()

      if (lastCompilationState.project =!= project ||
          lastCompilationState.code =!= Some(code)) {

        clientLog("Compiling...")
        logInfo(s"Code updated:\n$code")

        val savedFile = project match {
          case None =>
            saveCode(code)

          case Some(p) =>
            val path = {
              val wc = GitService.getWorkingCopy(user.get, p.repo.desc)

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
              val wc = GitService.getWorkingCopy(user, repo.desc)

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
              requestId  = Some(requestId),
              wasLoop    = Set(),
              project    = project,
              savedFile  = Some(savedFile.getName())
            )

            lastCompilationState = cstate

            event(HCompilation("success"))

            clientLog("Compilation successful!")

            notifyMainOverview(cstate)

            lazy val isOnlyInvariantActivated = modules.values.forall(m =>
                ( m.isActive && m.name === shared.Invariant) ||
                (!m.isActive && m.name =!= shared.Invariant))

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
              modules(Invariant).actor ! OnUpdateCode(cstate)
              val termmod = modules(Termination)
              if(termmod.isActive)
                termmod.actor ! OnUpdateCode(cstate)
            } else {
              modules.values.filter(e => e.isActive && e.name =!= Invariant).foreach (_.actor ! OnUpdateCode(cstate))
            }

          case None =>
            for ((l,e) <- compReporter.errors) {
              logInfo(s"  ${e mkString "\n  "}")
            }

            clientLog("Compilation failed!")
            event(HCompilation("failure"))

            lastCompilationState = CompilationState.failure(
              code, project, Some(savedFile.getName())
            )
        }

        val annotations = {
          compReporter.errors.map{ case (l,e) =>
            shared.CodeAnnotation(l, 0, e.mkString("\n"), shared.CodeAnnotationError)
          }.toSeq ++
          compReporter.warnings.map{ case (l,e) =>
            shared.CodeAnnotation(l, 0, e.mkString("\n"), shared.CodeAnnotationWarning)
          }.toSeq
        }.filter(_.line >= 0)

        notifyAnnotations(annotations)
      }
      else {
        val cstate = lastCompilationState.copy(requestId = Some(requestId))
        self ! DispatchTo(WebsiteBuilder, OnUpdateCode(cstate))
        event(HCompilation(cstate.compResult))
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
      val facts: Map[String, OverviewFunction] = (for (fd <- cstate.functions) yield {
        fd.id.name -> OverviewFunction(fd.id.name, decodeName(fd.id.name), fd.getPos.line, fd.getPos.col)
      }).toMap

      event(HUpdateOverview(facts))
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

  def notifyAnnotations(annotations: Seq[shared.CodeAnnotation]): Unit = {
    event(HEditor(annotations = Some(annotations.toArray)))
  }

}
