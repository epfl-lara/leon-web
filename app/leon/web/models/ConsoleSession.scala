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
import play.api.libs.iteratee.{ Execution => _, _ }
import play.api.libs.concurrent.{ Execution => _, _ }
import akka.pattern._
import play.api.Play.current
import leon.frontends.scalac._
import leon.utils.TemporaryInputPhase
import leon.utils.InterruptManager
import leon.utils.PreprocessingPhase
import leon.web.workers._
import leon.web.stores._
import leon.web.services._
import leon.web.shared.{ User => _, _ }
import leon.web.shared.messages.{ DoCancel => MDoCancel, _ }
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

  case class ModuleContext(name: Module, actor: ActorRef, var isActive: Boolean = false)

  var modules = Map[Module, ModuleContext]()
  var cancelledWorkers = Set[BaseActor]()
  var interruptManager: InterruptManager = _

  def leonModules: Map[Module, ModuleContext] = modules.filter { case (m, _) =>
    m =!= Compilation && m =!= RepositoryHandler
  }

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

  def receive = {
    case Init =>
      reporter = new WSReporter(channel)
      sender ! InitSuccess(enumerator)

      interruptManager = new InterruptManager(reporter)

      modules += ModuleEntry(Compilation       , new CompilationWorker(self, interruptManager, channel), true)
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

    case message @ USetCommandFlags(flags) =>
      leonModules.values.foreach(_.actor ! message)

    case DoCancel =>
      cancelledWorkers = Set()
      logInfo("Starting Cancel Procedure...")
      interruptManager.interrupt()
      leonModules.values.foreach(_.actor ! DoCancel)

    case Cancelled(wa)  =>
      cancelledWorkers += wa

      logInfo(cancelledWorkers.size + "/" + leonModules.size + ": Worker " + wa.getClass + " notified its cancellation")
      if (cancelledWorkers.size === leonModules.size) {
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

            case MDoCancel =>
              self ! DoCancel

            case StorePermaLink(code) =>
              self ! message

            case AccessPermaLink(link) =>
              self ! message

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

            case DoUpdateCode(code, requestId) =>
              self ! UpdateCode(code, None, None, requestId)

          }

          case message if modules.get(message.module).exists(_.isActive) =>
            val module = modules(message.module)
            module.actor ! OnClientEvent(lastCompilationState, message)

          case message =>
            notifyError("Module "+ message.module +" not available.")
        }
      }
      catch {
        case t: Throwable =>
          notifyError("Could not process event: " + t.getMessage)
      }

    case DispatchTo(module, msg: Any) =>
      modules.get(module).filter(_.isActive) foreach { module =>
        module.actor ! msg
      }

    case UpdateCode(code, user, repoState, reqId) => {
      val isOnlyInvariantActive =
        leonModules.values.forall(m =>
          ( m.isActive && m.name === shared.Invariant) ||
          (!m.isActive && m.name =!= shared.Invariant))

      self ! DispatchTo(Compilation, Compile(
        lastCompilationState,
        code,
        user,
        repoState,
        reqId,
        isOnlyInvariantActive
      ))
    }

    case CompilationDone(cstate, None, _) =>
      lastCompilationState = cstate

    case CompilationDone(cstate, Some(program), notifyInvariant) =>
      lastCompilationState = cstate

      self ! DispatchTo(Termination, OnUpdateCode(cstate))

      if (notifyInvariant) {
        self ! DispatchTo(Invariant, OnUpdateCode(cstate))
      }
      else {
        val toUpdate = modules.values.filter { e =>
          e.isActive && e.name =!= Invariant
        }

        toUpdate.foreach (_.actor ! OnUpdateCode(cstate))
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

    case Quit =>

    case msg =>
      clientLog("ConsoleSession received an unknown message: " + msg)
  }

}
