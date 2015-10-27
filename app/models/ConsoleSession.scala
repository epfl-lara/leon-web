package leon.web

package models

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.Future

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import akka.pattern.ask

import play.api.Play.current

import leon.frontends.scalac._
import leon.xlang._
import leon.utils.TemporaryInputPhase
import leon.utils.InterruptManager
import leon.purescala._
import leon.utils.PreprocessingPhase

import leon.web.workers._
import leon.web.stores.PermalinkStore
import leon.web.services.github._
import leon.web.json.GitHub._
import leon.web.shared.{Action, Module}

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

  def withUser(f: User => Unit): Unit = user match {
    case Some(user) =>
      f(user)
    case None       =>
      notifyError("Cannot perform this operation when user is not logged-in.")
      logInfo("Cannot perform this operation when user is not logged-in.")
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

      modules += ModuleEntry(Module.verification, new VerificationWorker(self, interruptManager))
      modules += ModuleEntry(Module.termination , new TerminationWorker(self, interruptManager))
      modules += ModuleEntry(Module.synthesis   , new SynthesisWorker(self, interruptManager))
      modules += ModuleEntry(Module.execution   , new ExecutionWorker(self, interruptManager))
      modules += ModuleEntry(Module.repair      , new RepairWorker(self, interruptManager))
      modules += ModuleEntry(Module.invariant   , new OrbWorker(self, interruptManager))

      logInfo("New client")

    case DoCancel =>
      cancelledWorkers = Set()
      logInfo("Starting Cancel Procedure...")
      interruptManager.interrupt()
      modules.values.foreach(_.actor ! DoCancel)

    case Cancelled(wa: WorkerActor)  =>
      cancelledWorkers += wa

      logInfo(cancelledWorkers.size+"/"+modules.size+": Worker "+wa.getClass+" notified its cancellation")
      if (cancelledWorkers.size == modules.size) {
        logInfo("All workers got cancelled, resuming normal operations")
        interruptManager.recoverInterrupt()
      }

    case NotifyClient(event) =>
      pushMessage(event)

    case ProcessClientEvent(event) =>
      try {
        logInfo("[<] "+(event \ "action").as[String])

        (event \ "module").as[String] match {
          case "main" =>
            (event \ "action").as[String] match {
              case Action.doCancel =>
                self ! DoCancel

              case Action.doUpdateCode =>
                self ! UpdateCode((event \ "code").as[String])

              case Action.storePermaLink =>
                self ! StorePermaLink((event \ "code").as[String])

              case Action.accessPermaLink =>
                self ! AccessPermaLink((event \ "link").as[String])

              case Action.loadRepositories => withUser { user =>
                self ! LoadRepositories(user)
              }

              case Action.loadRepository => withUser { user =>
                val owner = (event \ "owner").as[String]
                val name  = (event \ "name").as[String]

                self ! LoadRepository(user, owner, name)
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

    case LoadRepositories(user) =>
      // TODO: Handle case when the token is missing
      val token = user.oAuth2Info.get.accessToken
      val gh    = GitHubService(token)
      val res   = Await.result(gh.listUserRepositories(), 2.seconds)

      res match {
        case Left(error) =>
          event("repositories", Map(
            "status" -> toJson("failed"),
            "error"  -> toJson(error.message),
            "repos"  -> toJson(Seq[String]())
          ))

        case Right(repos) =>
          event("repositories", Map(
            "status" -> toJson("success"),
            "error"  -> toJson(""),
            "repos"  -> toJson(repos)
          ))
      }

    case LoadRepository(user, owner, name) =>
      // TODO: Handle case when the token is missing
      val token  = user.oAuth2Info.get.accessToken
      val gh     = GitHubService(token)
      val result = Await.result(gh.getRepository(owner, name), 1.seconds)

      result match {
        case Left(error) =>
          event("load_repository", Map(
            "status" -> toJson("failed"),
            "error"  -> toJson(error.message),
            "files"  -> toJson(Seq[String]())
          ))

        case Right(repo) =>
          val (owner, name) = (repo.owner, repo.name)

          val wc = new RepositoryInfos(s"${user.fullId}/$owner/$name")

          if (!wc.exists) {
            clientLog(s"Cloning repository $owner/$name...")
            wc.cloneRepo(repo.cloneURL, Some(token))
            clientLog(s"Cloning repository $owner/$name... Done.")
          }

          clientLog(s"Listing files in $owner/$name...")
          val files = wc.getFiles(repo.defaultBranch)
                        .getOrElse(Seq[String]())
                        .filter(f => f.substring(f.lastIndexOf(".") + 1) == "scala")

          clientLog(s"Listing files in $owner/$name... Done.")

          event("load_repository", Map(
            "status" -> toJson("success"),
            "error"  -> toJson(""),
            "files"  -> toJson(files)
          ))
      }

    case UpdateCode(code) =>
      if (lastCompilationState.code != Some(code)) {
        clientLog("Compiling...")
        logInfo("Code to compile:\n"+code)

        saveCode(code)

        val compReporter = new CompilingWSReporter(channel)
        var compContext  = leon.Main.processOptions(Nil).copy(reporter = compReporter)

        val opgm = try {
          // First we extract Leon program
          val pipeline = TemporaryInputPhase andThen
                         ExtractionPhase andThen
                         (new PreprocessingPhase(false))
                         // InstrumentationPhase andThen InferInvariantsPhase

          val (_, pgm) = pipeline.run(compContext, (List(code), Nil))

          compReporter.terminateIfError


          Some(pgm)
        } catch {
          case t: Throwable =>
            logInfo("Failed to compile and/or extract")
            None
        }

        opgm match {
          case Some(program) =>

            val cstate = CompilationState(
              optProgram = Some(program),
              code = Some(code),
              compResult = "success",
              wasLoop = Set()
            )

            lastCompilationState = cstate

            event("compilation", Map("status" -> toJson("success")))

            clientLog("Compilation successful!")

            notifyMainOverview(cstate)

            lazy val isOnlyInvariantActivated = modules.values.forall(m =>
                ( m.isActive && m.name == Module.invariant) ||
                (!m.isActive && m.name != Module.invariant))

            lazy val postConditionHasQMark =
              program.definedFunctions.exists { funDef =>
                funDef.postcondition match {
                  case Some(postCondition) =>
                  import leon.purescala._
                  import Expressions._
                  ExprOps.exists {
                    case FunctionInvocation(callee, _) =>
                      leon.purescala.DefOps.fullName(callee.fd)(program) == "leon.invariant.?"
                    case _ =>
                      false
                  }(postCondition)
                  case None => false
                }
              }

            if (isOnlyInvariantActivated || postConditionHasQMark) {
              modules(Module.invariant).actor ! OnUpdateCode(cstate)
            } else {
              modules.values.filter(e => e.isActive && e.name != Module.invariant).foreach (_.actor ! OnUpdateCode(cstate))
            }
          case None =>
            for ((l,e) <- compReporter.errors) {
              logInfo("  "+e.mkString("\n  "))
            }

            clientLog("Compilation failed!")

            event("compilation", Map("status" -> toJson("failure")))

            lastCompilationState = CompilationState.failure(code)
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
      } else {
        val cstate = lastCompilationState

        event("compilation", Map("status" -> toJson(cstate.compResult)))
      }

    case Quit =>


    case msg =>
      clientLog("Unknown Actor Message: "+msg)
  }

  def notifyMainOverview(cstate: CompilationState) {
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

  def saveCode(code: String) {
    import java.io.{File,PrintWriter}
    val d = new DateTime().toString(DateTimeFormat.forPattern("YYYY-MM-dd_HH-mm-ss.SS"))

    val filePath = new File(s"logs/inputs/$d.scala");
    val w = new PrintWriter( filePath , "UTF-8")
    try {
      w.print(code)
    } finally {
      w.close
    }
  }

  def notifyAnnotations(annotations: Seq[CodeAnnotation]) {
    event("editor", Map("annotations" -> toJson(annotations.map(_.toJson))))
  }

}

