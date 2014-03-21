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
import leon.synthesis.ConvertHoles
import leon.utils.TypingPhase

import leon.web.workers._

import java.util.concurrent.atomic.AtomicBoolean

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

class ConsoleSession(remoteIP: String) extends Actor with BaseActor {
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
  var cancelledWorkers = Set[WorkerActor]()
  var interruptManager: InterruptManager = _

  def receive = {
    case Init =>
      reporter = new WSReporter(channel)
      sender ! InitSuccess(enumerator)


      interruptManager = new InterruptManager(reporter)

      modules += "verification" -> ModuleContext("verification", Akka.system.actorOf(Props(new VerificationWorker(self, interruptManager))))
      modules += "termination"  -> ModuleContext("termination",  Akka.system.actorOf(Props(new TerminationWorker(self, interruptManager))))
      modules += "synthesis"    -> ModuleContext("synthesis",    Akka.system.actorOf(Props(new SynthesisWorker(self, interruptManager))))
      modules += "execution"    -> ModuleContext("execution",    Akka.system.actorOf(Props(new ExecutionWorker(self, interruptManager))))

      logInfo("New client")

      event("connected", Map())

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
              case "doCancel" =>
                self ! DoCancel

              case "doUpdateCode" =>
                self ! UpdateCode((event \ "code").as[String])

              case "storePermaLink" =>
                self ! StorePermaLink((event \ "code").as[String])

              case "accessPermaLink" =>
                self ! AccessPermaLink((event \ "link").as[String])

              case "featureSet" =>
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
      Permalink.store(code) match {
        case Some(link) =>
          event("permalink", Map("link" -> toJson(link)))
        case _ =>
          notifyError("Coult not create permalink")
      }

    case AccessPermaLink(link) =>
      Permalink.get(link) match {
        case Some(code) =>
          event("replace_code", Map("newCode" -> toJson(code)))
        case None =>
          notifyError("Link not found ?!?: "+link)
      }

    case UpdateCode(code) =>
      if (lastCompilationState.code != Some(code)) {
        clientLog("Compiling...")
        logInfo("Code to compile:\n"+code)

        saveCode(code)

        val compReporter = new CompilingWSReporter(channel)
        var compContext  = leon.Main.processOptions(List("--library")).copy(reporter = compReporter)


        val opgm = try {
          // First we extract Leon program
          val extraction = TemporaryInputPhase andThen
                           ExtractionPhase andThen
                           MethodLifting andThen
                           TypingPhase andThen
                           CompleteAbstractDefinitions andThen
                           ConvertHoles

          val pgm0 = extraction.run(compContext)((code, Nil))
          val pgm1 = ArrayTransformation(compContext, pgm0)
          val pgm2 = EpsilonElimination(compContext, pgm1)
          //val (pgm3, wasLoop) = ImperativeCodeElimination.run(compContext)(pgm2)
          val pgm4 = FunctionClosure.run(compContext)(pgm2)


          Some(pgm4)
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

            notifyAnnotations(Seq())

            modules.values.filter(_.isActive).foreach (_.actor ! OnUpdateCode(cstate))
          case None =>
            for ((l,e) <- compReporter.errors) {
              logInfo("  "+e.mkString("\n  "))
            }

            clientLog("Compilation failed!")

            event("compilation", Map("status" -> toJson("failure")))

            val annotations = compReporter.errors.map{ case (l,e) =>
              CodeAnnotation(l, 0, e.mkString("\n"), CodeAnnotationError)
            }.toSeq

            notifyAnnotations(annotations)

            lastCompilationState = CompilationState.failure(code)
        }
      } else {
        val cstate = lastCompilationState

        event("compilation", Map("status" -> toJson(cstate.compResult)))
      }

    case Quit =>


    case msg =>
      clientLog("Unknown Actor Message: "+msg)
  }

  def notifyMainOverview(cstate: CompilationState) {
    if (cstate.isCompiled) {
      val facts = for (fd <- cstate.functions) yield {
        toJson(Map(
          "name"        -> toJson(fd.id.name),
          "displayName" -> toJson(fd.orig.getOrElse(fd).id.name),
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

