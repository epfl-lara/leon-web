package leon.web
package workers

import akka.actor._
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.termination._
import leon.utils._
import models._
import play.api.libs.json._
import play.api.libs.json.Json._
import leon.web.shared.{TerminationStatus => Status}

class TerminationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._

  def tgToJson(fd: FunDef, tgo: Option[TerminationGuarantee]): JsValue = tgo match {
    case Some(tg) => tg match {
      case Terminates(reason) => toJson(Map(
        "status" -> toJson(Status.terminates)
      ))
      case LoopsGivenInputs(reason, args) => toJson(Map(
        "status" -> toJson(Status.loopsfor),
        "call" -> toJson(args.mkString(fd.id+"(", ",", ")"))
      ))
      case CallsNonTerminating(funs) => toJson(Map(
        "status" -> toJson(Status.callsnonterminating),
        "calls" -> toJson(funs.map(_.id.name))
      ))
      case _ => toJson(Map(
        "status" -> toJson(Status.noguarantee)
      ))
    }
    case None => toJson(Map(
      "status" -> toJson(Status.wip)
    ))
  }

  def notifyTerminOverview(cstate: CompilationState, data: Map[FunDef, Option[TerminationGuarantee]]) {
    if (cstate.isCompiled) {
      val facts = for ((fd, tg) <- data) yield (fd.id.name -> tgToJson(fd, tg))
      event("update_overview", Map("module" -> toJson("termination"), "overview" -> toJson(facts.toMap)))
    }
  }

  def receive = {
    case OnUpdateCode(cstate) if cstate.isCompiled =>

      val reporter = new WorkerReporter(session)
      var ctx      = leon.Main.processOptions(List()).copy(interruptManager = interruptManager, reporter = reporter)

      val program = cstate.program//.duplicate

      // Notify the UI that the termination checker indeed will be launched now
      val data = (cstate.functions.map { funDef =>
         (funDef -> None)
      }).toMap
      notifyTerminOverview(cstate, data)

      logInfo("Termination checker started...")
      try {
        //val tc = new SimpleTerminationChecker(ctx, cstate.program)
        val tc = new ComplexTerminationChecker(ctx, program)

        val data = (cstate.functions.map { funDef =>
          (funDef -> Some(tc.terminates(funDef)))
        }).toMap
        logInfo("Termination checker done.")

        notifyTerminOverview(cstate, data)
      } catch {
        case t: Throwable =>
          logInfo("[!] Termination crashed!", t)

          val data = (cstate.functions.map { funDef =>
            (funDef -> Some(NoGuarantee))
          }).toMap

          notifyTerminOverview(cstate, data)
      }

    case DoCancel =>
      sender ! Cancelled(this)

    case _ =>
  }
}
