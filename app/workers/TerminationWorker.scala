package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.termination._
import leon.purescala.Common._
import leon.purescala.Definitions._

class TerminationWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._

  def notifyTerminOverview(cstate: CompilationState, data: Map[FunDef, TerminationGuarantee]) {
    if (cstate.isCompiled) {
      val facts = for ((fd, tg) <- data) yield {
        val t = toJson(Map(
          "status" -> toJson(if (tg.isGuaranteed) "terminates" else "noguarantee")
        ))

        fd.id.name -> t
      }

      event("update_overview", Map("module" -> toJson("termination"), "overview" -> toJson(facts.toMap)))
    }

  }

  def receive = {
    case OnUpdateCode(cstate) if cstate.isCompiled =>

      val reporter = new WorkerReporter(session)
      var ctx      = leon.Main.processOptions(List()).copy(interruptManager = interruptManager, reporter = reporter)

      val program = cstate.program//.duplicate

      try {
        val tc = new SimpleTerminationChecker(ctx, cstate.program)
        //val tc = new ComplexTerminationChecker(ctx, program)
        //tc.initialize()

        val data = (program.definedFunctions.toList.sortWith(_.getPos < _.getPos).map { funDef =>
          (funDef -> tc.terminates(funDef))
        }).toMap

        notifyTerminOverview(cstate, data)
      } catch {
        case t: Throwable =>
          logInfo("[!] Termination crashed!", t)

          val data = (program.definedFunctions.toList.sortWith(_.getPos < _.getPos).map { funDef =>
            (funDef -> NoGuarantee)
          }).toMap

          notifyTerminOverview(cstate, data)
      }

    case DoCancel =>
      sender ! Cancelled(this)

    case _ =>
  }
}
