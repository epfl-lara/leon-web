package leon.web
package workers

import akka.actor._
import leon.purescala.Definitions._
import leon.termination._
import leon.utils._
import models._
import leon.web.shared.{TerminationStatus => Status}

class TerminationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._

  import shared.messages.{DoCancel => _, _ }
  def tgToTerminationDetails(fd: FunDef, tgo: Option[TerminationGuarantee]): TerminationDetails = tgo match {
    case Some(tg) => tg match {
      case Terminates(reason) => TerminationDetails(
        status = Status.terminates,
        reason = Some(reason)
      )
      case LoopsGivenInputs(reason, args) => TerminationDetails(
        status = Status.loopsfor,
        call = args.mkString(fd.id+"(", ",", ")")
      )
      case CallsNonTerminating(funs) => TerminationDetails(
        status = Status.callsnonterminating,
        calls = funs.map(_.id.name)
      )
      case _ => TerminationDetails(
        status = Status.noguarantee
      )
    }
    case None => TerminationDetails(
      status = Status.wip
    )
  }

  def notifyTerminOverview(cstate: CompilationState, data: Map[FunDef, Option[TerminationGuarantee]]): Unit = {
    if (cstate.isCompiled) {
      val facts = for ((fd, tg) <- data) yield (fd.id.name -> tgToTerminationDetails(fd, tg))
      event(HUpdateTerminationOverview(overview = facts.toMap))
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

        val data = (tc.functions.map { funDef =>
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
