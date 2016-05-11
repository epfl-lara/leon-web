package leon.web
package workers

import akka.actor._

import models._
import leon.LeonContext
import leon.utils._
import leon.purescala.PrinterOptions
import leon.purescala.PrinterContext
import leon.purescala.ScalaPrinter
import leon.synthesis._
import leon.repair._
import leon.web.shared.Action

class RepairWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._
  
  def receive = {
    case DoCancel =>
      sender ! Cancelled(this)

    case OnClientEvent(cstate, event) =>
      event match {
        case shared.messages.DoRepair(fname) =>
          doRepair(cstate, fname)
        case _ => notifyError("Received unknown event: "+event)
      }
    case _ =>
  }

  def doRepair(cstate: CompilationState, fname: String): Unit = {
    try {
      val program = cstate.program
      import shared.messages._
      def progress(name: String): Unit = {
        event(HRepairResult(result = "progress", progress = name))
      }

      def error(err: String): Unit = {
        event(HRepairResult(result = "error", progress = err))
      }

      event(HRepairResult(result = "init"))

      progress("Initializing ...")
      val timeoutMs = Some(30000l)
      val to = new TimeoutFor(ctx.interruptManager)

      program.definedFunctions.find( _.id.name === fname) match {
        case Some(fd) =>
          val rep = new Repairman(ctx, program, fd, timeoutMs, timeoutMs)

          to.interruptAfter(timeoutMs) {
            progress("Discovering tests...")
            val tb = rep.discoverTests()

            if (tb.invalids.nonEmpty) {
                progress("Minimizing "+tb.invalids.size+" tests...")
                val tb2 = tb.minimizeInvalids(fd, ctx, program)

                val synth = rep.getSynthesizer(tb2)

                try {
                  progress("Synthesizing repair ...")
                  val (search0, sols0) = synth.synthesize()

                  progress("Verifying repair ...")
                  val (search, solutions) = synth.validate((search0, sols0), allowPartial = false)

                  if (solutions.isEmpty) {
                    error("Failed to repair :(")
                  } else {
                    val (sol, isTrusted) = solutions.last
                    val expr = sol.toSimplifiedExpr(ctx, program, fd)

                    val fdDup = fd.duplicate()
                    fdDup.body = Some(expr)

                    val p = new ScalaPrinter(PrinterOptions(), Some(program))

                    val allCode = try {
                      val fInt = new FileInterface(new MuteReporter())

                      fInt.substitute(
                        cstate.code.getOrElse(""),
                        fd,
                        fdDup
                      )(ctx)
                    } catch {
                      case t: Throwable =>
                        notifyError("Could not substitute repair solution back in code")
                        cstate.code.getOrElse("")
                    }

                    event(HRepairResult(
                      result = "success",
                      success = "Repair Found!",
                      solCode = ScalaPrinter(expr),
                      cursor = Some(HMoveCursor(
                        line = fd.getPos.line,
                        column = (fd.getPos.col-1)
                      )),
                      allCode = allCode
                    ))
                  }
                } finally {
                  synth.shutdown()
                }
            }
          }
        case None =>
          event(HRepairResult(result = "error", error = "Failed to find function"))
      }
    } catch {
      case t: Throwable =>
        notifyError("Woops, repair crashed: "+t.getMessage)
        logInfo("Repair crashed", t)
    }
  }
}
