package leon.web
package workers

import akka.actor._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.purescala.Expressions._
import leon.purescala.Definitions.{TypedFunDef, Program}

class ExecutionWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites {
  import ConsoleProtocol._
  import leon.evaluators._

  protected var program: Option[Program] = None
  def receive = {
    case OnUpdateCode(cstate) =>
      program = Some(cstate.program)
      val groundFunctions = cstate.functions.collect {
        case fd if fd.params.isEmpty => fd.typed -> Seq[Expr]()
      }.toMap

      doEval(cstate, groundFunctions)

    case DoCancel =>
      sender ! Cancelled(this)

    case NewCounterExamples(cstate, ceMap) =>
      doEval(cstate, ceMap)

    case _ =>
  }

  def doEval(cstate: CompilationState, targets: Map[TypedFunDef, Seq[Expr]]) = {
    if (cstate.isCompiled) {
      for ((tfd, args) <- targets.toSeq.sortBy(_._1.getPos)) {
        val tracingEval = new TracingEvaluator(ctx, cstate.program, 10000)

        val positionedArgs = (args zip tfd.params).map { case (a, ad) => a.setPos(ad) }.toList

        try {
          tracingEval.eval(FunctionInvocation(tfd, positionedArgs).setPos(tfd))
        } catch {
          case e: StackOverflowError =>
            notifyError("Stack Overflow when exploring expression!")
          case e: Throwable =>
            notifyError("Unexpected error while evaluating: "+e.getClass+" "+e.getMessage)
        }

        tracingEval.lastGC match {
          case Some(gc) =>
            val facts = for((v, res) <- gc.values.toSeq) yield {
              v.getPos match {
                case rp: RangePosition =>
                  res match {
                    case Error(tpe, msg) =>
                      val err = "Error: "+msg
                      if (rp == tfd.getPos) {
                        // We don't want to report the global "Postcondition error" for the entire function
                        None
                      } else {
                        Some(rp -> err)
                      }
                    case e =>
                      Some(rp -> e.asString)
                  }

                case _ =>
                  None
              }
            }

            event("update_exploration_facts", Map("newFacts" -> toJson(facts.flatten)))

          case _ =>
        }
      }
    }
  }
}
