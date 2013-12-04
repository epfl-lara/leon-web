package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.purescala.Trees.FunctionInvocation

class ExecutionWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._
  import leon.codegen._
  import leon.evaluators._

  val reporter = new WorkerReporter(session)
  var ctx      = leon.Main.processOptions(Nil).copy(interruptManager = interruptManager, reporter = reporter)

  val params = CodeGenParams(maxFunctionInvocations = 5000, checkContracts = true)

  def notifyExecutionOverview(cstate: CompilationState) {
    if (cstate.isCompiled) {
      val facts = for (fd <- cstate.program.definedFunctions if fd.args.isEmpty) yield {
        fd.id.name -> toJson(Map("line" -> fd.getPos.line))
      }

      event("update_execution_overview", Map("functions" -> toJson(facts.toMap)))
    }

  }

  def receive = {
    case OnUpdateCode(cstate) =>
      notifyExecutionOverview(cstate)

    case DoCancel =>
      sender ! Cancelled(this)

    case OnClientEvent(cstate, event) =>
      (event \ "action").as[String] match {
        case "doExecute" =>
          val fname = (event \ "fname").as[String]

          val evaluator = new CodeGenEvaluator(ctx, cstate.program, params)

          cstate.program.definedFunctions.find(_.id.name == fname) match {
            case Some(fd) =>
              evaluator.eval(FunctionInvocation(fd, List())) match {
                case EvaluationResults.Successful(v) =>
                  notifySuccess(v.toString)

                case EvaluationResults.RuntimeError(msg) =>
                  notifyError("Evaluation failed: "+msg +"")

                case EvaluationResults.EvaluatorError(msg) =>
                  notifyError("Evaluation failed: "+msg +"")
              }

            case _ =>
              notifyError("Function "+fname +" not found :(")
          }

      }

    case _ =>
  }
}
