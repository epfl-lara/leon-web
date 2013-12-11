package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.purescala.Trees._
import leon.purescala.Definitions.FunDef

class ExecutionWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._
  import leon.codegen._
  import leon.evaluators._

  val reporter = new WorkerReporter(session)
  var ctx      = leon.Main.processOptions(Nil).copy(interruptManager = interruptManager, reporter = reporter)

  def doExplore(cstate: CompilationState, manualTargets: Map[FunDef, Seq[Expr]] ) = {
    var allFacts = List[JsValue]()

    val autoTargets = cstate.program.definedFunctions.collect {
      case fd if fd.args.isEmpty => (fd, Nil) 
    }.toMap

    val explorationTargets = autoTargets ++ manualTargets

    if (cstate.isCompiled) {
      for ((fd, args) <- explorationTargets) {
        val tracingEval = new TracingEvaluator(ctx, cstate.program)

        val positionedArgs = (args zip fd.args).map { case (a, ad) => a.setPos(ad) }.toList

        tracingEval.eval(FunctionInvocation(fd, positionedArgs).setPos(fd))
      
        tracingEval.lastGlobalContext match {
          case Some(gc) =>
            for ((v, res) <- gc.values) {
              v.getPos match {
                case RangePosition(fromLine, fromCol, _, toLine, toCol, _, _) =>
                  val sentRes = res match {
                    case Error(msg) =>
                      "Error: "+msg
                    case e => e.toString
                  }
                  allFacts ::= toJson(Map(
                    "fromRow"     -> toJson(fromLine-1),
                    "fromColumn"  -> toJson(fromCol-1),
                    "toRow"       -> toJson(toLine-1),
                    "toColumn"    -> toJson(toCol-1),
                    "result"      -> toJson(sentRes)
                  ))
                case NoPosition =>
                  println("Expr: "+v+" ("+v.getClass+") has no position")
                case _ =>
              }
            }
          case _ =>
        }
      }
    }

    event("update_exploration_facts", Map("newFacts" -> toJson(allFacts)))
  }

  def receive = {
    case OnUpdateCode(cstate) =>
      doExplore(cstate, Map())

    case DoCancel =>
      sender ! Cancelled(this)

    case NewCounterExamples(cstate, ceMap) =>
      doExplore(cstate, ceMap)

    case _ =>
  }
}
