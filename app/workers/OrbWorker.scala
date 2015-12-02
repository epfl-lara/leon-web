package leon
package web
package workers

import models.ConsoleProtocol
import leon.utils.InterruptManager
import models.ConsoleProtocol
import akka.actor.ActorRef
import models.ConsoleProtocol
import leon.invariant.engine._
import leon.invariant.util._
import leon.purescala.Definitions.ValDef
import leon.transformations.InstUtil
import leon.transformations.InstrumentationPhase
import shared.Module
import models.JsonWrites
import models.InvariantPosition
import play.api.libs.json._
import play.api.libs.json.Json._
import leon.invariant.structure.FunctionUtils._
import purescala.Expressions._
import purescala.Common._
import purescala.ExprOps._
import purescala._
import leon.verification.VCStatus
import leon.verification.VCResult
import models.CompilationState
import models.FunInvariantStatus
import leon.web.shared.Constants
import leon.web.shared.InvariantStatus
import leon.invariant.util._
import ProgramUtil._
import Util._
import leon.invariant.engine._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites with VerificationNotifier {
  import ConsoleProtocol._

  private var invariantOverview = Map[String, FunInvariantStatus]()
  private var allCode: Option[String] = None

  private var inferEngine: Option[InferenceEngine] = None

  def receive = {
    case DoCancel =>
      inferEngine.foreach(_.interrupt())
      inferEngine = None
      sender ! Cancelled(this)

    case OnUpdateCode(cstate) =>

      for (fd <- cstate.functions) {
        val veriStatus =
          if (fd.hasTemplate || fd.getPostWoTemplate =!= tru) false
          else true
        invariantOverview += fd.id.name ->
          FunInvariantStatus(Some(fd), None, None, None, None, invariantFound = veriStatus)
      }
      allCode = None
      notifyInvariantOverview(cstate)

      val startProg = cstate.program
      //val nctx = this.ctx.copy(reporter = new TestSilentReporter)
      val leonctx = createLeonContext(this.ctx, s"--timeout=120", s"--minbounds", "--vcTimeout=7") //, s"--functions=${inFun.id.name}")

      inferEngine match {
        case Some(engine) =>
          engine.interrupt()
          // reinitialize the interrupt manager
          leonctx.interruptManager.recoverInterrupt()
        case None =>
      }

      val inferContext = new InferenceContext(startProg, leonctx)
      val engine = (new InferenceEngine(inferContext))
      inferEngine = Some(engine)

      Future {
        engine.runWithTimeout(Some(
          (ic: InferenceCondition) => {
            ic.prettyInv match {
              case Some(inv) =>
                val timeExecution = ic.time
                val funName = InstUtil.userFunctionName(ic.fd)
                val userFun = functionByName(funName, startProg).get
                // replace '?' in the template
                val template = userFun.template.map { k =>
                  PrettyPrinter(simplePostTransform {
                    case Variable(id) if id.name.startsWith("q?") =>
                      Variable(FreshIdentifier("?", id.getType))
                    case e => e
                  }(k))
                }.getOrElse("")
                val invStr = PrettyPrinter(inv)
                invariantOverview += funName ->
                  FunInvariantStatus(Some(userFun), Some(template), Some(invStr), None, timeExecution, true)
                notifyInvariantOverview(cstate)
              case _ => Nil
            }
          }))
      } onComplete {
        case Success(result: InferenceReport) =>
          inferEngine = None
          // TODO : Update ONLY the function, not all the code.
          allCode = Some(ScalaPrinter(InferenceReportUtil.pushResultsToInput(inferContext, result.conditions)))
          notifyInvariantOverview(cstate)
        case Failure(msg) =>
          inferEngine = None
          clientLog("Failed for no reason except: " + msg)
      }

    case OnClientEvent(cstate, event) =>

    case _                            =>
  }

  def notifyInvariantOverview(cstate: CompilationState): Unit = {
    val fics = Json.obj((
      (invariantOverview.toSeq.filter(_._2.fd.nonEmpty).sortBy(_._2.fd.map(_.getPos).get).map {
        case (fd, fi) =>
          fd -> (fi: JsValueWrapper)
      }) ++ allCode.map((code: String) =>
        Constants.invariantMainCode ->
          (FunInvariantStatus(None, None, None, Some(code), None): JsValueWrapper))): _*)

    event("update_overview",
      Map("module" -> toJson(Module.invariant),
        "overview" -> fics))
  }

  /**
   * Used only for debugging
   */
  class TestSilentReporter extends DefaultReporter(Set()) {
    var lastErrors: List[String] = Nil

    override def emit(msg: Message): Unit = msg match {
      case Message(this.ERROR, _, msg) => lastErrors ++= List(msg.toString)
      case Message(this.FATAL, _, msg) => lastErrors ++= List(msg.toString)
      case _                           =>
    }
  }
}
