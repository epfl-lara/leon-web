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
import leon.purescala.Definitions.FunDef
import leon.web.shared.Constants
import leon.web.shared.InvariantStatus
import leon.invariant.util._
import ProgramUtil._
import PredicateUtil._
import Util._
import leon.invariant.engine._

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites with VerificationNotifier {
  import ConsoleProtocol._

  private var invariantOverview = Map[String, FunInvariantStatus]()
  private var allCode: Option[String] = None

  def receive = {
    case OnUpdateCode(cstate) =>

      for (fd <- cstate.functions) {
        val veriStatus =
          if (fd.hasTemplate || fd.getPostWoTemplate != tru) false
          else true
        invariantOverview += fd.id.name ->
          FunInvariantStatus(Some(fd), None, None, None, None, invariantFound = veriStatus)
      }
      notifyInvariantOverview(cstate)

      val startProg = cstate.program
      //val nctx = this.ctx.copy(reporter = new TestSilentReporter)
      val leonctx = createLeonContext(this.ctx, s"--timeout=20") //, s"--functions=${inFun.id.name}")
      val inferContext = new InferenceContext(startProg, leonctx)
      val result = (new InferenceEngine(inferContext)).runWithTimeout(Some(
        (ic: InferenceCondition) => {
          ic.prettyInv match {
            case Some(inv) =>
              //allCode = Some(PrettyPrinter(result.finalProgramWoInstrumentation))
              val timeExecution = ic.time
              val allCodeWhenUpdatingThisFunction =
                ScalaPrinter(InferenceReportUtil.pluginResultInInput(inferContext, List(ic), inferContext.inferProgram))
              // TODO : Update ONLY the function, not all the code.
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
                FunInvariantStatus(Some(userFun), Some(template), Some(invStr),
                  Some(allCodeWhenUpdatingThisFunction), timeExecution, true)
              notifyInvariantOverview(cstate)
            case _ => Nil
          }
        }))
      allCode = Some(ScalaPrinter(InferenceReportUtil.pluginResultInInput(inferContext, result.conditions, inferContext.inferProgram)))
      println("code: "+allCode.get)
      notifyInvariantOverview(cstate)
    // TODO: Ravi:
    // Each time an invariant is found, call:
    //   invariantOverview += funDef -> FunInvariantStatus(funDef, Some(oldInvariant), Some(newInvariant), Some(timeExecution), true)
    //   notifyInvariantOverview(cstate)
    // You can inspire yourself from the code below:
    // Note that if you want to update the verification status itself, you can use the following functions (inherited)
    //
    //   verifOverview += f -> FunVerifStatus(f, resultsWithCex)
    //   notifyVerifOverview(cstate)

    ////////////// DELETE THIS - START (when above task implemented)
    /*result.conditions.foreach(report => report.prettyInv match {
        case Some(inv) =>
          val fd = report.fd
          val funName = InstUtil.userFunctionName(fd)
          val oldInvariant = fd.template.map(k => PrettyPrinter(simplifyArithmetic(InstUtil.replaceInstruVars(Util.multToTimes(k), fd)))).getOrElse("")
          val newInvariant = PrettyPrinter(inv)
          val timeExecution = report.time
          val allCodeWhenUpdatingThisFunction = allCode // TODO : Update ONLY the function, not all the code.
          println(funName)

          invariantOverview += funName ->
            FunInvariantStatus(Some(fd), Some(oldInvariant), Some(newInvariant), allCodeWhenUpdatingThisFunction, timeExecution, true)
        case _ => Nil
      })
      // Add found=true for all invariants which were not needed.
      for((name, fis) <- invariantOverview if fis.status == InvariantStatus.undefined) {
        invariantOverview += name -> fis.copy(invariantFound = true)
      }

      notifyInvariantOverview(cstate)*/
    ////////////// DELETE THIS - END

    case DoCancel                     =>

    case OnClientEvent(cstate, event) =>

    case _                            =>
  }

  def notifyInvariantOverview(cstate: CompilationState) {
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
