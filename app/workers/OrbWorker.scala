package leon
package web
package workers

import models.ConsoleProtocol
import leon.utils.InterruptManager
import models.ConsoleProtocol
import akka.actor.ActorRef
import models.ConsoleProtocol
import leon.invariant.engine.InferInvariantsPhase
import leon.invariant.util.ResultVariable
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
import purescala.PrettyPrinter
import leon.verification.VCStatus
import leon.verification.VCResult
import models.CompilationState
import models.FunInvariantStatus
import leon.purescala.Definitions.FunDef
import leon.web.shared.Constants
import leon.web.shared.InvariantStatus
import leon.invariant.util.Util

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites with VerificationNotifier {
  import ConsoleProtocol._

  private var invariantOverview  = Map[String, FunInvariantStatus]()
  private var allCode: Option[String] = None
  
  def receive = {
    case OnUpdateCode(cstate) =>
      // TODO: Ravi:
      // Initialize invariantOverview to something here.
      
      for(fd <- cstate.functions) {
        invariantOverview += fd.id.name -> FunInvariantStatus(Some(fd), None, None, None, None)
      }
      notifyInvariantOverview(cstate)
      
      val (_, result) = InferInvariantsPhase.run(this.ctx, cstate.program)
      allCode = Some(PrettyPrinter(result.finalProgramWoInstrumentation))

      /*val reports = result.conditions.flatMap(report => report.prettyInv match {
        case Some(s) =>
          val inv = s
          val fd = report.fd
          val funName = InstUtil.userFunctionName(fd)

          List(Map("name" -> toJson(funName),
            "startCol" -> toJson(0),
            "startRow" -> toJson(0),
            "length" -> toJson(0),
            "oldInvariant" -> toJson(fd.template.map(_.toString()).getOrElse("")),
            "newInvariant" -> toJson(PrettyPrinter(inv))))
        case _ => Nil
      })*/

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
      result.conditions.foreach(report => report.prettyInv match {
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
      
      notifyInvariantOverview(cstate)
      ////////////// DELETE THIS - END
      
      
      
    case DoCancel =>

    case OnClientEvent(cstate, event) =>

    case _ =>
  }
  
  def notifyInvariantOverview(cstate: CompilationState) {
    val fics = Json.obj((
      (invariantOverview.toSeq.filter(_._2.fd.nonEmpty).sortBy(_._2.fd.map(_.getPos).get).map{ case (fd, fi) =>
        fd -> (fi: JsValueWrapper)
      }) ++ allCode.map((code: String) =>
        Constants.invariantMainCode ->
        (FunInvariantStatus(None, None, None, Some(code), None): JsValueWrapper))) : _*
    )
 
    event("update_overview",
      Map("module" -> toJson(Module.invariant),
          "overview" -> fics 
        )
    )
  }
}
