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

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites {
  import ConsoleProtocol._
  
  def receive = {
    case OnUpdateCode(cstate) =>
      val result = (InstrumentationPhase andThen InferInvariantsPhase).run(this.ctx)(cstate.program)
      event("invariantSearch", Map("status" -> toJson("success")))
      val report = result.conditions.flatMap(report => report.invariant match {
        case Some(s) =>
          val inv = s
          val fd = report.fd
          import leon.invariant.structure.FunctionUtils._
          import purescala.Expressions._
          import purescala.Common._
          import purescala.ExprOps._
          
          val newpost = // TODO : Check if method from UnfoldingTemplateSolver.scala
            if (fd.postcondition.isDefined) {
              val Lambda(resultBinder, _) = fd.postcondition.get
              Lambda(resultBinder, And(fd.getPostWoTemplate, inv))
            } else {
              //replace #res in the invariant by a new result variable
              val resvar = FreshIdentifier("res", fd.returnType, true)
              // FIXME: Is this correct (ResultVariable(fd.returnType) -> resvar.toVariable))
              val ninv = replace(Map(ResultVariable(fd.returnType) -> resvar.toVariable), inv)
              Lambda(Seq(ValDef(resvar, Some(fd.returnType))), ninv)
            }
          
          val funName = InstUtil.userFunctionName(fd)
          
          val pos = fd.postcondition.map { _.getPos }.getOrElse(null)
          val startCol = if(pos != null) pos.col else -1
          val startRow = if(pos != null) pos.line else -1
          val length = if(pos != null) pos.fullString.length() else -1
          
          List(Map("name" -> toJson(funName),
              "startCol" -> toJson(startCol),
              "startRow" -> toJson(startRow),
              "length" -> toJson(length),
              "oldInvariant" -> toJson(fd.template.map(_.toString()).getOrElse("")),
              "newInvariant" -> toJson(newpost.toString())))
        case _ => Nil
      })
      
      event("invariant_result",
          Map("module" -> toJson(Module.invariant),
              "invariants" -> toJson(report)))
    case DoCancel =>
    
    case OnClientEvent(cstate, event) =>
    
    case _ =>
  }
}