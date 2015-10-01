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

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._
  
  def receive = {
    case OnUpdateCode(cstate) =>
      val result = InferInvariantsPhase.run(this.ctx)(cstate.program)
      result.conditions.map(report => report.invariant match {
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
          // TODO
        case _ =>
      })
    case DoCancel =>
    
    case OnClientEvent(cstate, event) =>
    
    case _ =>
  }
}