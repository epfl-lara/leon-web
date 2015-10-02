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

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites {
  import ConsoleProtocol._

  def receive = {
    case OnUpdateCode(cstate) =>
      val result = InferInvariantsPhase.run(this.ctx)(cstate.program)
      event("invariantSearch", Map("status" -> toJson("success")))
      val reports = result.conditions.flatMap(report => report.invariant match {
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
      })

      event("invariant_result",
        Map("module" -> toJson(Module.invariant),
          "invariants" -> toJson(reports),
          "code" -> toJson(PrettyPrinter(result.finalProgramWoInstrumentation))))
    case DoCancel =>

    case OnClientEvent(cstate, event) =>

    case _ =>
  }
}