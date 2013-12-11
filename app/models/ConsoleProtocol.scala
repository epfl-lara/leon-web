package leon.web
package models

import workers.WorkerActor

import play.api.libs.json._
import play.api.libs.iteratee._
import leon.synthesis.Solution
import leon.purescala.Definitions._
import leon.purescala.Trees._

object ConsoleProtocol {
  case object Init
  case class InitSuccess(enum: Enumerator[JsValue])
  case class InitFailure(error: String)

  case class ProcessClientEvent(event: JsValue)

  case class UpdateCode(code: String)

  case class Cancelled(wa: WorkerActor)
  case object DoCancel

  case class StorePermaLink(code: String)
  case class AccessPermaLink(link: String)

  case class SynthesisGetRulesToApply(chooseLine: Int, chooseColumn: Int)
  case class SynthesisApplyRule(cid: Int, rid: Int)
  case class SynthesisSearch(cid: Int)

  case class VerificationDoManualVerify(fname: String)
  case class VerificationDoVerify(fnames: Set[String], standalone: Boolean)
  case object VerificationDone

  case class NewCounterExamples(cstate: CompilationState, ces: Map[FunDef, Seq[Expr]])

  case class DispatchTo(module: String, msg: Any)

  case object Stop

  // Communication between session and modules
  case class OnUpdateCode(cstate: CompilationState)
  case class OnClientEvent(cstate: CompilationState, event: JsValue)
  case class NotifyClient(event: JsValue)
  case object Enable
  case object Disable

  case object Quit
}
