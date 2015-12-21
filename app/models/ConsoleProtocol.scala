package leon.web
package models

import workers.WorkerActor
import play.api.libs.json._
import play.api.libs.iteratee._
import leon.purescala.Definitions._
import leon.purescala.Expressions._
import leon.web.models.github.Repository
import leon.synthesis.Synthesizer
import leon.web.shared.Project

object ConsoleProtocol {
  case object Init
  case class InitSuccess(enum: Enumerator[JsValue])
  case class InitFailure(error: String)

  case class ProcessClientEvent(event: JsValue)

  case class UpdateCode(code: String, user: Option[User], project: Option[Project])

  case class Cancelled(wa: WorkerActor)
  case object DoCancel

  case class StorePermaLink(code: String)
  case class AccessPermaLink(link: String)

  case class LoadRepositories(user: User)
  case class LoadRepository(user: User, owner: String, repo: String)
  case class LoadFile(user: User, owner: String, repo: String, file: String)
  case class SwitchBranch(user: User, owner: String, repo: String, branch: String)
  case class RepositoryLoaded(user: User, repo: Repository)

  case class SynthesisGetRulesToApply(chooseLine: Int, chooseColumn: Int)
  case class SynthesisApplyRule(cid: Int, rid: Int)
  case class SynthesisSearch(cid: Int)

  case class VerificationDoManualVerify(fname: String)
  case class VerificationDoVerify(fnames: Set[String], standalone: Boolean)
  case object VerificationDone

  case class NewCounterExamples(cstate: CompilationState, ces: Map[TypedFunDef, Seq[Expr]])
  case class NewSolutions(cstate: CompilationState, synth: Synthesizer, solutions: Stream[leon.synthesis.Solution])
  case class CreateUpdatePrettyPrinter(cstate: CompilationState, afterFun: Option[FunDef], expr: Expr, output: String)
  
  case class DispatchTo(module: String, msg: Any)

  case object Stop

  // Communication between session and modules
  case class OnUpdateCode(cstate: CompilationState)
  case class OnClientEvent(cstate: CompilationState, event: JsValue)
  case class NotifyClient(event: JsValue)
  case object Enable
  case object Disable

  // Communication between session and JGit ProgressMonitor
  case class OnJGitProgressUpdate(
    taskName: String,
    curWork: Int,
    totalWork: Option[Int] = None,
    percentage: Option[Int] = None
  )

  case class OnJGitProgressEnd(
    taskName: String,
    curWork: Int,
    totalWork: Option[Int] = None,
    percentage: Option[Int] = None
  )

  case object Quit
}
