package leon.web
package models

import workers.WorkerActor
import play.api.libs.iteratee._
import leon.purescala.Definitions._
import leon.purescala.Expressions._
import leon.web.shared.github.Repository
import leon.synthesis.Synthesizer
import leon.web.shared.{Project}
import leon.web.shared.messages._
import shared.git.GitOperation
import shared.{Provider, Repository, RepositoryDesc}
import java.nio.ByteBuffer

object ConsoleProtocol {
  case object Init
  case class InitSuccess(enum: Enumerator[Array[Byte]])
  case class InitFailure(error: String)

  case class ProcessClientEvent(event: Array[Byte])

  case class UpdateCode(code: String, user: Option[User], project: Option[Project])

  case class Cancelled(wa: BaseActor)
  case object DoCancel
  case class ULoadRepositories(user: User)
  case class ULoadRepository(user: User, repo: RepositoryDesc)
  case class ULoadFile(user: User, repo: RepositoryDesc, file: String)
  case class USwitchBranch(user: User, repo: RepositoryDesc, branch: String)
  case class UDoGitOperation(user: User, project: Project, op: GitOperation)
  case class URepositoryLoaded(user: User, repo: Repository, currentBranch: String)
  case class UUnlinkAccount(user: User, provider: Provider)
  case class UUserUpdated(user: Option[User])

  case class SynthesisGetRulesToApply(chooseLine: Int, chooseColumn: Int)
  case class SynthesisApplyRule(cid: Int, rid: Int)
  case class SynthesisSearch(cid: Int)

  case class VerificationDoManualVerify(fname: String)
  case class VerificationDoVerify(fnames: Set[String], standalone: Boolean)
  case object VerificationDone

  case class NewCounterExamples(cstate: CompilationState, ces: Map[TypedFunDef, Seq[Expr]])
  case class NewSolutions(cstate: CompilationState, synth: Synthesizer, solutions: Stream[leon.synthesis.Solution])
  case class CreateUpdatePrettyPrinter(cstate: CompilationState, afterFun: Option[FunDef], expr: Expr, output: String)
  
  case class DispatchTo(module: shared.module.Module, msg: Any)

  case object Stop

  // Communication between session and modules
  case class OnUpdateCode(cstate: CompilationState)
  case class OnClientEvent(cstate: CompilationState, event: shared.messages.MessageToServer)
  case class NotifyClient(event: shared.messages.MessageFromServer)
  case class NotifyClientBin(event: Array[Byte])
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
