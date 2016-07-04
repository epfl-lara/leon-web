package leon.web
package shared
package messages
//package leon.web.shared
import github._
import git._
import shared.SourceCodeSubmissionResult

sealed trait MessageFromServer

/** Events triggered in reaction to the [[leon.web.client.react.Action]]s.
  * These events can be listened to, and are meant to trigger state
  * transformations can will themselves trigger a re-render of the app.
  */
sealed trait Event

case class GotPermalink(
  link: String
) extends MessageFromServer

case class Commit(
  hash: String,
  shortHash: String,
  shortMessage: String,
  fullMessage: String,
  commitTime: String,
  author: String,
  committer: String,
  desc: String
)

case class HMoveCursor(line: Double, column: Double = 0) extends MessageFromServer

case class HUpdateOverview(
  overview: Map[String, OverviewFunction]
) extends MessageFromServer

case class HUpdateTerminationOverview(
  overview: Map[String, TerminationDetails]
) extends MessageFromServer

case class HUpdateVerificationOverview(
  overview: Map[String, VerificationDetails]
) extends MessageFromServer


case class HUpdateInvariantsOverview(
  overview: Map[String, InvariantDetails],
  kind: String,
  code: String
) extends MessageFromServer


case class SP(index: Int, line: Int, column: Int, description: String, problem: String)

case class SynthesisOverview(functions: Option[Map[String, Array[SP]]]) extends MessageFromServer

case class HUpdateExplorationFacts(newFacts: Array[NewResult]) extends MessageFromServer

case class NewResult(
  fromRow: Int,
  fromColumn: Int,
  toRow: Int,
  toColumn: Int,
  result: String
)

case class HEditor(
  annotations: Option[Array[CodeAnnotation]]
) extends MessageFromServer

case class HNotification(
  content: String,
  `type`: String
) extends MessageFromServer

case class HLog(
  message: String
) extends MessageFromServer


case class HSynthesisResult(
  result: String,
  cid: Int = 0,
  fname: String = "",
  problem: String = "",
  closed: Double = 0.0,
  total: Double = 0.0,
  solCode: String = "",
  solCodeSimplified: String = "",
  allCode: String = "",
  allCodeSimplified: String = "",
  cursor: Option[HMoveCursor] = None,
  proven: Boolean = false
) extends MessageFromServer

case class HSynthesisProof(
  status: String
) extends MessageFromServer with Status

case class HDisambiguationDisplay(
  var display: String,
  allCode: String
)

case object DisambiguationStarted extends MessageFromServer

case object DisambiguationNoresult extends MessageFromServer

case class HDisambiguationResult(
  input: String,
  fname: String,
  forceAsking: Boolean,
  confirm_solution: HDisambiguationDisplay,
  custom_alternative: HDisambiguationDisplay,
  alternatives: List[HDisambiguationDisplay]
) extends MessageFromServer

case class HSynthesisExploration(
  html: String,
  fname: String,
  cid: Int,
  from: List[Int],
  allCode: String,
  allCodeSimplified: String,
  cursor: Option[HMoveCursor]
) extends MessageFromServer

case class HRulesApps(
  status: String,
  id: Int,
  name: String
) extends Status 

case class HSynthesisRulesToApply(
  fname: String,
  cid: Int,
  rulesApps: Array[HRulesApps]
) extends MessageFromServer

case class HRepairResult(
  result: String = "",
  progress: String = "",
  error: String = "",
  focused: String = "",
  success: String = "",
  solCode: String = "",
  allCode: String = "",
  cursor: Option[HMoveCursor] = None
) extends MessageFromServer

case class ResultOutput(
  result: String,
  output: Option[DualOutput] = None,
  error: Option[String] = None
)

case class DualOutput(rawoutput: String, prettyoutput: String, var modifying: Option[String] = None)

case class VC(
  status: String,
  fun: String,
  kind: String,
  time: String,
  lineFrom: Int,
  lineTo: Int,
  counterExample: Option[Map[String, DualOutput]] = None,
  execution: Option[ResultOutput] = None
) extends Status

case class HReplaceCode(newCode: String) extends MessageFromServer

case class HCompilationProgress(total: Float, current: Float) extends MessageFromServer

case class HCompilation(status: String) extends MessageFromServer with Status

sealed trait Status { def status: String }

case class VerificationDetails(
  fname: String,
  status: String,
  crashingInputs: Option[Map[String, DualOutput]],
  vcs: Array[VC],
  time: Double
) extends MessageFromServer with Status

case class TerminationDetails(
  status: String,
  call: String = "",
  calls: Set[String] = Set(),
  reason: Option[String] = None
) extends Status 

case class InvariantDetails(
  status: String,
  fun: String,
  oldInvariant: String,
  newInvariant: String,
  newCode: String,
  time: Double
)
case class OverviewFunction(
  name: String,
  displayName: String,
  line: Int,
  column: Int
)

case class RepositoriesLoaded (
  repos: Array[Repository]
) extends MessageFromServer with Event

case class RepositoryLoaded(
  repository: Repository,
  files: Array[String],
  branches: Array[Branch],
  currentBranch: String
) extends MessageFromServer with Event

case class FileLoaded(
  file: String,
  content: String
) extends MessageFromServer with Event

case class BranchChanged(
  success: Boolean,
  branch: Option[String],
  files: Option[Array[String]],
  error: Option[String]
) extends MessageFromServer with Event
//case class FileLoaded(fileName: String, content: String) extends Event


//case class BranchChanged(branch: String, files: Seq[String]) extends Message with Event
case class CodeUpdated(code: String) extends MessageFromServer with Event
//case class GitProgress(task: String, percentage: Option[String]) extends Message with Event

case class GitProgress(
  taskName: String,
  status: String,
  percentage: Option[String]
) extends MessageFromServer with Event with Status 

sealed trait GitOperationResult
case class GitStatusDiff(
  status: Map[String, Set[String]],
  diff: String
) extends GitOperationResult
case object GitOperationResultNone extends GitOperationResult
case class GitCommits(
  commits: Seq[Commit]
) extends GitOperationResult

case class GitOperationDone(
  op: GitOperation,
  success: Boolean,
  data: GitOperationResult = GitOperationResultNone
) extends MessageFromServer with Event

case class GetBootstrapSourceCode_answer(bootstrapSourceCode: Option[String]) extends MessageFromServer
case class SubmitSourceCodeResult(result: SourceCodeSubmissionResult, requestId: Int) extends MessageFromServer
case class SubmitStringModification_answer(
                                            stringModificationSubmissionResult: StringModificationSubmissionResult,
                                            requestSourceId: Int,
                                            requestStringModSubResID: Int
                                          ) extends MessageFromServer

object HandlerMessages {
  type VCS = Array[VC]
  type Html = String
  type DataOverView = Map[String, OverviewFunction]
}
  
object MessageFromServer {
  import boopickle.Default._
  import boopickle.PicklerHelper
  import Picklers._
  
  implicit val goPickler = generatePickler[GitOperation]
  
  implicit val annotationPickler = generatePickler[CodeAnnotation]
  implicit val repoPickler = generatePickler[Repository]
  implicit val gopPickler = generatePickler[GitOperationDone]
  implicit val vcPickler = generatePickler[VC]
  implicit val verifPickler = generatePickler[VerificationDetails]
  implicit val msgPickler = generatePickler[MessageFromServer]
}