package leon.web
package shared

object HandlerMessages {
  sealed trait Message

  case class HRepository(
    id: Long,
    name: String,
    fullName: String,
    owner: String,
    visibility: String,
    fork: Boolean,
    size: Long,
    cloneURL: String,
    defaultBranch: String,
    branches: Array[String]
  )
  
  case class HBranch(
    name: String,
    sha: String
  )
  

  case class HPermalink(
    link: String
  ) extends Message

  case class HRepositories(
    repos: Array[HRepository]
  )

  case class HRepositoryLoaded(
    repository: HRepository,
    files: Array[String],
    branches: Array[HBranch],
    currentBranch: String
  )

  case class HFileLoaded(
    file: String,
    content: String
  )

  case class HBranchChanged(
    success: Boolean,
    branch: Option[String],
    files: Option[Array[String]],
    error: Option[String]
  )

  case class HGitProgress(
    taskName: String,
    status: String,
    percentage: Option[String]
  ) extends Status 

  case class HGitOperationResult(
    op: String,
    success: Boolean,
    data: Any
  )

  case class HCommit(
    hash: String,
    shortHash: String,
    shortMessage: String,
    fullMessage: String,
    commitTime: String,
    author: String,
    committer: String,
    desc: String
  )
  
  case class HMoveCursor(line: Double) extends Message
  
  type DataOverView = Map[String, OverviewFunction]
  
  case class HUpdateOverview(
    module: String,
    overview: DataOverView
  ) extends Message
  
  case class SP(index: Int, line: Int, description: String)
  
  case class SynthesisOverview(functions: Option[Map[String, Array[SP]]]) extends Message
  
  case class HUpdateExplorationFacts(newFacts: Array[NewResult]) extends Message
  
  case class NewResult(
    fromRow: Int,
    fromColumn: Int,
    toRow: Int,
    toColumn: Int,
    result: String
  )

  case class Annotation(
    row: Double,
    column: Double,
    text: String,
    var `type`: String
  )
  
  case class HEditor(
    annotations: Option[Array[Annotation]]
  ) extends Message

  case class HNotification(
    content: String,
    `type`: String
  ) extends Message
  
  case class HLog(
    message: String
  ) extends Message
  
  
  case class HSynthesisResult(
    result: String,
    cid: Int,
    fname: String,
    problem: String,
    closed: Double,
    total: Double,
    solCode: String,
    allCode: String,
    cursor: Option[HMoveCursor]
  ) extends Message
  
  case class HDisambiguationDisplay(
    var display: String,
    allCode: String
  )
  
  case class DisambiguationStarted() extends Message
  
  case class DisambiguationNoresult() extends Message
  
  case class HDisambiguationResult(
    input: String,
    fname: String,
    confirm_solution: HDisambiguationDisplay,
    custom_alternative: HDisambiguationDisplay,
    alternatives: Array[HDisambiguationDisplay]
  ) extends Message
  
  case class HSynthesisExploration(
    html: String,
    fname: String,
    cid: Int,
    from: Array[String],
    allCode: String,
    cursor: Option[HMoveCursor]
  ) extends Message

  case class HRulesApps(
    status: String,
    id: Int,
    name: String
  ) extends Status 
  
  case class HSynthesisRulesToApply(
    fname: String,
    cid: Int,
    rulesApps: Array[HRulesApps]
  )

  case class HRepairResult(
    result: String,
    progress: String,
    error: String,
    focused: String,
    success: String,
    solCode: String,
    allCode: String,
    cursor: Option[HMoveCursor]
  ) extends Message
  
  case class ResultOutput(
    result: String,
    output: DualOutput
  )
  
  case class DualOutput(rawoutput: String, prettyoutput: String, modifying: Option[String])
  
  case class VC(
    status: String,
    fun: String,
    kind: String,
    time: String,
    counterExample: Option[Map[String, DualOutput]],
    execution: Option[ResultOutput]
  ) extends Status
  
  case class HReplaceCode(newCode: String) extends Message
  
  case class HCompilationProgress(total: Float, current: Float) extends Message

  case class HCompilation(status: String) extends Message with Status
  
  case class StatusCode(
    status: String,
    code: String
  )
   extends Status 
  trait Status{ def status: String }
  
  case class VerificationDetails(
    status: String,
    vcs: Array[VC]
  ) extends Message with Status
  
  case class TerminationDetails(
    status: String,
    call: String,
    calls: Array[String],
    reason: Option[String]
  ) extends Status 
  
  case class InvariantDetails(
    status: String,
    fun: String,
    oldInvariant: String,
    newInvariant: String,
    newCode: String,
    time: Double
  )
  
  case class HInvariants(
    invariants: Array[InvariantDetails],
    kind: String,
    module: String,
    code: String
  )
  
  type Html = String
  
  case class OverviewFunction(
    name: String,
    displayName: String,
    line: Int,
    column: Int
  )
}