package leon.web
package shared
package messages

sealed trait MessageToServer {
  def module: String
}

/// main

sealed trait MainModule { val module = "main" }
case class StorePermaLink(code: String) extends MessageToServer with MainModule
case class AccessPermaLink(link: String) extends MessageToServer with MainModule
case class FeatureSet(feature: String, active: Boolean) extends MessageToServer with MainModule
case class DoUpdateCode(code: String) extends MessageToServer with MainModule
case class DoUpdateCodeInProject(owner: String, repo: String, file: String, branch: String, code: String) extends MessageToServer with MainModule
case object DoCancel extends MessageToServer with MainModule

// synthesis
sealed trait SynthesisModule { val module = "synthesis" }
case class GetRulesToApply(fname: String, cid: Int) extends MessageToServer with SynthesisModule
case class DoApplyRule(fname: String, cid: Int, rid: Int) extends MessageToServer with SynthesisModule
case class DoExplore(fname: String, cid: Int, path: List[Int], exploreAction: String, ws: Int, select: Int) extends MessageToServer with SynthesisModule
case class DoSearch(fname: String, cid: Int) extends MessageToServer with SynthesisModule

// repair
sealed trait RepairModule { val module = "repair" }
case class DoRepair(fname: String) extends MessageToServer with RepairModule

// verification
sealed trait VerificationModule { val module = "verification" }
case class PrettyPrintCounterExample(output: String, rawoutput: String, fname: String) extends MessageToServer with VerificationModule

// git
sealed trait GitModule { self: MessageToServer =>
  val module = "git"
}
sealed trait GitOperation extends GitModule { self: MessageToServer =>
  def name: String
}

case object GitStatus extends MessageToServer with GitOperation {
  val name = GitOperation.STATUS
}
case object GitPull  extends MessageToServer with GitOperation {
  val name = GitOperation.PULL
}
case object GitReset  extends MessageToServer with GitOperation {
  val name = GitOperation.RESET
}

case class GitCommit(message: String) extends MessageToServer with GitOperation {
  val name = GitOperation.COMMIT
}
case class GitPush(force: Boolean)    extends MessageToServer with GitOperation {
  val name = GitOperation.PUSH
}
case class GitLog(number: Int)        extends MessageToServer with GitOperation {
  val name = GitOperation.LOG
}

// Github actions

/** Actions that the React app can trigger.
 *  These will have side effects that are to be reflected
 *  in the global [[leon.web.client.react.AppState]]. */
case class LoadRepository(owner: String, repo: String) extends MessageToServer with GitModule
case object LoadRepositories extends MessageToServer with GitModule
case class SwitchBranch(owner: String, repo: String, branch: String) extends MessageToServer with GitModule
case class LoadFile(owner: String, repo: String, file: String) extends MessageToServer with GitModule
case class DoGitOperation(op: GitOperation, project: Project) extends MessageToServer with GitModule

object GitOperation {
  val STATUS = "status"
  val COMMIT = "commit"
  val PUSH   = "push"
  val PULL   = "pull"
  val RESET  = "reset"
  val LOG    = "log"
}



object PicklersToServer {
  import boopickle.Default._
  implicit val msgPickler = generatePickler[MessageToServer] 
}