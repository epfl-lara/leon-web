package leon.web
package shared
package messages

import git._
import module._

sealed trait MessageToServer {
  def module: Module
}

/// main

sealed trait MainModule { val module = Main }
case class StorePermaLink(code: String) extends MessageToServer with MainModule
case class AccessPermaLink(link: String) extends MessageToServer with MainModule
case class FeatureSet(feature: module.Module, active: Boolean) extends MessageToServer with MainModule
case class DoUpdateCode(code: String) extends MessageToServer with MainModule
case class DoUpdateCodeInProject(owner: String, repo: String, file: String, branch: String, code: String) extends MessageToServer with MainModule
case object DoCancel extends MessageToServer with MainModule

// synthesis
sealed trait SynthesisModule { val module = Synthesis }
case class GetRulesToApply(fname: String, cid: Int) extends MessageToServer with SynthesisModule
case class DoApplyRule(fname: String, cid: Int, rid: Int) extends MessageToServer with SynthesisModule
case class DoExplore(fname: String, cid: Int, path: List[Int], exploreAction: String, ws: Int, select: Int) extends MessageToServer with SynthesisModule
case class DoSearch(fname: String, cid: Int) extends MessageToServer with SynthesisModule

// repair
sealed trait RepairModule { val module = Repair }
case class DoRepair(fname: String) extends MessageToServer with RepairModule

// verification
sealed trait VerificationModule { val module = Verification }
case class PrettyPrintCounterExample(output: String, rawoutput: String, fname: String) extends MessageToServer with VerificationModule

// git
sealed trait GitModule {
  val module = Git
}

/** Actions that the React app can trigger.
 *  These will have side effects that are to be reflected
 *  in the global [[leon.web.client.react.AppState]]. */
case class LoadRepository(owner: String, repo: String) extends MessageToServer with GitModule
case object LoadRepositories extends MessageToServer with GitModule
case class SwitchBranch(owner: String, repo: String, branch: String) extends MessageToServer with GitModule
case class LoadFile(owner: String, repo: String, file: String) extends MessageToServer with GitModule
case class DoGitOperation(op: GitOperation, project: Project) extends MessageToServer with GitModule

object MessageToServer {
  import boopickle.Default._
  implicit val msgPickler = generatePickler[MessageToServer] 
}