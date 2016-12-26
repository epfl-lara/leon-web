package leon.web
package shared
package messages
import git._

sealed trait MessageToServer {
  def module: Module
}

sealed trait MessageToServerExpecting[ReturnType <: MessageFromServer] extends MessageToServer
/// main

sealed trait MainModule { val module = Main }
case class StorePermaLink(code: String) extends MessageToServerExpecting[GotPermalink] with MainModule
case class AccessPermaLink(link: String) extends MessageToServer with MainModule
case class FeatureSet(feature: Module, active: Boolean) extends MessageToServer with MainModule
case object DoCancel extends MessageToServer with MainModule
case class UnlinkAccount(provider: Provider) extends MessageToServer with MainModule
case class SetCommandFlags(flags: String) extends MessageToServer with MainModule

// compilation
case class DoUpdateCode(code: String, requestId: Int) extends MessageToServer with MainModule

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
case class VerificationTimeout(seconds: Int) extends MessageToServer with VerificationModule

// git
sealed trait RepositoryModule {
  val module = RepositoryHandler
}

/** Actions that the React app can trigger.
 *  These will have side effects that are to be reflected
 *  in the global [[leon.web.client.react.AppState]]. */
case class LoadRepository(repo: RepositoryDesc) extends MessageToServer with RepositoryModule
case object LoadRepositories extends MessageToServer with RepositoryModule
case class SwitchBranch(repo: RepositoryDesc, branch: String) extends MessageToServer with RepositoryModule
case class LoadFile(repo: RepositoryDesc, file: String) extends MessageToServer with RepositoryModule
case class DoGitOperation(op: GitOperation, repoState: RepositoryState) extends MessageToServer with RepositoryModule

case class DoUpdateCodeInRepository( code: String, repoState: RepositoryState) extends MessageToServer with RepositoryModule

// Website builder
sealed trait WebsiteBuilderModule {
  val module = WebsiteBuilder
}

case class GetBootstrapSourceCode() extends MessageToServerExpecting[GetBootstrapSourceCode_answer] with WebsiteBuilderModule
case class SubmitStringModification(stringModification: StringModification, sourceCodeId: Int, stringModID: Int) extends MessageToServerExpecting[SubmitStringModification_answer] with WebsiteBuilderModule
case class PointSourceProducingElement(webElementID: Int, charIndex: Int, sourceId: Int) extends MessageToServerExpecting[SourcePositionProducingElement] with WebsiteBuilderModule
case class DownloadWebpage() extends MessageToServerExpecting[DownloadWebpage_answer] with WebsiteBuilderModule

object MessageToServer {
  import boopickle.Default._
  import Module._
  
  implicit val msgPickler = generatePickler[MessageToServer] 
}
