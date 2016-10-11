package leon.web
package client

import scala.scalajs.js
import js.Dynamic.{ global => g, literal => l/*, newInstance => jsnew*/ }
import leon.web.shared.{Module => ModuleName, Main => MainModule, _}
import shared.messages._
import shared.git._

//import leon.web.client.ops.websocket._

/**
 * @author Mikael
 */
object Backend {
  val activateSessionAnalytics = true
  import Main.Server

  /** A module interaction to send message and report analytics */
  abstract class Module(val module: ModuleName) {
    def event(action: String, label: String) = {
      if(activateSessionAnalytics && !js.isUndefined(g.ga)) {
        g.ga("send",
          l(hitType= "event",
            eventCategory= module.name,
            eventAction= action,
            eventLabel= label))
      }
    }
    implicit class ContinueWith(first: Unit) {
      def andThenAnalytics(second: => Unit) = second
      def andThenAnalytics(action: String, label: String) = event(action, label)
      def andThenAnalytics(action: String) = event(action, "")
    }
  }
  
  object main extends Module(MainModule) {
    def storePermaLink(code: String) =
      Server ! StorePermaLink(code)
    def accessPermaLink(link: String) = 
      Server ! AccessPermaLink(link)
    def setFeatureActive(feature: shared.Module, active: Boolean) =
      Server ! FeatureSet(feature = feature, active = active) andThenAnalytics (Action.featureSet, feature.name + "=" + active)

    def cancel() =
      Server ! DoCancel andThenAnalytics  (Action.doCancel, s"main")
    def unlinkAccount(provider: Provider) =
      Server ! UnlinkAccount(provider = provider) andThenAnalytics (Action.unlinkAccount, provider.id)

  }
  
  object repository extends Module(RepositoryHandler) {

    def doUpdateCodeInProject(repo: Repository, file: String, branch: String, code: String): Unit =
      Server sendBuffered DoUpdateCodeInProject(
        repo   = repo,
        file   = file,
        branch = branch,
        code   = code
      ) andThenAnalytics (Action.doUpdateCodeInProject, s"$repo:$branch:$file")

    def loadRepositories(): Unit =
      Server sendBuffered LoadRepositories andThenAnalytics Action.loadRepositories

    def loadRepository(repo: RepositoryDesc): Unit =
      Server sendBuffered LoadRepository(repo) andThenAnalytics (Action.loadRepositories, s"$repo")

    def loadRepository(repo: Repository): Unit = {
      loadRepository(repo.desc)
    }

    def switchBranch(repo: RepositoryDesc, branch: String): Unit =
      Server sendBuffered SwitchBranch(
        repo   = repo,
        branch = branch
      ) andThenAnalytics (Action.switchBranch, s"$repo:$branch")

    def switchBranch(repo: Repository, branch: String): Unit = {
      switchBranch(repo.desc, branch)
    }

    def loadFile(repo: RepositoryDesc, file: String): Unit =
      Server sendBuffered LoadFile(
        repo   = repo,
        file   = file
      ) andThenAnalytics (Action.loadFile, s"$repo:$file")

    def loadFile(repo: Repository, file: String): Unit = {
      loadFile(repo.desc, file)
    }

    def doGitOperation(op: GitOperation, project: Project): Unit =
      Server sendBuffered DoGitOperation(
        op      = op,
        project = project
      ) andThenAnalytics (Action.doGitOperation, s"$project:$op")
  }

  
  object synthesis extends Module(Synthesis) {
    def getRulesToApply(fname: String, cid: Int) = 
      Server ! GetRulesToApply(fname = fname, cid = cid) andThenAnalytics Action.getRulesToApply
    def doApplyRule(fname: String, cid: Int, rid: Int) =
      Server ! DoApplyRule( fname = fname, cid = cid, rid = rid) andThenAnalytics Action.doApplyRule
    def explore(fname: String, cid: Int,
        path: List[Int] = List[Int](),
        exploreAction: String = "init",
        ws: Int = 0,
        select: Int = 0) =
      Server ! DoExplore(
           fname  = fname,
           cid = cid,
           exploreAction = exploreAction,
           path = path,
           select = select,
           ws = ws) andThenAnalytics (Action.doExplore, exploreAction + select)
    def search(fname: String, cid: Int) =
      Server ! DoSearch(fname = fname, cid = cid) andThenAnalytics (Action.doSearch, fname)
  }
  
  object repair extends Module(Repair) {
    def doRepair(fname: String) =
      Server ! DoRepair(fname = fname) andThenAnalytics (Action.doRepair, fname)
    def cancel() =
      Server ! DoCancel andThenAnalytics  (Action.doCancel, s"repair")
  }
  
  object verification extends Module(Verification) {
    def prettyPrintCounterExample(output: String, rawoutput: String, fname: String) =
      Server ! PrettyPrintCounterExample(
          output = output, rawoutput = rawoutput,
          fname = fname) andThenAnalytics Action.prettyPrintCounterExample
  }
}

