package leon.web.client
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Dynamic.{ global => g, literal => l, newInstance => jsnew }

import leon.web.shared.{VerifStatus, TerminationStatus, InvariantStatus}
import leon.web.shared.{Module => ModuleName, Constants, Action}
import leon.web.shared.{Repository, RepositoryDesc, Project, Provider}

import leon.web.client.HandlersTypes._
import leon.web.client.ops.websocket._
import leon.web.client.ops.tojs._

import scala.language.dynamics

/**
 * @author Mikael
 */
object Backend {
  val activateSessionAnalytics = true
  
  /** A module interaction to send message and report analytics */
  abstract class Module(val moduleName: String) {

    private def _send(msg: String): Unit =
      Main.leonSocket.send(msg)

    private def _sendBuf(msg: String): Unit =
      Main.leonSocket.sendBuffered(msg)

    protected def _send(msg: js.Dynamic): Unit =
      _send(JSON.stringify(msg))

    protected def _sendBuf(msg: js.Dynamic): Unit =
      _sendBuf(JSON.stringify(msg))

    object send extends scala.Dynamic {
      def applyDynamicNamed(method: String)(fields: (String, js.Any)*): Unit = {
        _send(l.applyDynamicNamed("apply")((fields.toSeq :+ ("module" -> (moduleName: js.Any))): _*))
      }
    }

    object sendBuf extends scala.Dynamic {
      def applyDynamicNamed(method: String)(fields: (String, js.Any)*): Unit = {
        _sendBuf(l.applyDynamicNamed("apply")((fields.toSeq :+ ("module" -> (moduleName: js.Any))): _*))
      }
    }

    def event(action: String, label: String) = {
      if(activateSessionAnalytics && !js.isUndefined(g.ga)) {
        g.ga("send",
          l(hitType= "event",
            eventCategory= moduleName,
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

  object main extends Module(ModuleName.main) {

    def doUpdateCode(code: String): Unit =
      sendBuf(
        action = Action.doUpdateCode,
        code = code
      ) andThenAnalytics Action.doUpdateCode

    def storePermaLink(code: String) =
      send(action = Action.storePermaLink, code = code)

    def accessPermaLink(link: String) = 
      send(action = Action.accessPermaLink, link = link)

    def setFeatureActive(feature: String, active: Boolean) =
      send(action = Action.featureSet, feature = feature, active = active) andThenAnalytics (Action.featureSet, feature + "=" + active)

    def unlinkAccount(provider: Provider) =
      send(
        action   = Action.unlinkAccount,
        provider = provider.id
      ) andThenAnalytics (Action.unlinkAccount, provider.id)

    def cancel() =
      send(action = Action.doCancel) andThenAnalytics Action.doCancel
  }

  object repository extends Module(ModuleName.repository) {

    def doUpdateCodeInProject(repo: RepositoryDesc, file: String, branch: String, code: String): Unit =
      sendBuf(
        action = Action.doUpdateCodeInProject,
        repo   = repo.toJS,
        file   = file,
        branch = branch,
        code   = code
      ) andThenAnalytics (Action.doUpdateCodeInProject, s"$repo:$branch:$file")

    def doUpdateCodeInProject(repo: HRepository, file: String, branch: String, code: String): Unit = {
      val desc = RepositoryDesc.fromGitHub(repo.owner, repo.name)
      doUpdateCodeInProject(desc, file, branch, code)
    }

    def loadRepositories(): Unit =
      sendBuf(
        action = Action.loadRepositories
      ) andThenAnalytics Action.loadRepositories

    def loadRepository(repo: RepositoryDesc): Unit =
      sendBuf(
        action = Action.loadRepository,
        repo   = repo.toJS
      ) andThenAnalytics (Action.loadRepositories, s"$repo")

    def loadRepository(repo: HRepository): Unit = {
      val desc = RepositoryDesc.fromGitHub(repo.owner, repo.name)
      loadRepository(desc)
    }

    def switchBranch(repo: RepositoryDesc, branch: String): Unit =
      sendBuf(
        action = Action.switchBranch,
        repo   = repo.toJS,
        branch = branch
      ) andThenAnalytics (Action.switchBranch, s"$repo:$branch")

    def switchBranch(repo: HRepository, branch: String): Unit = {
      val desc = RepositoryDesc.fromGitHub(repo.owner, repo.name)
      switchBranch(desc, branch)
    }

    def loadFile(repo: RepositoryDesc, file: String): Unit =
      sendBuf(
        action = Action.loadFile,
        repo   = repo.toJS,
        file   = file
      ) andThenAnalytics (Action.loadFile, s"$repo:$file")

    def loadFile(repo: HRepository, file: String): Unit = {
      val desc = RepositoryDesc.fromGitHub(repo.owner, repo.name)
      loadFile(desc, file)
    }

    def doGitOperation(op: String, data: js.Any, project: Project): Unit =
      sendBuf(
        action  = Action.doGitOperation,
        op      = op,
        data    = data,
        project = project.toJS
      ) andThenAnalytics (Action.doGitOperation, s"$project:$op")
  }

  object synthesis extends Module(ModuleName.synthesis) {
    def getRulesToApply(fname: String, cid: Int) = 
      send(action = Action.getRulesToApply, fname = fname, cid = cid) andThenAnalytics Action.getRulesToApply
    def doApplyRule(fname: String, cid: Int, rid: Int) =
      send(action = Action.doApplyRule,  fname = fname, cid = cid, rid = rid) andThenAnalytics Action.doApplyRule
    def explore(fname: String, cid: Int,
        path: js.Array[Int] = js.Array[Int](),
        exploreAction: String = "init",
        ws: Int = 0,
        select: Int = 0) =
      send(action = Action.doExplore,
           fname  = fname,
           cid = cid,
           exploreAction = exploreAction,
           path = path,
           select = select,
           ws = ws) andThenAnalytics (Action.doExplore, exploreAction + select)
    def search(fname: String, cid: Int) =
      send(action = Action.doSearch, fname = fname, cid = cid) andThenAnalytics (Action.doSearch, fname)
  }
  
  object repair extends Module(ModuleName.repair) {
    def doRepair(fname: String) =
      send(action = Action.doRepair, fname = fname) andThenAnalytics (Action.doRepair, fname)
    def cancel() =
      send(action = Action.doCancel) andThenAnalytics Action.doCancel
  }
  
  object verification extends Module(ModuleName.verification) {
    def prettyPrintCounterExample(output: String, rawoutput: String, fname: String) =
      send(action = Action.prettyPrintCounterExample,
          output = output, rawoutput = rawoutput,
          fname = fname) andThenAnalytics Action.prettyPrintCounterExample
  }
}
