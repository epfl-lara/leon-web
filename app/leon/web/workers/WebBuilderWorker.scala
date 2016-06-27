package leon.web
package workers

import akka.actor._
import models._
import leon.utils._
import leon.purescala.Expressions._
import leon.purescala.Definitions._
import leon.purescala.Types._
import websitebuilder._
import memory._
import leon.web.shared.messages._
import leon.web.shared.SourceCodeSubmissionResult
import leon.web.websitebuilder.logging.serverReporter.ServerReporter
import leon.web.websitebuilder.stringModification.StringModificationProcessor
import leon.web.websitebuilder.programEvaluator.ProgramEvaluator
import leon.web.websitebuilder.bootstrapSourceCode.BootstrapSourceCodeGetter
import leon.web.websitebuilder.logging.serverReporter._
import leon.web.websitebuilder.programEvaluator.dustbin.LeonProgramMaker

class WebBuilderWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._

  def receive = {
    case s: RequestInitialClientWebBuildingState =>
      event(NewClientWebBuildingState(APIForClient.requestInitialClientWBState()))

//      TODO: Is this wrapping in OnClientEvent really needed? Why is it here?
    case OnClientEvent(cstate, s) =>
      s match {
        case s:SendStringModification =>
          println("Will process string modification " + s)
          event(NewClientWebBuildingState(APIForClient.stringModification(s.stringModification, s.baseServerWBStateID)))
        case _ => notifyError("Unknown event for WebBuilderWorker : " + s)
      }

    case OnUpdateCode(cstate) =>
      logInfo("WebBuilder received an update on the source code. requestId = "+cstate.requestId)
//      TODO: What to do if cstate.code is worth None?
      event(NewClientWebBuildingState(APIForClient.sourceCodeChange(cstate.code.get)))

    case DoCancel =>
      sender ! Cancelled(this)

    case _ =>
  }

  def processGetBootstrapSourceCode(getBootstrapSourceCode: GetBootstrapSourceCode): MessageFromServer = {
    val serverReporter = new ServerReporter
    val src = BootstrapSourceCodeGetter.getBootstrapSourceCode(serverReporter)
    GetBootstrapSourceCode_answer(src)
  }

  def processNewCode(cstate: CompilationState, onUserRequest: Boolean, forceFunDef: Option[FunDef] = None): SubmitSourceCodeResult = {
    val requestId = cstate.requestId.getOrElse(0)
    val sourceCode = cstate.code.getOrElse("")
    val serverReporter = new ServerReporter
    val program = cstate.program
    ProgramEvaluator.evaluateAndConvertResult(program, sourceCode, forceFunDef, serverReporter) match {
      case (Some((webPageWithIDedWebElement, sourceMapProducer, ctx)), evaluationLog) =>
        if (onUserRequest) {
          Memory.setSourceMap(requestId, sourceMapProducer)(ctx)
        } else {
          Memory.setAutoSourceMap(requestId, sourceMapProducer)(ctx)
        }

        SubmitSourceCodeResult(SourceCodeSubmissionResult(Some(webPageWithIDedWebElement), evaluationLog), requestId)
      case (None, evaluationLog) =>
        Memory.setSourceMap(requestId, () => None)(null)
        SubmitSourceCodeResult(SourceCodeSubmissionResult(None,
          s"""
             |ProgramEvaluator did not manage to evaluate and unexpr the result of the leon program.
             | Here is the evaluation log: $evaluationLog
          """.stripMargin), requestId)
    }
  }

  def processStringModificationSubmission(cstate: CompilationState, submission: SubmitStringModification): SubmitStringModification_answer = {
    val sReporter = new ServerReporter
    val stringModification = submission.stringModification
    val stringModID = submission.stringModID
    val sourceCodeId = submission.sourceCodeId
    sReporter.report(Info,
      s"""Received a string modification from the client:
          |  webElementID: ${stringModification.webElementID}
          |  modified WebAttribute${stringModification.modifiedWebAttribute}
          |  new value: ${stringModification.newValue}
          |  id: ${stringModID}
           """.stripMargin
    )
    val weID = stringModification.webElementID
    val weExprFromSourceMap = Memory.getSourceMap(sourceCodeId) match {
      case Some(sourceMap) =>
        sourceMap.webElementIDToExpr(weID)
      case None =>
        throw new Exception(s"Could not find code with sourceCodeId = $sourceCodeId, maybe not up-to-date (last recorded is " + Memory.lastSourceId + ")")
    }

    sReporter.report(Info,
      s"""Here's what has been found in the sourceMap for the webElementID $weID:
          |${weExprFromSourceMap}
           """.stripMargin)

    SubmitStringModification_answer(StringModificationProcessor.process(this, cstate, stringModification, sourceCodeId, sReporter), sourceCodeId, stringModID)
  }
}