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
import leon.web.shared.messages.GetBootstrapSourceCode
import leon.web.websitebuilder.programEvaluator.LeonProgramMaker
import leon.web.shared.SourceCodeSubmissionResult
import leon.web.websitebuilder.logging.serverReporter.ServerReporter
import leon.web.websitebuilder.stringModification.StringModificationProcessor
import leon.web.websitebuilder.programEvaluator.ProgramEvaluator
import leon.web.shared.messages.GetBootstrapSourceCode_answer
import leon.web.shared.messages.SubmitStringModification_answer
import leon.web.shared.messages.SubmitStringModification
import leon.web.shared.messages.SubmitSourceCodeResult
import leon.web.shared.messages.MessageFromServer
import leon.web.websitebuilder.logging.serverReporter._
import leon.evaluators.DefaultEvaluator

class WebBuilderWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._

  def receive = {
    case OnUpdateCode(c) => 
      logInfo("WebBuilder received code requestId = "+c.requestId+", processing...")
      //println(c.program)
      ProgramEvaluator.functionToEvaluate = c.program.definedFunctions.find{ fd =>
        fd.returnType match {
          case CaseClassType(ccd, targs) => ccd.id.name == "WebPage"
          case _ => false
        }
      }
      
      event(processNewCode(c, true))
    case OnClientEvent(cstate, s) =>
      s match {
        case s: SubmitStringModification => 
          println("Will process string modification " + s)
          event(processStringModificationSubmission(cstate, s))
        case _ => notifyError("Unknown event for WebBuilderWorker : " + s)
      }

    case DoCancel =>
      sender ! Cancelled(this)

    case _ =>
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
        val javascript = cstate.program.definedFunctions.find{
          case fd => 
            fd.params.length == 0 && fd.id.name == "javascript" && fd.returnType == StringType
        }.flatMap{ fd =>
          new DefaultEvaluator(ctx, cstate.program).eval(FunctionInvocation(fd.typed, Seq())).result match {
            case Some(StringLiteral(s)) => Some(s)
            case _ => None
          }
        }

        SubmitSourceCodeResult(SourceCodeSubmissionResult(Some(webPageWithIDedWebElement), evaluationLog), javascript, requestId)
      case (None, evaluationLog) =>
        Memory.setSourceMap(requestId, () => None)(null)
        SubmitSourceCodeResult(SourceCodeSubmissionResult(None,
          s"""
             |ProgramEvaluator did not manage to evaluate and unexpr the result of the leon program.
             | Here is the evaluation log: $evaluationLog
          """.stripMargin), None, requestId)
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