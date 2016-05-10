package leon
package web
package workers

import models.ConsoleProtocol
import leon.utils.InterruptManager
import models.ConsoleProtocol
import akka.actor.ActorRef
import models.ConsoleProtocol
import leon.invariant.engine._
import leon.invariant.util._
import leon.purescala.Definitions.ValDef
import leon.transformations.InstUtil
import leon.transformations.InstrumentationPhase
import shared.Module
import models.JsonWrites
import models.InvariantPosition
import play.api.libs.json._
import play.api.libs.json.Json._
import leon.invariant.structure.FunctionUtils._
import purescala.Expressions._
import purescala.Common._
import purescala.ExprOps._
import purescala._
import leon.verification.VCStatus
import leon.verification.VCResult
import models.CompilationState
import models.FunInvariantStatus
import leon.web.shared.Constants
import leon.web.shared.InvariantStatus
import leon.invariant.util._
import ProgramUtil._
import Util._
import leon.invariant.engine._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import leon.purescala.Definitions.Program
import laziness._
import LazinessUtil._
import verification._

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites with VerificationNotifier {
  import ConsoleProtocol._

  private var invariantOverview = Map[String, FunInvariantStatus]()
  private var allCode: Option[String] = None

  private var inferEngine: Option[InferenceEngine] = None
  protected var program: Option[Program] = None
  def receive = {
    case DoCancel =>
      inferEngine.foreach(_.interrupt())
      inferEngine = None
      sender ! Cancelled(this)

    case OnUpdateCode(cstate) =>
      initState(cstate)
      program = Some(cstate.program)
      val startProg = cstate.program
      inferEngine match {
        case Some(engine) =>
          engine.interrupt()
          // reinitialize the interrupt manager
          this.ctx.interruptManager.recoverInterrupt()
        case None =>
      }

      // a call-back for updating progress in the interface
      val progressCallback: InferenceCondition => Unit = (ic: InferenceCondition) => {
        val funName = InstUtil.userFunctionName(ic.fd)
        val userFun = functionByName(funName, startProg).get
        // replace '?' in the template
        val template = userFun.template.map { k =>
          PrettyPrinter(simplePostTransform {
            case Variable(id) if id.name.startsWith("q?") =>
              Variable(FreshIdentifier("?", id.getType))
            case e => e
          }(k))
        }.getOrElse("")
        val timeExecution = ic.time
        invariantOverview += funName ->
          FunInvariantStatus(Some(userFun), Some(template), ic.prettyInv.map(inv => PrettyPrinter(inv)),
              None, timeExecution, ic.prettyInv.isDefined)
        notifyInvariantOverview(cstate)
      }
      // a call-back that would be invoked when inference, which is running in a Future, is completed
      val onInferComplete: InferenceContext => Try[InferenceReport] => Unit = inferctx => _ match {
        case Success(result: InferenceReport) =>
          inferEngine = None
          // TODO : Update ONLY the function, not all the code.
          allCode = Some(ScalaPrinter(InferenceReportUtil.pushResultsToInput(inferctx, result.conditions)))
          notifyInvariantOverview(cstate)
        case Failure(msg) =>
          inferEngine = None
          clientLog("Failed due to: " + msg)
      }

      if (hasLazyEval(startProg)) { // we need to use laziness extension phase
        val leonctx = createLeonContext(ctx, s"--lazy", s"--useOrb", s"--timeout=100")
        val (stateVeriProg, resourceVeriProg) = LazinessEliminationPhase.genVerifiablePrograms(this.ctx, startProg)
        val checkCtx = LazyVerificationPhase.contextForChecks(leonctx)
        Future {
          LazyVerificationPhase.checkSpecifications(stateVeriProg, checkCtx)
        } onComplete {
          case Success(stateResult: VerificationReport) =>
            var failed = false
            stateResult.vrs foreach {
              case (vc, vr) =>
                val funName = vc.fd.id.name
                val userFun = functionByName(funName, startProg).getOrElse(vc.fd)
                val success = vr.isValid
                if (!success) {
                  invariantOverview += (funName ->
                    FunInvariantStatus(Some(userFun), None, None, None, vr.timeMs.map(_ / 1000.0), false, true))
                  notifyInvariantOverview(cstate)
                  failed = true
                }
            }
            if (!failed) {
              //set up state for resource inference
              for (fd <- cstate.functions)
                invariantOverview += fd.id.name -> FunInvariantStatus(Some(fd), None, None, None, None, invariantFound = !fd.hasTemplate)
              notifyInvariantOverview(cstate)
              // resource inference
              val inferctx = LazyVerificationPhase.getInferenceContext(checkCtx, resourceVeriProg)
              val engine = new InferenceEngine(inferctx)
              inferEngine = Some(engine)
              Future {
                LazyVerificationPhase.checkUsingOrb(engine, inferctx, Some(progressCallback))
              } onComplete {
                onInferComplete(inferctx)
              }
            }
          case Failure(msg) =>
            inferEngine = None
            clientLog("Failed due to: " + msg)
        }
      } else {
        setStateBeforeInference(cstate)
        val leonctx = createLeonContext(this.ctx, s"--timeout=120", s"--minbounds", "--vcTimeout=3", "--solvers=smt-z3") //, s"--functions=${inFun.id.name}")
        val inferContext = new InferenceContext(startProg, leonctx)
        val engine = (new InferenceEngine(inferContext))
        inferEngine = Some(engine)
        Future {
          engine.runWithTimeout(Some(progressCallback))
        } onComplete {
          onInferComplete(inferContext)
        }
      }

    case OnClientEvent(cstate, event) =>

    case _                            =>
  }

  def initState(cstate: CompilationState) = {
    for (fd <- cstate.functions) {
      invariantOverview += fd.id.name ->
        FunInvariantStatus(Some(fd), None, None, None, None, false)
    }
    allCode = None
    notifyInvariantOverview(cstate)
  }

  def setStateBeforeInference(cstate: CompilationState) = {
    for (fd <- cstate.functions) {
      val veriStatus =
        if (fd.hasTemplate || fd.getPostWoTemplate != tru) false
        else true
      invariantOverview += fd.id.name ->
        FunInvariantStatus(Some(fd), None, None, None, None, invariantFound = veriStatus)
    }
    allCode = None
    notifyInvariantOverview(cstate)
  }

  def hasLazyEval(program: Program): Boolean = {
    userLevelFunctions(program).exists{fd =>
      isMemoized(fd) || exists(isLazyInvocation(_)(program))(fd.fullBody)
    }
  }

  def hasTemplates(program: Program): Boolean = {
    userLevelFunctions(program).exists(_.hasTemplate)
  }
  
  import shared.messages._

  def funInvariantStatusToOverviewFunction(fi: FunInvariantStatus): InvariantDetails = {
    InvariantDetails(
      status = fi.status,
      fun    = fi.fd.map(InstUtil.userFunctionName(_)).getOrElse(""),
      oldInvariant = fi.template.getOrElse(""),
      newInvariant = fi.invString.getOrElse(""),
      newCode = fi.newCode.getOrElse(""),
      time = fi.time.getOrElse(0.0)   
    )
  }
  
  def notifyInvariantOverview(cstate: CompilationState): Unit = {
    val fics = 
      ((invariantOverview.toSeq.filter(_._2.fd.nonEmpty).sortBy(_._2.fd.map(_.getPos).get).map {
        case (fd, fi) =>
          fd -> funInvariantStatusToOverviewFunction(fi)
      }) ++ allCode.map((code: String) =>
        Constants.invariantMainCode ->
          funInvariantStatusToOverviewFunction(FunInvariantStatus(None, None, None, Some(code), None)))).toMap

    event(HUpdateInvariantsOverview(module = Module.invariant, overview = fics, kind = "", code = ""))
  }

  /**
   * Used only for debugging
   */
  class TestSilentReporter extends DefaultReporter(Set()) {
    var lastErrors: List[String] = Nil

    override def emit(msg: Message): Unit = msg match {
      case Message(this.ERROR, _, msg) => lastErrors ++= List(msg.toString)
      case Message(this.FATAL, _, msg) => lastErrors ++= List(msg.toString)
      case _                           =>
    }
  }
}
