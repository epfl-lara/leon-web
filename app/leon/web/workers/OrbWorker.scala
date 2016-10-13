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
import leon.purescala.Definitions._
import leon.transformations.InstUtil
import leon.transformations.InstrumentationPhase
import shared.Module
import models.StringToExprCached
import models.InvariantPosition
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
import laziness._
import HOMemUtil._
import verification._
import invariant.structure._

/**
 * @author Mikael
 */
class OrbWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with StringToExprCached with VerificationNotifier {
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
      inferEngine match {
        case Some(engine) =>
          engine.interrupt()
          // reinitialize the interrupt manager
          this.ctx.interruptManager.recoverInterrupt()
        case None =>
      }
      val memVerification = hasLazyEval(cstate.program) // we need to use laziness extension phase
       // convert question marks to templates, if any, right here before inlining.
      val funToTmplFun = cstate.functions.flatMap { fd =>
        val (_, tmplOpt) = FunctionUtils.tmplAndPost(fd)
        tmplOpt.map(tmpl => fd -> tmpl).toList
      }.toMap
      val funToTmpl = funToTmplFun.map {
        case (k, FunctionInvocation(_, Seq(Lambda(_, body)))) => k -> body
      }.toMap
      val funNameToTmpl = funToTmplFun.map { case (fd, tfun) => fd.id.name -> tfun }
      val startProg = ProgramUtil.assignTemplateAndCojoinPost(funToTmpl, cstate.program) //note starProg could be mutated due to inlining)

      // a call-back for updating progress in the interface
      val progressCallback: InferenceCondition => Unit = (ic: InferenceCondition) => {
        val timeExecution = Some(ic.time / 1000.0)
        val funName = HOMemUtil.userFunctionName(ic.fd)
        println(s"Looking for function with name: $funName")
        cstate.functions.find(_.id.name == funName) match {
          case None =>
            println(s"Failed to find a function with name: $funName")
          case Some(userFun) =>
            println(s"Found function with name: $funName")
            // replace '?' in the template
            funNameToTmpl.get(funName) match {
              case Some(tmplInvc) =>
                val (userInv, tmpl) = tmplInvc match {
                  case FunctionInvocation(_, Seq(Lambda(args, tmplbody))) =>
                    if(ic.solved) {
                      val model = ic.invariants.head._2
                      val repmap = args.map {
                        case ValDef(argid) =>
                          val value = model.collectFirst { case (id, value) if id.name == argid.name + "?" => value }.get
                          (argid -> value)
                      }.toMap
                      (Some(replaceFromIDs(repmap, tmplbody)), Some(tmplbody))
                    } else
                      (None, Some(tmplbody))
                  case _ =>
                    (None, None)
                }
                val templateStr = tmpl.map { k =>
                    PrettyPrinter(simplePostTransform {
                      case Variable(id) if TVarFactory.isTemp(id, FunctionUtils.qmarkContext)  =>
                        Variable(FreshIdentifier("?", id.getType))
                      case e => e
                    }(k))
                  }.getOrElse("")

                invariantOverview += funName ->
                  FunInvariantStatus(Some(userFun), Some(templateStr), userInv.map(inv => PrettyPrinter(inv)),
                  None, timeExecution, ic.solved)
              case None =>
                invariantOverview += funName ->
                  FunInvariantStatus(Some(userFun), Some(""), ic.prettyInv.map(inv => PrettyPrinter(inv)),
                  None, timeExecution, ic.solved)
            }
            notifyInvariantOverview(cstate)
        }
      }
      // a call-back that would be invoked when inference, which is running in a Future, is completed
      val onInferComplete: InferenceContext => Try[InferenceReport] => Unit = inferctx => _ match {
        case Success(result: InferenceReport) =>
          inferEngine = None
          allCode = Some(ScalaPrinter(InferenceReportUtil.pushResultsToInput(inferctx, result.conditions)))
          notifyInvariantOverview(cstate)
        case Failure(msg) =>
          inferEngine = None
          clientLog("Failed due to: " + msg)
      }

      if (memVerification) {
        val leonctx = createLeonContext(ctx, "--mem", "--unrollfactor=2", "--webMode", "--timeout=120")
        val (clFac, stateVeriProg, resourceVeriProg) = HOInferencePhase.genVerifiablePrograms(this.ctx, startProg)
        val checkCtx = HOMemVerificationPhase.contextForChecks(leonctx)
        Future {
          HOMemVerificationPhase.checkSpecifications(clFac, stateVeriProg, checkCtx)
        } onComplete {
          case Success(stateResult: VerificationReport) =>
            var failed = false
            stateResult.vrs foreach {
              case (vc, vr) =>
                val funName = HOMemUtil.userFunctionName(vc.fd) //vc.fd.id.name
                val userFun = functionByName(funName, startProg).getOrElse(vc.fd)
                val success = vr.isValid
                if (!success) {
                  val VCStatus.Invalid(cex) = vr.status
                  val cexStr = cex.toMap.map{ case (k,v) => k.name +" -> "+ PrettyPrinter(v) }.mkString("; ")
                  invariantOverview += (funName ->
                    FunInvariantStatus(Some(userFun), None, Some(cexStr), None, vr.timeMs.map(_ / 1000.0), false, true))
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
              val inferctx = HOMemVerificationPhase.getInferenceContext(checkCtx, resourceVeriProg)
              val engine = new InferenceEngine(inferctx)
              inferEngine = Some(engine)
              Future {
                HOMemVerificationPhase.checkUsingOrb(engine, inferctx, Some(progressCallback))
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
        val leonctx = createLeonContext(this.ctx, "--webMode", "--timeout=120", "--minbounds=0", "--vcTimeout=3", "--solvers=orb-smt-z3", "-nlTimeout=3")
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
    // TODO: fix this to handle higher-order functions without memoization
    userLevelFunctions(program).exists{fd => fd.flags.contains(IsField(true)) || hasMemAnnotation(fd) }
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

    event(HUpdateInvariantsOverview(overview = fics, kind = "", code = ""))
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
