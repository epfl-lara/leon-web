package leon.web
package workers

import akka.actor._
import models._
import leon.utils._
import leon.verification._
import leon.solvers._
import leon.purescala.Definitions._
import leon.purescala.ExprOps._
import leon.purescala.Expressions._
import leon.purescala.Types._
import shared.Action
import scala.concurrent.duration._
import leon.evaluators.EvaluationResults
import leon.purescala.Common.Identifier
import leon.purescala.SelfPrettyPrinter

trait VerificationNotifier extends WorkerActor with StringToExprCached {
  import ConsoleProtocol._

  protected var verifOverview = Map[FunDef, FunVerifStatus]()
  
  var counterExamplesExprs: List[Map[String, Expr]] = Nil
  import shared.messages.{VC => HVC, _}
  
  def writes(ex: Map[Identifier, Expr]): Map[String, DualOutput] =
    ex.toSeq.map(t => (t._1.asString, t._2)).sortBy(_._1).map {
      case (id, expr) =>
        val rawoutput = expr.asString
        val exprAsString = program.map(p => SelfPrettyPrinter.print(expr, rawoutput)(ctx, p)).getOrElse(rawoutput)
        updateExprCache(rawoutput, expr)
        id -> DualOutput(rawoutput, exprAsString)
    }.toMap
  
  def writes(er: EvaluationResults.Result[Expr]): ResultOutput = er match {
    case EvaluationResults.Successful(ex) =>
      val rawoutput = ex.asString
      val exAsString = program.map(p => SelfPrettyPrinter.print(ex, ex.asString)(ctx, p)).getOrElse(rawoutput)
      updateExprCache(rawoutput, ex)
      ResultOutput("success", output = Some(DualOutput(rawoutput, exAsString)))

    case EvaluationResults.RuntimeError(msg)  => ResultOutput("error", error = Some(msg))
    case EvaluationResults.EvaluatorError(msg) => ResultOutput("error", error = Some(msg))
  }
    
  def getMinMaxRowOf(e: Positioned): (Int, Int) = {
    e.getPos match {
      case r:RangePosition => (r.lineFrom, r.lineTo)
      case p => (p.line, p.line)
    }
  }
  def getMinMaxRow[T](e: T)(extractor: PartialFunction[T, Positioned]): (Int, Int) = {
    if(extractor.isDefinedAt(e)) {
      getMinMaxRowOf(extractor(e))
    } else {
      (0, -1)
    }
  }
  
  def writes(vr: (VC, VCResult, Option[EvaluationResults.Result[Expr]])): HVC = {
    val (vc, res, cexExec) = vr

    val timeSec = res.timeMs.map(t => f"${t/1000d}%-3.3f").getOrElse("")

    val (lineFrom, lineTo) = getMinMaxRowOf(vc)/*.kind match {
      case VCKinds.Postcondition => getMinMaxRow(vc.fd.postcondition){ case Some(c) => c }
      case VCKinds.Precondition => getMinMaxRow(vc.fd.precondition){ case Some(c) => c }
      case VCKinds.Assert => getMinMaxRowOf(vc)
      case _ => (-1, 0)
    }*/
    
    val base = HVC(
      kind   = vc.kind.toString,
      fun    = vc.fd.id.name,
      status = res.status.name,
      lineFrom = lineFrom,
      lineTo = lineTo,
      time   = timeSec
    )

    res.status match {
      case VCStatus.Invalid(cex) =>
        
        cexExec match {
          case Some(er) =>
            base.copy(counterExample = Some(writes(cex.toMap[Identifier, Expr])),
                      execution      = Some(writes(er)))
          case _ =>
            base.copy(counterExample = Some(writes(cex.toMap[Identifier, Expr])))
        }
      case _ =>
        base
    }
  }
  
  def notifyVerifOverview(cstate: CompilationState): Unit = {
    if (cstate.isCompiled) {
      // All functions that depend on an invalid function
      val dependsOnInvalid = verifOverview.filter(_._2.status === "invalid").flatMap { r =>
        cstate.program.callGraph.transitiveCallers(r._1)
      }.toSet

      for ((fd, fv) <- verifOverview) {
        verifOverview += fd -> fv.copy(isCondValid = dependsOnInvalid contains fd)
      }

      // Notify execution worker to run all counter examples
      val allCEs = verifOverview.flatMap { case (fd, fv) =>
        fv.vcData.collect {
          case (vc, VCResult(VCStatus.Invalid(ce), _, _), _) =>
            val callArgs = vc.fd.params.map(ad => ce.getOrElse(ad.id, simplestValue(ad.getType)))
            vc.fd.typed -> callArgs
        }.headOption
      }.toMap

      sender ! DispatchTo(shared.Execution, NewCounterExamples(cstate, allCEs))
    }

    val fvcs = 
      verifOverview.toSeq.sortBy(_._1.getPos).map{ case (fd, fv) =>
        fd.id.name -> VerificationDetails(
           fname = fd.id.name,
           status = fv.status,
           time = fv.totalTime,
           crashingInputs = fv.crashingInputs.map(writes),
           vcs = fv.vcData.toArray.map(writes)
        ) }.toMap

    event(HUpdateVerificationOverview(overview = fvcs))
  }
}

class VerificationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with StringToExprCached with VerificationNotifier {
  import ConsoleProtocol._
  import leon.evaluators._
  import leon.codegen._

  override lazy implicit val ctx = leon.Main.processOptions(List(
    "--feelinglucky",
    "--solvers=smt-cvc4,smt-z3,ground"
  )).copy(interruptManager = interruptManager, reporter = reporter)
  
  var program: Option[Program] = None
  
  activateCache = true

  def doVerify(cstate: CompilationState, vctx: VerificationContext, funs: Set[FunDef], standalone: Boolean): Unit = {
    val params    = CodeGenParams.default.copy(maxFunctionInvocations = 5000, checkContracts = false)
    val evaluator = new CodeGenEvaluator(vctx, cstate.program, params=params)

    for ((f, fv) <- verifOverview.toSeq.sortBy(_._1.getPos) if funs(f)) {
      var crashingInputs: Option[Map[Identifier, Expr]] = None
      try {
        val vcs = fv.vcData.map(_._1).toSeq
        val vr = VerificationPhase.checkVCs(vctx, vcs)

        val resultsWithCex = for ((vc, ovr) <- vr.results) yield {
          val cexExec = ovr match {
            case Some(VCResult(VCStatus.Invalid(ce), _, _)) =>
              val callArgs = vc.fd.params.map(ad => ce.getOrElse(ad.id, simplestValue(ad.getType)))
              val callExpr = FunctionInvocation(vc.fd.typed(vc.fd.tparams.map(_.tp)), callArgs)

              try {
                Some(evaluator.eval(callExpr))
              } catch {
                case e: StackOverflowError =>
                  notifyError("Stack Overflow while testing counter example.")
                  None
                case e: Throwable =>
                  crashingInputs = Some(vc.fd.paramIds.zip(callArgs).toMap)
                  throw e
              }
            case _ =>
              None
          }
          val ovr2 = ovr match {
            case Some(VCResult(VCStatus.Unknown, a, b)) =>
              Some(VCResult(VCStatus.Crashed, a, b))
            case e => e
          }

          (vc, (ovr2, cexExec))
        }

        verifOverview += f -> FunVerifStatus(f, resultsWithCex)

        notifyVerifOverview(cstate)
      } catch {
        case t: Throwable =>
          logInfo("[!] Verification crashed!", t)
          clientLog("Verification crashed: "+t.getMessage())
          verifOverview += f -> verifOverview(f).copy(verifCrashed = true, crashingInputs = crashingInputs)
      }
    }

    notifyVerifOverview(cstate)
  }

  def receive = {
    case OnUpdateCode(cstate) if cstate.isCompiled =>
      this.clearExprCache()
      this.program = Some(cstate.program)
      val program = cstate.program

      var toGenerate = Set[FunDef]()
      val oldVerifOverView = verifOverview

      val verifFunctions = cstate.functions.filter(fd => fd.hasBody).toSet
      
      val recomputeEverything = (verifFunctions exists { fd => 
        fd.returnType == StringType && { // If a pretty printer changed, we recompute everything.
          val h = FunctionHash(fd)
          oldVerifOverView forall { case (f, _) =>
            f.id.name != fd.id.name || FunctionHash(f) != h
          }
        }
        // Or alternatively, it could be that a pretty printer has been deleted, so we need to regenerate everything
      }) || oldVerifOverView.exists{
        case (fd, _) =>
          fd.returnType == StringType && {
            val h = FunctionHash(fd)
            verifFunctions forall ( f =>
              f.id.name != fd.id.name || FunctionHash(f) != h
            )
          }
      }

      // Generate VCs
      for (f <- verifFunctions) {
        val h = FunctionHash(f)

        oldVerifOverView find { case (fd, _) => FunctionHash(fd) === h } match {
          case Some((fd, vcs)) if !recomputeEverything=>
            verifOverview += f -> vcs
          case _ =>
            verifOverview -= f
            toGenerate += f
        }
      }

      verifOverview = verifOverview.filterKeys(verifFunctions)

      val toInvalidate = toGenerate.flatMap { program.callGraph.transitiveCallers _ }

      toGenerate ++= toInvalidate

      val tsolver = SolverFactory.getFromSettings(ctx, program).withTimeout(5.seconds)

      val vctx = new VerificationContext(ctx, cstate.program, tsolver)

      if (!toGenerate.isEmpty) {
        clientLog("Generating VCs...")

        toGenerate.foreach{ fd =>
          toGenerate ++= cstate.innerFunctionsOf(fd)
        }

        // Generate VCs
        val fvcs = VerificationPhase.generateVCs(vctx, toGenerate.toSeq)
        val fvcsMap = fvcs.groupBy(_.fd)

        for (f <- toGenerate) {
          verifOverview += f -> FunVerifStatus(f, fvcsMap.getOrElse(f, Seq()).map { vc =>
            (vc, (None, None))
          }.toMap)
        }

        clientLog("Generated "+fvcs.size+" VC(s) for "+fvcsMap.size+" function(s)!")

        notifyVerifOverview(cstate)
      }

      doVerify(cstate, vctx, toGenerate, false)

    case DoCancel =>
      sender ! Cancelled(this)

    case OnClientEvent(cstate, event) =>
      import shared.messages._
      event match {
        case PrettyPrintCounterExample(output, rawoutput, fname) =>
          val exprOpt = getExprFromCache(rawoutput)
          val fd = cstate.program.definedFunctions.find(_.id.name == fname)
          exprOpt match {
            case Some(expr) =>
              sender ! DispatchTo(shared.Synthesis, CreateUpdatePrettyPrinter(cstate, fd, expr, output))
            case None =>
              notifyError("Could not find original expression of "+rawoutput)
          }
        case _ => notifyError("Received unknown event: "+event)
      }

    case _ =>
  }
}
