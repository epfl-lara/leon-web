package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

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

trait VerificationNotifier extends WorkerActor with JsonWrites {
  import ConsoleProtocol._

  protected var verifOverview = Map[FunDef, FunVerifStatus]()
  
  var counterExamplesExprs: List[Map[String, Expr]] = Nil

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

      sender ! DispatchTo(shared.Module.execution, NewCounterExamples(cstate, allCEs))
    }

    val fvcs = Json.obj(
      verifOverview.toSeq.sortBy(_._1.getPos).map{ case (fd, fv) =>
        fd.id.name -> (fv: JsValueWrapper)
      } : _*
    )

    event("update_overview", Map("module" -> toJson("verification"), "overview" -> fvcs))
  }
}

class VerificationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites with VerificationNotifier {
  import ConsoleProtocol._
  import leon.evaluators._
  import leon.codegen._

  override lazy implicit val ctx = leon.Main.processOptions(List(
    "--feelinglucky",
    "--solvers=smt-cvc4,smt-z3,ground",
    "--evalground"
  )).copy(interruptManager = interruptManager, reporter = reporter)
  
  var program: Option[Program] = None
  
  activateCache = true

  def doVerify(cstate: CompilationState, vctx: VerificationContext, funs: Set[FunDef], standalone: Boolean): Unit = {
    val params    = CodeGenParams.default.copy(maxFunctionInvocations = 5000, checkContracts = false)
    val evaluator = new CodeGenEvaluator(vctx.context, cstate.program, params)

    for ((f, fv) <- verifOverview.toSeq.sortBy(_._1.getPos) if funs(f)) {
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
          verifOverview += f -> verifOverview(f).copy(verifCrashed = true)
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
      
      val recomputeEverything = verifFunctions exists { fd => 
        fd.returnType == StringType && {
          val h = FunctionHash(fd)
          (oldVerifOverView find {
            case (f, _) => f.id.name == fd.id.name
          }) match {
            case Some(f) => FunctionHash(f._1) != h 
            case None => true // The function did not exist before
          }
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

      val vctx = VerificationContext(ctx, cstate.program, tsolver, reporter)

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
      (event \ "action").as[String] match {
        case Action.prettyPrintCounterExample =>
          val output = (event \ "output").as[String]
          val rawoutput = (event \ "rawoutput").as[String]
          val fname = (event \ "fname").as[String]
          val exprOpt = getExprFromCache(rawoutput)
          val fd = cstate.program.definedFunctions.find(_.id.name == fname)
          exprOpt match {
            case Some(expr) =>
              sender ! DispatchTo(shared.Module.synthesis, CreateUpdatePrettyPrinter(cstate, fd, expr, output))
            case None =>
              notifyError("Could not find original expression of "+rawoutput)
          }
          
        case action =>
          notifyError("Received unknown action: "+action)
      }

    case _ =>
  }
}
