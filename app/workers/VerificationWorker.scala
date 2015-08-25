package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.verification._
import leon.solvers._
import leon.solvers.combinators._
import leon.solvers.smtlib._
import leon.solvers.z3._
import leon.purescala._
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.ExprOps._
import leon.purescala.Expressions._

import scala.concurrent.duration._

class VerificationWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) with JsonWrites {
  import ConsoleProtocol._
  import leon.evaluators._
  import leon.codegen._

  override lazy implicit val ctx = leon.Main.processOptions(List(
    "--feelinglucky",
    "--solvers=smt-cvc4,smt-z3,ground",
    "--evalground"
  )).copy(interruptManager = interruptManager, reporter = reporter)

  private var verifOverview  = Map[FunDef, FunVerifStatus]()

  def notifyVerifOverview(cstate: CompilationState) {
    if (cstate.isCompiled) {
      // All functions that depend on an invalid function
      val dependsOnInvalid = verifOverview.filter(_._2.status == "invalid").flatMap { r =>
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

      sender ! DispatchTo("execution", NewCounterExamples(cstate, allCEs))
    }

    val fvcs = Json.obj(
      verifOverview.toSeq.sortBy(_._1.getPos).map{ case (fd, fv) =>
        fd.id.name -> (fv: JsValueWrapper)
      } : _*
    )

    event("update_overview", Map("module" -> toJson("verification"), "overview" -> fvcs))
  }

  def doVerify(cstate: CompilationState, vctx: VerificationContext, funs: Set[FunDef], standalone: Boolean) {
    val params    = CodeGenParams.default.copy(maxFunctionInvocations = 5000, checkContracts = false)
    val evaluator = new CodeGenEvaluator(vctx.context, cstate.program, params)

    for ((f, fv) <- verifOverview.toSeq.sortBy(_._1.getPos) if funs(f)) {
      try {
        val vcs = fv.vcData.map(_._1).toSeq
        val vr = AnalysisPhase.checkVCs(vctx, vcs)

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

          vc -> (ovr, cexExec)
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
      val program = cstate.program

      var toGenerate = Set[FunDef]()
      val oldVerifOverView = verifOverview

      val verifFunctions = cstate.functions.filter(fd => fd.hasBody).toSet

      // Generate VCs
      for (f <- verifFunctions) {
        val h = FunctionHash(f)

        oldVerifOverView find { case (fd, _) => FunctionHash(fd) == h } match {
          case Some((fd, vcs)) =>
            verifOverview += f -> vcs
          case None =>
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
        val fvcs = AnalysisPhase.generateVCs(vctx, toGenerate.toSeq)
        val fvcsMap = fvcs.groupBy(_.fd)

        for (f <- toGenerate) {
          verifOverview += f -> FunVerifStatus(f, fvcsMap.getOrElse(f, Seq()).map { vc =>
            vc -> (None, None)
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
        case action =>
          notifyError("Received unknown action: "+action)
      }

    case _ =>
  }
}
