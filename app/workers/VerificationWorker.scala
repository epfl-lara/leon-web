package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.verification._
import leon.xlang.XlangAnalysisPhase
import leon.solvers._
import leon.solvers.z3._
import leon.purescala._
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._

class VerificationWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._
  import leon.evaluators._
  import leon.codegen._

  var verifCrashed   = false
  var verifOverview  = Map[FunDef, List[VerificationCondition]]()
  var ceExecResults  = Map[VerificationCondition, EvaluationResults.Result]()

  def evrToJson(er: EvaluationResults.Result): JsValue = er match {
    case EvaluationResults.Successful(ex) =>
      toJson(Map(
        "result" -> toJson("success"),
        "output" -> toJson(ScalaPrinter(ex))
      ))

    case EvaluationResults.RuntimeError(msg) =>
      toJson(Map(
        "result" -> toJson("error"),
        "error" -> toJson(msg)
      ))

    case EvaluationResults.EvaluatorError(msg) =>
      toJson(Map(
        "result" -> toJson("error"),
        "error" -> toJson(msg)
      ))
  }

  def vcToJson(cstate: CompilationState, vc: VerificationCondition): JsValue = {
    def ceToJson(ce: Map[Identifier, Expr]): JsValue = {
      toJson(ce.map{ case (id, ex) =>
        id.toString -> toJson(ScalaPrinter(ex))
      })
    }

    val base = Map(
      "kind"   -> toJson(vc.kind.toString),
      "fun"    -> toJson(vc.funDef.orig.getOrElse(vc.funDef).id.name),
      "status" -> toJson(vc.status),
      "time"   -> toJson(vc.time.map("%-3.3f".format(_)).getOrElse("")))

    vc.counterExample match {
      case Some(ce) =>
        ceExecResults.get(vc) match {
          case Some(er) =>
            toJson(base + ("counterExample" -> ceToJson(ce), "execution" -> evrToJson(er)))
          case _ =>
            toJson(base + ("counterExample" -> ceToJson(ce)))
        }
      case None =>
        toJson(base)
    }
  }

  def notifyVerifOverview(cstate: CompilationState) {
    case class FunVerif(fd: FunDef, vcs: List[VerificationCondition]) {
      val totalTime = vcs.flatMap(_.time).foldLeft(0d)(_ + _)
      var status = getOverallVCsStatus(vcs)

      def getOverallVCsStatus(vcs: Seq[VerificationCondition]) = {
        var c: Option[String] = None

        for (vc <- vcs if vc.hasValue) {
          if (vc.value == Some(true) && c == None) {
            c = Some("valid")
          }
          if (vc.value == Some(false)) {
            c = Some("invalid")
          }
          if (vc.value == None && c != Some("invalid")) {
            c = Some("timeout")
          }
        }
        if (vcs.isEmpty) {
          c = Some("valid")
        }

        c.getOrElse(if (verifCrashed) "crashed" else "undefined")
      }
    }

    var verifResults = verifOverview.map { case (fd, vcs) =>
      (fd, FunVerif(fd, vcs))
    }

    if (cstate.isCompiled) {
      for ((fd, fv) <- verifResults if fv.status != "valid") {
        for (cfd <- cstate.program.transitiveCallers(fd) if verifResults.get(cfd).map(_.status) == Some("valid")) {
          verifResults(cfd).status = "cond-valid"
        }
      }
    }

    val allCEs = verifOverview.flatMap { case (fd, vcs) =>
      vcs.find(_.counterExample.isDefined) match {
        case Some(vc) =>
          val ce = vc.counterExample.get
          val callArgs = vc.funDef.args.map(ad => ce.getOrElse(ad.id, simplestValue(ad.tpe)))

          Some(vc.funDef.typed(vc.funDef.tparams.map(_.tp)) -> callArgs)

        case None =>
          None
      }
    }.toMap

    sender ! DispatchTo("execution", NewCounterExamples(cstate, allCEs))

    val fvcs = toJson(verifResults.toSeq.sortWith{ (a,b) => a._1.getPos < b._1.getPos }.map{ case (fd, fv) =>
      val v = toJson(Map(
        "status" -> toJson(fv.status),
        "time" -> toJson(fv.totalTime),
        "vcs"  -> toJson(fv.vcs.map(vcToJson(cstate, _)))
      ))

      fv.fd.id.name -> v
    }.toMap)

    event("update_overview", Map("module" -> toJson("verification"), "overview" -> fvcs))
  }

  def doVerify(cstate: CompilationState, funs: Set[FunDef], standalone: Boolean) {
    val verifTimeout = 3000L // 3sec

    val reporter = new WorkerReporter(session)
    var compContext  = leon.Main.processOptions(List("--feelinglucky", "--evalground")).copy(interruptManager = interruptManager, reporter = reporter)

    val solvers = List(SolverFactory(() => (new FairZ3Solver(compContext, cstate.program) with TimeoutSolver).setTimeout(verifTimeout)))

    val vctx = VerificationContext(compContext, solvers, reporter)

    val vcs = verifOverview.collect {
      case (fd, vcs) if funs(fd) => fd -> vcs
    }

    val params = CodeGenParams(maxFunctionInvocations = 5000, checkContracts = false)
    val evaluator = new CodeGenEvaluator(compContext, cstate.program, params)

    try {
      verifCrashed = false
      val vr = AnalysisPhase.checkVerificationConditions(vctx, vcs)

      val report = XlangAnalysisPhase.completeVerificationReport(vr, cstate.functionWasLoop _)

      for ((f, vcs) <- report.fvcs) {
        verifOverview += f -> vcs

        for (vc <- vcs if vc.kind == VCKind.Postcondition) vc.counterExample match {
          case Some(ce) =>
            val callArgs = vc.funDef.args.map(ad => ce.getOrElse(ad.id, simplestValue(ad.tpe)))
            val callExpr = FunctionInvocation(vc.funDef.typed(vc.funDef.tparams.map(_.tp)), callArgs)

            ceExecResults += vc -> evaluator.eval(callExpr)

          case _ =>
            ceExecResults -= vc
        }
      }
    } catch {
      case t: Throwable =>
        verifCrashed = true
        logInfo("[!] Verification crashed!", t)
    }

    notifyVerifOverview(cstate)
  }

  def receive = {
    case OnUpdateCode(cstate) if cstate.isCompiled =>
      val program = cstate.program
      val reporter = new WorkerReporter(session)

      var toGenerate = Set[FunDef]()
      val oldVerifOverView = verifOverview

      val verifFunctions = program.definedFunctions.filter(fd => fd.hasBody).toSet

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

      val toInvalidate = toGenerate.flatMap { program.transitiveCallers _ }

      toGenerate ++= toInvalidate

      if (!toGenerate.isEmpty) {
        clientLog("Generating VCs...")

        toGenerate.foreach{ fd =>
          toGenerate ++= cstate.innerFunctionsOf(fd)
        }

        // Generate VCs
        val fvcs = AnalysisPhase.generateVerificationConditions(reporter, program, toGenerate.map(_.id.name))

        for ((f, vcs) <- fvcs) {
          verifOverview += f -> vcs
        }

        clientLog("Done!")

        notifyVerifOverview(cstate)
      }

      doVerify(cstate, toGenerate, false)

    case DoCancel =>
      sender ! Cancelled(this)

    case OnClientEvent(cstate, event) =>
      (event \ "action").as[String] match {
        case "doVerify" =>
          val fname = (event \ "fname").as[String]

          verifOverview.keySet.find(_.id.name == fname) match {
            case Some(fd) =>
              doVerify(cstate, Set(fd) ++ cstate.innerFunctionsOf(fd), true)
            case None =>
              logInfo("Function "+fname+" not found!")
          }

        case action =>
          notifyError("Received unknown action: "+action)
      }

    case _ =>

  }
}
