package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.verification._
import leon.xlang.XLangAnalysisPhase
import leon.solvers._
import leon.solvers.combinators._
import leon.solvers.smtlib._
import leon.solvers.z3._
import leon.purescala._
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.ExprOps._
import leon.purescala.Expressions._

class VerificationWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._
  import leon.evaluators._
  import leon.codegen._

  implicit val erWrites = new Writes[EvaluationResults.Result] {
    def writes(er: EvaluationResults.Result) = er match {
      case EvaluationResults.Successful(ex) =>
        Json.obj(
          "result" -> "success",
          "output" -> ScalaPrinter(ex)
        )

      case EvaluationResults.RuntimeError(msg) =>
        Json.obj(
          "result" -> "error",
          "error"  -> msg
        )

      case EvaluationResults.EvaluatorError(msg) =>
        Json.obj(
          "result" -> "error",
          "error"  -> msg
        )
    }
  }

  implicit val exWrites = new Writes[Map[Identifier, Expr]] {
    def writes(ex: Map[Identifier, Expr]) = Json.obj(
      ex.toSeq.sortBy(_._1.name).map {
        case (id, expr) => id.toString -> (ScalaPrinter(expr): JsValueWrapper)
      } :_*
    )
  }

  implicit val vrWrites = new Writes[(VC, VCResult)] {
    def writes(vr: (VC, VCResult)) = {
      val (vc, res) = vr

      val timeSec = res.timeMs.map(t => f"${t/1000d}%-3.3f").getOrElse("")

      val base = Json.obj(
        "kind"   -> vc.kind.toString,
        "fun"    -> vc.fd.orig.getOrElse(vc.fd).id.name,
        "status" -> res.status.name,
        "time"   -> timeSec
      )

      res.status match {
        case VCStatus.Invalid(cex) =>
          ceExecResults.get(vc) match {
            case Some(er) =>
              base ++ Json.obj(
                "counterExample" -> cex,
                "execution"      -> er
              )
            case _ =>
              base ++ Json.obj(
                "counterExample" -> cex
              )
          }
        case _ =>
          base
      }
    }
  }

  implicit val fvWrites = new Writes[FunVerifStatus] {
    def writes(fv: FunVerifStatus) = Json.obj(
      "status" -> fv.status,
      "time"   -> fv.totalTime,
      "vcs"    -> fv.vrs.toSeq
    )
  }



  case class FunVerifStatus(fd: FunDef, results: Map[VC, Option[VCResult]], isCondValid: Boolean = false) {
    def vrs = results.mapValues(_.getOrElse(VCResult.unknown))

    lazy val totalTime: Long = vrs.flatMap(_._2.timeMs).foldLeft(0L)(_ + _)

    lazy val overallStatus = {
      val rs = vrs.map(_._2)

      if (rs.exists(_.isInvalid)) {
        "invalid"
      } else if (rs.exists(_.status == VCStatus.Timeout)) {
        "timeout"
      } else if (vrs.isEmpty || rs.forall(_.isValid)) {
        "valid"
      } else if (verifCrashed) {
        "crashed"
      } else {
        "undefined"
      }
    }

    lazy val status: String = {
      if (isCondValid && overallStatus == "valid") {
        "cond-valid"
      } else {
        overallStatus
      }
    }
  }

  var verifCrashed   = false
  var verifOverview  = Map[FunDef, FunVerifStatus]()
  var ceExecResults  = Map[VC, EvaluationResults.Result]()

  def notifyVerifOverview(cstate: CompilationState) {

    if (cstate.isCompiled) {
      verifOverview = verifOverview.mapValues(_.copy(isCondValid = false))

      // Cond-valid-ify if a valid function depends on an invalid one
      for ((fd, fv) <- verifOverview if fv.status == "invalid") {
        for (cfd <- cstate.program.callGraph.transitiveCallers(fd)) {
          verifOverview.get(cfd) match {
            case Some(vf) =>
              verifOverview += cfd -> vf.copy(isCondValid = true)
            case None =>
              // ?!?
          }
        }
      }
    }

    val allCEs = verifOverview.flatMap { case (fd, fv) =>
      fv.vrs.collect {
        case (vc, VCResult(VCStatus.Invalid(ce), _, _)) =>
          val callArgs = vc.fd.params.map(ad => ce.getOrElse(ad.id, simplestValue(ad.getType)))
          vc.fd.typed(vc.fd.tparams.map(_.tp)) -> callArgs
      }.headOption
    }.toMap

    sender ! DispatchTo("execution", NewCounterExamples(cstate, allCEs))

    val fvcs = Json.obj(
      verifOverview.toSeq.sortBy(_._1.getPos).map{ case (fd, fv) =>
        fd.id.name -> (fv: JsValueWrapper)
      } : _*
    )

    event("update_overview", Map("module" -> toJson("verification"), "overview" -> fvcs))
  }

  def doVerify(cstate: CompilationState, vctx: VerificationContext, funs: Set[FunDef], standalone: Boolean) {
    try {
      val params = CodeGenParams.default.copy(maxFunctionInvocations = 5000, checkContracts = false)
      val evaluator = new CodeGenEvaluator(vctx.context, cstate.program, params)

      verifCrashed = false

      // Keep only C-EX that are not verified here
      ceExecResults = ceExecResults.filterKeys(vc => !funs(vc.fd))

      for ((f, fv) <- verifOverview if funs(f)) {
        val vcs = fv.vrs.map(_._1).toSeq
        val vr = AnalysisPhase.checkVCs(vctx, vcs)

        for ((vc, ovr) <- vr.results) {
          ovr.map(_.status) match {
            case Some(VCStatus.Invalid(ce)) =>
              val callArgs = vc.fd.params.map(ad => ce.getOrElse(ad.id, simplestValue(ad.getType)))
              val callExpr = FunctionInvocation(vc.fd.typed(vc.fd.tparams.map(_.tp)), callArgs)

              try {
                ceExecResults += vc -> evaluator.eval(callExpr)
              } catch {
                case e: StackOverflowError =>
                  notifyError("Stack Overflow while testing counter example.")
              }

            case _ =>
          }
        }

        val nfv = FunVerifStatus(f, vr.results)
        if (nfv != fv) {
          verifOverview += f -> nfv
          notifyVerifOverview(cstate)
        }
      }
    } catch {
      case t: Throwable =>
        verifCrashed = true
        logInfo("[!] Verification crashed!", t)
        clientLog("Verification crashed: "+t.getMessage())
    }
  }

  val reporter = new WorkerReporter(session)

  val ctx = leon.Main.processOptions(List("--feelinglucky", "--evalground")).copy(interruptManager = interruptManager, reporter = reporter)


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

      val tsolver = SolverFactory.getFromName(ctx, program)("smt-z3").withTimeout(20000L)

      val vctx = VerificationContext(ctx, cstate.program, tsolver, reporter)

      if (!toGenerate.isEmpty) {
        clientLog("Generating VCs...")

        toGenerate.foreach{ fd =>
          toGenerate ++= cstate.innerFunctionsOf(fd)
        }

        // Generate VCs
        val fvcs = AnalysisPhase.generateVCs(vctx, Some(toGenerate.map(_.id.name).toSeq))
        val fvcsMap = fvcs.groupBy(_.fd.id.fullName)


        for (f <- toGenerate) {
          val vcs = fvcsMap.getOrElse(f.id.fullName, Seq()).map {
            v => (v -> (None: Option[VCResult]))
          }
          verifOverview += f -> FunVerifStatus(f, vcs.toMap)
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
