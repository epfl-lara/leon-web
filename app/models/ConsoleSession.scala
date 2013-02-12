package leon.web
package models

import akka.actor._
import akka.util.duration._
import akka.dispatch.Await
import akka.dispatch.Future

import play.api._
import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.json.Json._
import play.api.libs.json.Writes._

import akka.util.Timeout
import akka.pattern.ask

import play.api.Play.current

import leon.{LeonContext, Settings, Reporter, SilentReporter}
import leon.solvers.z3.{UninterpretedZ3Solver, FairZ3Solver}
import leon.solvers.{Solver, TimeoutSolver, TrivialSolver}
import leon.plugin.{TemporaryInputPhase, ExtractionPhase}
import leon.synthesis._
import leon.synthesis.search._
import leon.synthesis.utils.SynthesisProblemExtractionPhase
import leon.verification.{AnalysisPhase,VerificationCondition}
import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.ScalaPrinter
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.xlang._

import java.util.concurrent.TimeoutException

class ConsoleSession extends Actor {
  import context.dispatcher
  import ConsoleProtocol._

  var channel: PushEnumerator[JsValue] = _
  var reporter: WSReporter = _

  def log(msg: String) = {
    channel.push(toJson(Map("kind" -> "log", "level" -> "log", "message" -> msg)))
  }

  def error(msg: String) = {
    channel.push(toJson(Map("kind" -> "log", "level" -> "error", "message" -> msg)))
  }

  def event(kind: String, data: Map[String, JsValue]) = {
    channel.push(toJson(Map("kind" -> toJson(kind)) ++ data))
  }

  var codeHistory: List[String]        = Nil
  var compilationHistory: List[String] = Nil

  var chooses: Map[Int, (ChooseInfo, SimpleSearch)] = Map()
  var currentProgram: Option[Program] = None
  var functions: Map[String, FunDef] = Map()


  case class VerificationContext(vcs: List[VerificationCondition], solvers: List[Solver])
  var verificationCtx: Option[VerificationContext] = None

  def notifyError(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("error")))
  }

  def notifySuccess(msg: String) = {
    event("notification", Map(
      "content" -> toJson(msg),
      "type"    -> toJson("success")))
  }

  def setAnnotations(as: Seq[CodeAnnotation]) {
    event("editor", Map("annotations" -> toJson(as.map(_.toJson))))
  }

  def receive = {
    case Init =>
      channel  = Enumerator.imperative[JsValue]()
      reporter = new WSReporter(channel)
      sender ! InitSuccess(channel)

    case ProcessClientEvent(event) =>
      try {
        (event \ "action").as[String] match {
          case "doUpdateCode" =>
            self ! UpdateCode((event \ "code").as[String])

          case "synthesis_getRulesToApply" =>
            val chooseLine   = (event \ "chooseLine").as[Int]
            val chooseColumn = (event \ "chooseColumn").as[Int]

            self ! SynthesisGetRulesToApply(chooseLine, chooseColumn)

          case "synthesis_doApplyRule" =>
            val chooseId = (event \ "cid").as[Int]
            val ruleId = (event \ "rid").as[Int]

            self ! SynthesisApplyRule(chooseId, ruleId)

          case "synthesis_doSearch" =>
            val chooseId = (event \ "cid").as[Int]

            self ! SynthesisSearch(chooseId)

          case "synthesis_doCancelSearch" =>
            val chooseId = (event \ "cid").as[Int]

            self ! SynthesisCancelSearch(chooseId)

          case "verification_doVerify" =>
            val fname = (event \ "fname").as[String]

            self ! VerificationDoVerify(fname)

          case "verification_doCancel" =>
            self ! VerificationDoCancelCurrent

          case action =>
            notifyError("Received unknown action: "+action)
        }
      } catch {
        case t: Throwable =>
          notifyError("Could not process event: "+t.getMessage)
      }

    case VerificationDoCancelCurrent =>
      assert(verificationCtx.isDefined, "Verification not currently running ?!?")
      verificationCtx.get.solvers.map(_.halt)


    case VerificationDone =>
      assert(verificationCtx.isDefined, "Verification not currently running ?!?")
      verificationCtx = None
      

    case VerificationDoVerify(fname: String) =>
      if (currentProgram.isDefined) {
        assert(!(verificationCtx.isDefined), "Verification currently running ?!?")

        var compContext  = leon.Main.processOptions(reporter, List("--feelinglucky", "--evalground"))

        // Generate VCs
        val vcs = AnalysisPhase.generateVerificationConditions(reporter, currentProgram.get, Set(fname))
        val solvers = List(
          new TimeoutSolver(new TrivialSolver(compContext), 10000L),
          new TimeoutSolver(new FairZ3Solver(compContext), 10000L)
        )

        solvers.map(_.setProgram(currentProgram.get))

        verificationCtx = Some(VerificationContext(vcs, solvers))

        val future = Future {
          try {
            Some(AnalysisPhase.checkVerificationConditions(reporter, solvers, vcs))
          } catch {
            case t: Throwable =>
              notifyError("Woops, verification crashed: "+t.getMessage)
              t.printStackTrace()
              None
          }
        }

        future.map {
          case Some(report) =>
            try {
              def vcToJson(vc: VerificationCondition): JsValue = {
                def ceToJson(ce: Map[Identifier, Expr]): JsValue = {
                  toJson(ce.map{ case (id, ex) =>
                    id.toString -> toJson(ScalaPrinter(ex))
                  })
                }

                val base = Map(
                  "fun" -> toJson(vc.funDef.id.toString),
                  "kind" -> toJson(vc.kind.toString),
                  "status" -> toJson(vc.status),
                  "time" -> toJson(vc.time.map("%-3.3f".format(_)).getOrElse("")))

                vc.counterExample match {
                  case Some(ce) =>
                    toJson(base + ("counterExample" -> ceToJson(ce)))
                  case None =>
                    toJson(base)
                }
              }

              val status = if (report.totalInvalid > 0) {
                "failure"
              } else if (report.totalUnknown > 0) {
                "failure"
              } else {
                "success"
              }

              self ! VerificationDone
              event("verification_result", Map("status" -> toJson(status), "vcs" -> toJson(report.conditions.map(vcToJson))))
            } catch {
              case t: Throwable =>
              self ! VerificationDone
                notifyError("Woops, solution processing crashed: "+t.getMessage)
                event("verification_result", Map("status" -> toJson("failure"), "vcs" -> toJson(List[String]())))
                t.printStackTrace()
            }
          case None =>
            self ! VerificationDone
            event("verification_result", Map("status" -> toJson("failure"), "vcs" -> toJson(List[String]())))
        }
      } else {
        event("verification_result", Map("status" -> toJson("failure")))
        notifyError("No program available. Compilation failed?")
      }

    case UpdateCode(code) =>
      if (codeHistory.headOption != Some(code)) {
        log("Compiling...")
        codeHistory = code :: codeHistory

        val compReporter = new CompilingWSReporter(channel)
        var compContext  = leon.Main.processOptions(compReporter, Nil)
        var synthContext = compContext.copy(reporter = reporter)

        val pipeline = TemporaryInputPhase andThen ExtractionPhase andThen FunctionClosure

        try {
          val (prog, _, _) = pipeline.run(compContext)((code, Nil))
          currentProgram = Some(prog)

          def noop(u:Expr, u2: Expr) = u

          var options = SynthesisOptions()
          val ctx = new LeonContext()

          // Extract Synthesis Problems
          chooses = ChooseInfo.extractFromProgram(synthContext, prog, options).zipWithIndex.map {
            case (ci, i) =>
              val search = new SimpleWebSearch(this, ci.synthesizer, ci.problem)
              (i+1) -> (ci, search)
          }.toMap

          // Extract Verification Problems
          if (chooses.isEmpty) {
            functions = prog.definedFunctions.filter(fd => fd.hasBody).map(fd => fd.id.name -> fd).toMap
          } else {
            functions = Map()
          }

          event("compilation", Map("status" -> toJson("success")))

          val synthesisAnnotations = chooses.values.map { case (ci, _) =>
            val (line, col) = ci.ch.posIntInfo
            CodeAnnotation(line, col, "Synthesis problem: "+ci.problem.toString, CodeAnnotationSynthesis)
          }

          val verificationAnnotations = functions.values.map { case fd =>
            val (line, col) = fd.posIntInfo
            CodeAnnotation(line, col, "Verifiable function", CodeAnnotationVerification)
          }
          setAnnotations((synthesisAnnotations ++ verificationAnnotations).toSeq)

          log("Compilation successful!")

          compilationHistory = "success" :: compilationHistory

        } catch {
          case t: Throwable =>
            chooses = Map()
            functions = Map()
            currentProgram = None

            event("compilation", Map("status" -> toJson("failure")))

            setAnnotations(compReporter.errors.map{ case (l,e) =>
              CodeAnnotation(l, 0, e.mkString("\n"), CodeAnnotationError)
            }.toSeq)

            compilationHistory = "failure" :: compilationHistory

            log("Compilation failed!")
            t.printStackTrace
        }
      } else {
        compilationHistory.headOption.foreach { status =>
          event("compilation", Map("status" -> toJson(status)))
        }
      }

    case Quit =>

    case SynthesisCancelSearch(cid) =>
      chooses.get(cid) match {
        case Some((ci, search)) =>
          ci.synthesizer.shouldStop.set(true)
          search.stop()
        case None =>
      }

    case SynthesisSearch(cid) =>
      try {
        chooses.get(cid) match {
          case Some((ci @ ChooseInfo(ctx, prog, fd, _, ch, _), search)) =>

            val future = Future {
              try {
                ci.synthesizer.shouldStop.set(false)
                val sol = search.search()
                sol
              } catch {
                case t: Throwable =>
                  notifyError("Woops, search crashed: "+t.getMessage)
                  t.printStackTrace()
                  None
              }
            }

            future.map {
              case Some(sol) =>
                try {
                  val chToSol = Map(ci -> sol.toSimplifiedExpr(ctx, prog))
                  val fInt = new FileInterface(reporter)

                  val newCode = fInt.substitueChooses(codeHistory.head, chToSol, true)

                  event("synthesis_replace_code", Map("cid" -> toJson(cid),
                                                      "newCode" -> toJson(newCode)))

                  event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("success")))

                  notifySuccess("Search succeeded!")
                } catch {
                  case t: Throwable =>
                    notifyError("Woops, solution processing crashed: "+t.getMessage)
                    t.printStackTrace()
                }

              case None =>
                event("synthesis_search", Map("action" -> toJson("result"), "status" -> toJson("failure")))

                if (search.shouldStop) {
                  notifyError("Search cancelled")
                } else {
                  notifyError("Search failed to find a solution")
                }

            }
          case None =>
        }
      } catch {
        case t: Throwable =>
          notifyError("Woops, I crashed: "+t.getMessage())
          t.printStackTrace();
      }


    case SynthesisApplyRule(cid, rid) =>
      try {
        val path = List(rid)
        chooses.get(cid) match {
          case Some((ci @ ChooseInfo(ctx, prog, fd, pc, ch, _), search)) =>
            search.traversePath(path) match {
              case Some(al: search.g.AndLeaf) =>
                val res = search.expandAndTask(al.task)
                search.onExpansion(al, res)

                val solution = res match {
                  case es: search.ExpandSuccess[_] =>
                    // Solved
                    Some(es.sol)
                  case ex: search.Expanded[_] =>
                    // Node has been updated
                    search.traversePath(path) match {
                      case Some(an: search.g.AndNode) =>
                        an.task.composeSolution(an.subTasks.map(t => Solution.choose(t.p)))
                      case _ =>
                        None
                    }
                  case _ =>
                    None
                }

                solution match {
                  case Some(sol) =>
                    val chToSol = Map(ci -> sol.toSimplifiedExpr(ctx, prog))
                    val fInt = new FileInterface(reporter)

                    val newCode = fInt.substitueChooses(codeHistory.head, chToSol, true)

                    event("synthesis_replace_code", Map("cid" -> toJson(cid),
                                                        "newCode" -> toJson(newCode)))

                    notifySuccess("Successfully applied "+al.task.app.toString)

                  case None =>
                    notifyError("Failed to apply "+al.task.toString)

                }
              case _ =>
                error("Woops, choose not found..")
            }
          case _ =>
            error("Woops, choose not found..")
        }
      } catch {
        case t: Throwable =>
          notifyError("Woops, I crashed: "+t.getMessage())
          t.printStackTrace();
      }

    case SynthesisGetRulesToApply(chooseLine, chooseColumn) =>
      try {
        chooses.find(_._2._1.ch.posIntInfo == (chooseLine, chooseColumn)) match {
          case Some((i, (ci, search))) =>
            val orNode = search.g.tree match {
              case ol: search.g.OrLeaf =>
                val sub = search.expandOrTask(ol.task)
                search.onExpansion(ol, sub)

                // It was updated
                search.g.tree.asInstanceOf[search.g.OrNode]

              case on: search.g.OrNode =>
                on
            }

            val rulesApps = for ((t, i) <- orNode.altTasks.zipWithIndex) yield {
              val status = if (orNode.triedAlternatives contains t) {
                "closed"
              } else {
                "open"
              }

              toJson(Map("id" -> toJson(i),
                         "name" -> toJson(t.app.toString),
                         "status" -> toJson(status)))
            }

            event("synthesis_choose_rules", Map("chooseLine"   -> toJson(chooseLine),
                                                "chooseColumn" -> toJson(chooseColumn),
                                                "cid"          -> toJson(i),
                                                "rulesApps"    -> toJson(rulesApps)))

          case None =>
            error("Woops, choose not found..")
        }

      } catch {
        case t: Throwable =>
          notifyError("Woops, I crashed: "+t.getMessage())
          t.printStackTrace();
      }

    case msg =>
      log("Unknown Actor Message: "+msg)
  }
}
