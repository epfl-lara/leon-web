package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.purescala.ScalaPrinter
import leon.synthesis._
import leon.purescala.Common._
import leon.purescala.TreeOps._
import leon.purescala.Definitions._

class SynthesisWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._

  var choosesInfo = Map[String, Seq[(ChooseInfo, SimpleWebSearch)]]()

  def notifySynthesisOverview(cstate: CompilationState) {
    if (cstate.isCompiled) {
      val facts = for ((fname, sps) <- choosesInfo) yield {
        val problems = for (((ci, search), i) <- sps.zipWithIndex) yield {
          Map(
            "description" -> toJson("Problem #"+(i+1)),
            "problem" -> toJson(ci.problem.toString),
            "line" -> toJson(ci.ch.getPos.line),
            "column" -> toJson(ci.ch.getPos.col),
            "index" -> toJson(i)
          )
        }
        fname -> toJson(problems)
      }

      event("update_synthesis_overview", Map("functions" -> toJson(facts)))
    }

  }

  def receive = {
    case OnUpdateCode(cstate) =>
      var options = SynthesisOptions().copy(cegisGenerateFunCalls = true)
      val reporter = new WorkerReporter(session)
      var context = leon.Main.processOptions(Nil).copy(interruptManager = interruptManager, reporter = reporter)

      choosesInfo = ChooseInfo.extractFromProgram(context, cstate.program, options).map {
        case ci =>
          val search = new SimpleWebSearch(this, ci.synthesizer, ci.problem)
          (ci, search)
      }.groupBy(_._1.fd.id.name)

      notifySynthesisOverview(cstate)

    case DoCancel =>
      sender ! Cancelled(this)

    case OnClientEvent(cstate, event) =>
      (event \ "action").as[String] match {
        case "getRulesToApply" =>
          val fname = (event \ "fname").as[String]
          val cid   = (event \ "cid").as[Int]

          getRulesToApply(cstate, fname, cid)

        case "doApplyRule" =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]
          val ruleId = (event \ "rid").as[Int]

          doApplyRule(cstate, fname, chooseId, ruleId)

        case "doSearch" =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]

          doSearch(cstate, fname, chooseId)
      }

    case _ =>
  }

  def doApplyRule(cstate: CompilationState, fname: String, cid: Int, rid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci @ ChooseInfo(ctx, prog, fd, pc, ch, _), search)) =>
        try {
          val path = List(rid)

          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "problem" -> toJson(ScalaPrinter(ci.ch))
          ))

          val solution: Option[Solution] = search.traversePath(path) match {
            case Some(an: search.g.AndNode) =>
              logInfo("Applying :"+an.task.app.toString)


              an.task.composeSolution(an.subTasks.map(t => Solution.choose(t.p)))

            case Some(al: search.g.AndLeaf) =>
              logInfo("Applying :"+al.task.app.toString)

              val res = search.expandAndTask(al.task)
              search.onExpansion(al, res)

              res match {
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

            case _ =>
              logInfo("Path "+path+" did not lead to AndLeaf!")
              throw new Exception("WWOT")
          }


          solution match {
            case Some(sol) =>
              val solCode = sol.toSimplifiedExpr(ctx, prog)
              val fInt = new FileInterface(new MuteReporter())

              val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                            ci.ch,
                                            solCode)

              val (closed, total) = search.g.getStatus

              event("synthesis_result", Map(
                "result" -> toJson("success"),
                "solCode" -> toJson(ScalaPrinter(solCode)),
                "allCode" -> toJson(allCode),
                "closed" -> toJson(1),
                "total" -> toJson(1)
              ))
              logInfo("Application successful!")

            case None =>
              event("synthesis_result", Map(
                "result" -> toJson("failure"),
                "closed" -> toJson(1),
                "total" -> toJson(1)
              ))
              logInfo("Application failed!")
          }
        } catch {
          case t: Throwable =>
            notifyError("Internal error :(")
            logInfo("Synthesis Rule Application crashed", t)
        }
      case None =>
        notifyError("Can't find synthesis problem "+fname+"["+cid+"]")
    }
  }

  def getRulesToApply(cstate: CompilationState, fname: String, cid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci, search)) =>
        try {
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

          event("synthesis_rulesToApply", Map("fname"     -> toJson(fname),
                                              "cid"       -> toJson(cid),
                                              "rulesApps" -> toJson(rulesApps)))

        } catch {
          case t: Throwable =>
            notifyError("Woops, I crashed: "+t.getMessage())
            t.printStackTrace()
            logInfo("Synthesis RulesList crashed", t)
        }
      case None =>
        notifyError("Can't find synthesis problem "+fname+"["+cid+"]")
    }
  }

  def doSearch(cstate: CompilationState, fname: String, cid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci @ ChooseInfo(ctx, prog, fd, _, ch, _), search)) =>
        try {
          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "problem" -> toJson(ScalaPrinter(ci.ch))
          ))

          search.search() match {
            case _ if interruptManager.isInterrupted =>
              val (closed, total) = search.g.getStatus

              event("synthesis_result", Map(
                "result" -> toJson("failure"),
                "closed" -> toJson(closed),
                "total" -> toJson(total)
              ))

              // We refresh all synthesis state because an abort messes up with the search
              self ! OnUpdateCode(cstate)

            case Some((sol, isTrusted)) =>
              val (newSol, succeeded) = if (!isTrusted) {
                // Validate solution
                event("synthesis_proof", Map("status" -> toJson("init")))
                ci.synthesizer.validateSolution(search, sol, 2000L) match {
                  case (sol, true) =>
                    event("synthesis_proof", Map("status" -> toJson("success")))
                    (sol, true)
                  case (sol, false) =>
                    event("synthesis_proof", Map("status" -> toJson("failure")))
                    (sol, false)
                }
              } else {
                (sol, true)
              }

              val solCode = newSol.toSimplifiedExpr(ctx, prog)
              val fInt = new FileInterface(new MuteReporter())


              val oldFd = ci.fd
              val newFd = ci.fd.duplicate
              newFd.body = newFd.body.map(b => replace(Map(ci.ch -> solCode), b))

              val resFd = flattenFunctions(newFd)
              println(ScalaPrinter(resFd))

              val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                            oldFd,
                                            resFd)

              val (closed, total) = search.g.getStatus

              event("synthesis_result", Map(
                "result" -> toJson("success"),
                "proven" -> toJson(succeeded),
                "solCode" -> toJson(ScalaPrinter(solCode)),
                "allCode" -> toJson(allCode),
                "closed" -> toJson(closed),
                "total" -> toJson(total)
              ))

              logInfo("Synthesis search succeeded!")
            case None =>
              val (closed, total) = search.g.getStatus

              event("synthesis_result", Map(
                "result" -> toJson("failure"),
                "closed" -> toJson(closed),
                "total" -> toJson(total)
              ))

              notifyError("Search failed.")
              logInfo("Synthesis search failed!")
          }
        } catch {
          case t: Throwable =>
            notifyError("Woops, search crashed: "+t.getMessage)
            logInfo("Synthesis search crashed", t)
        }
      case None =>
        notifyError("Can't find synthesis problem "+fname+"["+cid+"]")
    }
  }
}
