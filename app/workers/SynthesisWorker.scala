package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._

import models._
import leon.utils._
import leon.purescala.PrinterOptions
import leon.purescala.PrinterContext
import leon.purescala.ScalaPrinter
import leon.synthesis._
import leon.purescala.Common._
import leon.purescala.TreeOps._
import leon.purescala.Trees._
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
  abstract class ExploreAction;
  case class ExploreSelect(selected: Int) extends ExploreAction
  case object ExploreNextSolution extends ExploreAction
  case object ExplorePreviousSolution extends ExploreAction
  case object ExploreAsChoose extends ExploreAction
  case object ExploreNoop extends ExploreAction

  def receive = {
    case OnUpdateCode(cstate) =>
      var options = SynthesisOptions().copy(cegisGenerateFunCalls = true)
      val reporter = new WorkerReporter(session)
      var context = leon.Main.processOptions(Nil).copy(interruptManager = interruptManager, reporter = reporter)

      choosesInfo = ChooseInfo.extractFromProgram(context, cstate.program, options).map {
        case ci =>
          val search = new SimpleWebSearch(this, context, ci.problem)
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

        case "doExplore" =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]
          val path = (event \ "path").as[List[Int]]
          val ws = (event \ "ws").as[Int]

          val action = (event \ "explore-action").as[String] match {
            case "select-alternative" =>
              val s = (event \ "select").as[Int]
              if (s < 0) {
                ExploreAsChoose
              } else {
                ExploreSelect(s)
              }
            case "next-solution" =>
              ExploreNextSolution
            case "previous-solution" =>
              ExplorePreviousSolution
            case "init" =>
              ExploreNoop
          }

          doExplore(cstate, fname, chooseId, path, ws, action)
      }

    case _ =>
  }

  def doExplore(cstate: CompilationState, fname: String, cid: Int, path: List[Int], ws: Int, action: ExploreAction) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci @ ChooseInfo(ctx, prog, fd, pc, src, ch, sopts), search)) =>
        import search.g.{OrNode, AndNode, Node}

        val sctx = SynthesisContext(ctx, sopts, fd, prog, ctx.reporter)
        val simplifier = Simplifiers.namePreservingBestEffort(ctx, prog) _

        try {
          search.traversePath(path) match {
            case Some(n) =>
              if (!n.isExpanded) {
                n.expand(sctx)
              }

              action match {
                case ExploreAsChoose =>
                  n.selected = Nil
                  // as choose

                case ExploreSelect(selected) =>
                  n.descendents.zipWithIndex.find { case (d, i) => i == selected } match {
                    case Some((d, _)) =>
                      n.selected = List(d)
                      if (!d.isExpanded) {
                        d.expand(sctx)
                      }
                    case _ =>
                      notifyError("Not found")
                  }

                case ExplorePreviousSolution =>
                  n.solutions match {
                    case Some(sols) if sols.isDefinedAt(n.selectedSolution - 1) =>
                      n.selectedSolution -= 1
                    case _ =>
                      notifyError("No more solutions..")
                  }

                case ExploreNextSolution =>
                  n.solutions match {
                    case Some(sols) if sols.isDefinedAt(n.selectedSolution + 1) =>
                      n.selectedSolution += 1
                    case _ =>
                      notifyError("No more solutions..")
                  }
                case _ =>

              }

              def solutionOf(n: Node): Option[Solution] = {
                n.solutions.flatMap ( sols => 
                  if (sols.isDefinedAt(n.selectedSolution)) {
                    Some(sols(n.selectedSolution))
                  } else {
                    None
                  }).orElse {
                    if (n.isClosed) {
                      Some(Solution.failed(n.p))
                    } else {
                      if (n.selected == Nil || !n.isExpanded) {
                        Some(Solution.choose(n.p))
                      } else {
                        val subSols = n.descendents.zipWithIndex.collect {
                          case (d, i) if n.selected contains d =>
                            solutionOf(d).toStream
                        }
                        n.composeSolutions(subSols).headOption
                      }
                    }
                  }
              }


              def solutionsTree(n: Node, path: List[Int], ws: Int): String = {

                val osol = n.solutions.flatMap ( sols => 
                  if (sols.isDefinedAt(n.selectedSolution)) {
                    Some(sols(n.selectedSolution))
                  } else {
                    None
                  }).orElse {
                    if (n.isClosed) {
                      Some(Solution.failed(n.p))
                    } else {
                      val subSols = n.descendents.zipWithIndex.collect {
                        case (d, i) if n.selected contains d =>
                          Stream(Solution(BooleanLiteral(true), Set(), FreshIdentifier("@"+i).toVariable))
                      }
                      n.composeSolutions(subSols).headOption
                    }
                  }
                

                val sol = simplifier(osol.getOrElse(Solution.choose(n.p)).toExpr)

                var code = ScalaPrinter(sol)

                val result = new StringBuffer()

                import java.util.regex._
                val pattern = Pattern.compile("( *)@(\\d+)")

                val matcher = pattern.matcher(code)
                while (matcher.find()) {
                  val ws = matcher.group(1).size
                  val i = matcher.group(2).toInt
                  matcher.appendReplacement(result, solutionsTree(n.descendents(i), i:: path, ws)) 
                }
                matcher.appendTail(result);

                code = result.toString

                val hpath = path.reverse.mkString("-")

                n match {
                  case on: OrNode =>
                    if (!on.isExpanded) {
                      on.expand(sctx)
                    }

                    val options = on.descendents.zipWithIndex.map { case (d: AndNode, i) =>
                      val name = if (d.isClosed) {
                        d.ri+" (failed)"
                      } else {
                        d.ri
                      }
                      if (on.selected contains d) {
                        s"""<option value="$i" selected="selected">$name</option>"""
                      } else {
                        s"""<option value="$i">$name</option>"""
                      }
                    }.mkString

                    s"""<pre class="code prettyprint exploreBlock lang-scala" path="$hpath" ws="$ws" style="margin-left: ${ws}ch"><span class="header">
                        <select class="knob" data-action="select-alternative">
                          <option value="-1">As choose</option>
                          $options
                        </select>
                        </span>$code</pre>"""
                  case an: AndNode if an.solutions.isDefined =>
                    val sols = an.solutions.get
                    if (sols.hasDefiniteSize && sols.size <= 1) {
                      code
                    } else {
                      val tot = if (sols.hasDefiniteSize) sols.size else "?"

                      s"""<pre class="code prettyprint exploreBlock lang-scala" path="$hpath" ws="$ws" style="margin-left: ${ws}ch"><span class="header">
                            <span class="knob fa fa-arrow-left" data-action="previous-solution"></span>
                            <span class="knob fa fa-arrow-right" data-action="next-solution"></span>
                            <span class="name">Solutions of ${an.ri.toString} (${an.selectedSolution+1}/$tot)</span>
                          </span>$code</pre>"""
                    }
                  case _ =>
                    code
                }
              }

              val allSol  = solutionOf(search.g.root)
              val (_, allCode) = solutionCode(cstate, ci, allSol.getOrElse(Solution.failed(ci.problem)))

              event("synthesis_exploration", Map(
                "from"  -> toJson(path),
                "fname" -> toJson(fname),
                "cid"   -> toJson(cid),
                "html"  -> toJson(solutionsTree(n, path.reverse, ws)),
                "allCode" -> toJson(allCode)
              ))

            case None =>
              notifyError("Woot!?!")
          }

        } catch {
          case e: Throwable =>
            e.printStackTrace
            notifyError("Woot: "+e.getMessage)
        }
      case _ =>
        notifyError("Can't find synthesis problem "+fname+"["+cid+"]")
    }
  }

  def doApplyRule(cstate: CompilationState, fname: String, cid: Int, rid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci @ ChooseInfo(ctx, prog, fd, pc, src, ch, sopts), search)) =>
        try {
          val sctx = SynthesisContext(ctx, sopts, fd, prog, ctx.reporter)
          val path = List(rid)

          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "fname" -> toJson(fname),
            "cid" -> toJson(cid),
            "problem" -> toJson(ScalaPrinter(ci.ch))
          ))

          val osol = search.traversePath(path) match {
            case Some(an: search.g.AndNode) =>
              logInfo("Applying :"+an.ri.toString)
              
              if (!an.isExpanded) {
                an.expand(sctx)
              }

              an.solutions.getOrElse {
                an.composeSolutions(an.descendents.map { d =>
                  Stream(Solution.choose(d.p))
                })
              }.headOption

            case _ =>
              logInfo("Path "+path+" did not lead to an and node!")
              None
          }

          sendSolution(cstate, ci, osol)

        } catch {
          case t: Throwable =>
            event("synthesis_result", Map(
              "result" -> toJson("failure"),
              "closed" -> toJson(1),
              "total" -> toJson(1)
            ))

            notifyError("Internal error :(")
            logInfo("Synthesis Rule Application crashed", t)
        }
      case None =>
        notifyError("Can't find synthesis problem "+fname+"["+cid+"]")
    }
  }

  def solutionCode(cstate: CompilationState, ci: ChooseInfo, sol: Solution): (Expr, String) = {
    import leon.purescala.PrinterHelpers._

    val ChooseInfo(ctx, prog, fd, pc, src, ch, _) = ci

    val solCode = sol.toSimplifiedExpr(ctx, prog)

    val (defs, expr) = liftClosures(solCode)

    val fInt = new FileInterface(new MuteReporter())

    val nfd = fd.duplicate

    nfd.body = nfd.body.map(b => Simplifiers.bestEffort(ctx, prog)(postMap{
      case ch if ch == ci.ch && ch.getPos == ci.ch.getPos =>
        Some(expr)
      case _ =>
        None
    }(b)))

    val fds = nfd :: defs.toList.sortBy(_.id.name)

    val p = new ScalaPrinter(PrinterOptions())

    val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                  fd,
                                  (indent) => {
      implicit val pctx = PrinterContext(fd, None, None, indent, p)
      p"${nary(fds, "\n\n")}"
      p.toString
    })

    (solCode, allCode)
  }

  def sendSolution(cstate: CompilationState, ci: ChooseInfo, osol: Option[Solution]) {
    val ChooseInfo(ctx, prog, fd, pc, src, ch, _) = ci

    osol match {
      case Some(sol) =>

        val (solCode, allCode) = solutionCode(cstate, ci, sol)

        event("synthesis_result", Map(
          "result" -> toJson("success"),
          "solCode" -> toJson(ScalaPrinter(solCode)),
          "cursor" -> toJson(Map(
            "line"   -> fd.getPos.line,
            "column" -> (fd.getPos.col-1)
          )),
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
  }

  def getRulesToApply(cstate: CompilationState, fname: String, cid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci, search)) =>
        try {
          val sctx = SynthesisContext(ci.ctx, ci.options, ci.fd, ci.prog, ci.ctx.reporter)
          val orNode = search.g.root
          
          if (!orNode.isExpanded) {
            orNode.expand(sctx)
          }

          val andNodes = orNode.descendents.collect {
            case n: search.g.AndNode =>
              n
          }

          val rulesApps = for ((t, i) <- andNodes.zipWithIndex) yield {
            val status = if (t.isClosed) {
              "closed"
            } else {
              "open"
            }

            toJson(Map("id" -> toJson(i),
                       "name" -> toJson(t.ri.toString),
                       "status" -> toJson(status)))
          }

          event("synthesis_rulesToApply", Map("fname"     -> toJson(fname),
                                              "cid"       -> toJson(cid),
                                              "rulesApps" -> toJson(rulesApps)))

        } catch {
          case t: Throwable =>
            event("synthesis_rulesToApply", Map("fname"     -> toJson(fname),
                                                "cid"       -> toJson(cid),
                                                "rulesApps" -> toJson(Seq[String]())))
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
      case Some((ci @ ChooseInfo(ctx, prog, fd, _, src, ch, _), search)) =>
        try {
          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "fname" -> toJson(fname),
            "cid" -> toJson(cid),
            "problem" -> toJson(ScalaPrinter(ci.ch))
          ))

          val sctx = SynthesisContext(ci.ctx, ci.options, ci.fd, ci.prog, ci.ctx.reporter)

          search.search(sctx) match {
            case _ if interruptManager.isInterrupted =>
              val (closed, total) = search.g.getStats()

              event("synthesis_result", Map(
                "result" -> toJson("failure"),
                "closed" -> toJson(closed),
                "total" -> toJson(total)
              ))

              // We refresh all synthesis state because an abort messes up with the search
              self ! OnUpdateCode(cstate)

            case sol #:: _ =>
              val (newSol, succeeded) = if (!sol.isTrusted) {
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
              newFd.body = newFd.body.map(b => replace(Map(src -> solCode), b))

              val resFd = flattenFunctions(newFd, ctx, prog)
              println(ScalaPrinter(resFd))

              val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                            oldFd,
                                            resFd,
                                            oldFd.owner.get)

              val (closed, total) = search.g.getStats()

              event("synthesis_result", Map(
                "result" -> toJson("success"),
                "proven" -> toJson(succeeded),
                "solCode" -> toJson(ScalaPrinter(solCode)),
                "allCode" -> toJson(allCode),
                "cursor" -> toJson(Map(
                  "line"   -> oldFd.getPos.line,
                  "column" -> (oldFd.getPos.col-1)
                )),
                "closed" -> toJson(closed),
                "total" -> toJson(total)
              ))

              logInfo("Synthesis search succeeded!")

            case _ =>
              val (closed, total) = search.g.getStats()

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
