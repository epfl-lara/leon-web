package leon.web
package workers

import akka.actor._
import play.api.libs.json._
import play.api.libs.json.Json._
import scala.concurrent.duration._

import models._
import leon.LeonContext
import leon.utils._
import leon.purescala.PrinterOptions
import leon.purescala.PrinterContext
import leon.purescala.ScalaPrinter
import leon.synthesis._
import leon.purescala.Common._
import leon.purescala.ExprOps._
import leon.purescala.Expressions._
import leon.purescala.Definitions._

class SynthesisWorker(val session: ActorRef, interruptManager: InterruptManager) extends Actor with WorkerActor {
  import ConsoleProtocol._

  var searchesState = Map[String, Seq[WebSynthesizer]]()

  def notifySynthesisOverview(cstate: CompilationState) {
    if (cstate.isCompiled) {
      val facts = for ((fname, sps) <- searchesState) yield {
        val problems = for ((synth, i) <- sps.zipWithIndex) yield {
          val ci = synth.ci
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
      var options = SynthesisSettings()
      val reporter = new WorkerReporter(session)
      var context = leon.Main.processOptions(Nil).copy(interruptManager = interruptManager, reporter = reporter)


      val synthesisInfos = ChooseInfo.extractFromProgram(cstate.program).map {
        case ci => new WebSynthesizer(this, context, cstate.program, ci, options)
      }

      searchesState = synthesisInfos.groupBy(_.ci.fd.id.name)

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

  import leon.synthesis.graph._

  def doExplore(cstate: CompilationState, fname: String, cid: Int, path: List[Int], ws: Int, action: ExploreAction) {
    searchesState.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some(synth) =>
        val search     = synth.getSearch()
        val simplifier = Simplifiers.namePreservingBestEffort(synth.context, synth.program) _

        try {
          search.traversePath(path) match {
            case Some(n) =>
              val hctx = SearchContext(synth.sctx, synth.ci, n, search)

              if (!n.isExpanded) {
                n.expand(hctx)
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
                        d.expand(hctx)
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
                    if (n.isDeadEnd) {
                      Some(Solution.failed(n.p))
                    } else {
                      if (n.selected == Nil || !n.isExpanded) {
                        Some(Solution.chooseComplete(n.p))
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

                val hctx = SearchContext(synth.sctx, synth.ci, n, search)

                val osol = n.solutions.flatMap ( sols => 
                  if (sols.isDefinedAt(n.selectedSolution)) {
                    Some(sols(n.selectedSolution))
                  } else {
                    None
                  }).orElse {
                    if (n.isDeadEnd) {
                      Some(Solution.failed(n.p))
                    } else {
                      val subSols = n.descendents.zipWithIndex.collect {
                        case (d, i) if n.selected contains d =>
                          Stream(Solution(BooleanLiteral(true), Set(), FreshIdentifier("@"+i).toVariable))
                      }
                      n.composeSolutions(subSols).headOption
                    }
                  }
                

                val sol = simplifier(osol.getOrElse(Solution.chooseComplete(n.p)).toExpr)

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
                      on.expand(hctx)
                    }

                    val options = on.descendents.zipWithIndex.collect { case (d: AndNode, i) =>
                      val name = if (d.isDeadEnd) {
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
              val (_, allCode) = solutionCode(cstate, synth, allSol.getOrElse(Solution.failed(synth.problem)))

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
    searchesState.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some(synth) =>
        try {
          val sctx = synth.sctx
          val search = synth.getSearch()
          val path = List(rid)

          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "fname" -> toJson(fname),
            "cid" -> toJson(cid),
            "problem" -> toJson(ScalaPrinter(synth.ci.ch))
          ))

          val osol = search.traversePath(path) match {
            case Some(an: AndNode) =>
              logInfo("Applying :"+an.ri.toString)
              

              if (!an.isExpanded) {
                val hctx = SearchContext(sctx, synth.ci, an, search)
                an.expand(hctx)
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

          sendSolution(cstate, synth, osol)

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

  def solutionCode(cstate: CompilationState, synth: Synthesizer, sol: Solution): (Expr, String) = {
    import leon.purescala.PrinterHelpers._

    val ci = synth.ci
    val ChooseInfo(fd, pc, src, ch) = ci

    val solCode = sol.toSimplifiedExpr(synth.context, synth.program)

    val (defs, expr) = liftClosures(solCode)

    val fInt = new FileInterface(new MuteReporter())

    val nfd = fd.duplicate

    nfd.body = nfd.body.map(b => Simplifiers.bestEffort(synth.context, synth.program)(postMap{
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

  def sendSolution(cstate: CompilationState, synth: Synthesizer, osol: Option[Solution]) {
    val fd = synth.ci.fd

    osol match {
      case Some(sol) =>

        val (solCode, allCode) = solutionCode(cstate, synth, sol)

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
    searchesState.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some(synth) =>
        try {
          val search = synth.search
          val orNode = search.g.root
          
          if (!orNode.isExpanded) {
            val hctx = SearchContext(synth.sctx, synth.ci, orNode, synth.search)
            orNode.expand(hctx)
          }

          val andNodes = orNode.descendents.collect {
            case n: AndNode =>
              n
          }

          val rulesApps = for ((t, i) <- andNodes.zipWithIndex) yield {
            val status = if (t.isDeadEnd) {
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
    searchesState.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some(synth) =>
        try {
          val ci = synth.ci

          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "fname" -> toJson(fname),
            "cid" -> toJson(cid),
            "problem" -> toJson(ScalaPrinter(ci.ch))
          ))

         val search = synth.search
         val ctx    = synth.context
         val prog   = synth.program

         search.search(synth.sctx) match {
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
                synth.validateSolution(search, sol, 2.seconds) match {
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
              newFd.body = newFd.body.map(b => replace(Map(ci.source -> solCode), b))

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
