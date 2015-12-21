package leon.web
package workers

import scala.concurrent.duration._
import akka.actor._
import leon.LeonContext
import leon.purescala.{ PrinterContext, PrinterOptions, ScalaPrinter }
import leon.purescala.Common._
import leon.purescala.DefOps
import leon.purescala.Definitions.{ FunDef, ValDef }
import leon.purescala.ExprOps._
import leon.purescala.Expressions._
import leon.purescala.Types.{StringType, CaseClassType, AbstractClassType}
import leon.synthesis._
import leon.synthesis.disambiguation.ExamplesAdder
import leon.utils._
import leon.web.shared.Action
import models._
import play.api.libs.json.Json._
import leon.purescala.TypeOps

class SynthesisWorker(s: ActorRef, im: InterruptManager) extends WorkerActor(s, im) {
  import ConsoleProtocol._

  override lazy implicit val ctx = leon.Main.processOptions(List(
    "--feelinglucky",
    "--solvers=smt-cvc4"
  )).copy(interruptManager = interruptManager, reporter = reporter)
  
  var searchesState = Map[String, Seq[WebSynthesizer]]()

  def notifySynthesisOverview(cstate: CompilationState): Unit = {
    if (cstate.isCompiled) {
      val facts = for ((fname, sps) <- searchesState) yield {
        val problems = for ((synth, i) <- sps.zipWithIndex) yield {
          val ci = synth.ci
          Map(
            "description" -> toJson("Problem #"+(i+1)),
            "problem" -> toJson(ci.problem.asString),
            "line" -> toJson(ci.source.getPos.line),
            "column" -> toJson(ci.source.getPos.col),
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

      //options = options.copy(rules = options.rules diff Seq(leon.synthesis.rules.TEGIS))

      try {
        val synthesisInfos = SourceInfo.extractFromProgram(ctx, cstate.program).map {
          case ci => new WebSynthesizer(this, ctx, cstate.program, ci, options)
        }

        searchesState = synthesisInfos.groupBy(_.ci.fd.id.name)
      } catch {
        case e: Throwable =>
          notifyError("Unexpected error while gathering synthesis problems: "+e.getClass+" "+e.getMessage)
      }

      notifySynthesisOverview(cstate)

    case DoCancel =>
      sender ! Cancelled(this)

    case OnClientEvent(cstate, event) =>
      (event \ "action").as[String] match {
        case Action.getRulesToApply =>
          val fname = (event \ "fname").as[String]
          val cid   = (event \ "cid").as[Int]

          getRulesToApply(cstate, fname, cid)

        case Action.doApplyRule =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]
          val ruleId = (event \ "rid").as[Int]

          doApplyRule(cstate, fname, chooseId, ruleId)

        case Action.doSearch =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]
          
          ctx.reporter.info("State of the program at the beginning of synthesis " + cstate.program)

          doSearch(cstate, fname, chooseId)

        case Action.doExplore =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]
          val path = (event \ "path").as[List[Int]]
          val ws = (event \ "ws").as[Int]

          val action = (event \ "exploreAction").as[String] match {
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

    case CreateUpdatePrettyPrinter(cstate, fdUsingIt, in, out) =>
      val tpe = in.getType match { case cct: CaseClassType => cct.root case  e => e}
      clientLog("Doing for type " + tpe)
      // Look for an existing function which accepts this type and return a string
      val program = cstate.program
      program.definedFunctions.find { fd => fd.returnType == StringType && fd.id.name.toLowerCase().endsWith("tostring") && fd.params.length == 1 && TypeOps.isSubtypeOf(tpe, fd.params.head.getType) } match {
        case Some(fd) =>
          // Here we add an example to this function, removing the body if it existed before in order to resynthesize it.
          val allCode = leon.web.utils.FileInterfaceWeb.allCodeWhereFunDefModified(fd){ nfd =>
            nfd.body = Some(new Hole(StringType, Nil))
            new ExamplesAdder(ctx, program).addToFunDef(nfd, Seq((in, StringLiteral(out))))
          }(cstate, ctx)
          
          event("replace_code", Map(
            "newCode" -> toJson(allCode)
          ))
        case None =>
          // Here we create a new pretty printing function
          val fdUsingItOpt = fdUsingIt.orElse(program.definedFunctions.lastOption)
          fdUsingItOpt match {
            case Some(fdUsingIt) =>
              val funName3 = tpe.asString.replaceAll("[^a-zA-Z0-9_]","")
              val funName = funName3(0).toLower + funName3.substring(1, Math.min(funName3.length, 10)) 
              val newId = FreshIdentifier(funName +"ToString")
              val newArgId = FreshIdentifier("in", tpe)
              val newFd = new FunDef(newId, Seq(), Seq(ValDef(newArgId)), StringType)
              newFd.fullBody = Hole(StringType, Seq())
              new ExamplesAdder(ctx, program).addToFunDef(newFd, Seq((in, StringLiteral(out))))
              
              val allCode = leon.web.utils.FileInterfaceWeb.allCodeWhereFunDefAdded(fdUsingIt)(newFd)(cstate, ctx)
              event("replace_code", Map(
                "newCode" -> toJson(allCode)
              ))
            case None =>
              notifyError("Could not find a place where to add a toString function")
          }
          
          
      }
      
    case _ =>
  }

  import leon.synthesis.graph._

  def doExplore(cstate: CompilationState, fname: String, cid: Int, path: List[Int], ws: Int, action: ExploreAction): Unit = {
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
                  n.descendants.zipWithIndex.find { case (d, i) => i === selected } match {
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
                      if (n.selected === Nil || !n.isExpanded) {
                        Some(Solution.chooseComplete(n.p))
                      } else {
                        val subSols = n.descendants.zipWithIndex.collect {
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
                      val subSols = n.descendants.zipWithIndex.collect {
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
                  matcher.appendReplacement(result, solutionsTree(n.descendants(i), i:: path, ws)) 
                }
                matcher.appendTail(result);

                code = result.toString

                val hpath = path.reverse.mkString("-")

                n match {
                  case on: OrNode =>
                    if (!on.isExpanded) {
                      on.expand(hctx)
                    }

                    val options = on.descendants.zipWithIndex.collect { case (d: AndNode, i) =>
                      val name = if (d.isDeadEnd) {
                        d.ri.asString+" (failed)"
                      } else {
                        d.ri.asString
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
                            <span class="name">Solutions of ${an.ri.asString} (${an.selectedSolution+1}/$tot)</span>
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

  def doApplyRule(cstate: CompilationState, fname: String, cid: Int, rid: Int): Unit = {
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
            "problem" -> toJson(ScalaPrinter(synth.ci.source))
          ))

          val osol = search.traversePath(path) match {
            case Some(an: AndNode) =>
              logInfo("Applying :"+an.ri.asString)
              

              if (!an.isExpanded) {
                val hctx = SearchContext(sctx, synth.ci, an, search)
                an.expand(hctx)
              }

              an.solutions.getOrElse {
                an.composeSolutions(an.descendants.map { d =>
                  Stream(Solution.choose(d.p))
                })
              }

            case _ =>
              logInfo("Path "+path+" did not lead to an and node!")
              Stream.Empty
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
    val SourceInfo(fd, pc, src, spec, tb) = ci

    val solCode = sol.toSimplifiedExpr(synth.context, synth.program)

    val (defs, expr) = liftClosures(solCode)

    val fInt = new FileInterface(new MuteReporter())

    val nfd = fd.duplicate()

    nfd.body = nfd.body.map(b => Simplifiers.bestEffort(synth.context, synth.program)(postMap{
      case ch if ch === src && ch.getPos === src.getPos =>
        Some(expr)
      case _ =>
        None
    }(b)))

    val fds = nfd :: defs.toList.sortBy(_.id.name)

    val prog = DefOps.addFunDefs(cstate.program, fds, fd)
    val p = new ScalaPrinter(PrinterOptions(), Some(prog))

    val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                  fd,
                                  (indent) => {
      implicit val pctx = PrinterContext(fd, Nil, indent, p)
      p"${nary(fds, "\n\n")}"
      p.toString
    })

    (solCode, allCode)
  }

  def sendSolution(cstate: CompilationState, synth: Synthesizer, ssol: Stream[Solution]): Unit = {
    val fd = synth.ci.fd
    val osol = ssol.headOption

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
        
        sender ! DispatchTo(shared.Module.disambiguation, NewSolutions(cstate, synth, ssol))

      case None =>
        event("synthesis_result", Map(
          "result" -> toJson("failure"),
          "closed" -> toJson(1),
          "total" -> toJson(1)
        ))

        logInfo("Application failed!")
    }
  }

  def getRulesToApply(cstate: CompilationState, fname: String, cid: Int): Unit = {
    searchesState.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some(synth) =>
        try {
          val search = synth.search
          val orNode = search.g.root
          
          if (!orNode.isExpanded) {
            val hctx = SearchContext(synth.sctx, synth.ci, orNode, synth.search)
            orNode.expand(hctx)
          }

          val andNodes = orNode.descendants.collect {
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
                       "name" -> toJson(t.ri.asString),
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

  def doSearch(cstate: CompilationState, fname: String, cid: Int): Unit = {
    searchesState.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some(synth) =>
        try {
          val ci = synth.ci

          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "fname" -> toJson(fname),
            "cid" -> toJson(cid),
            "problem" -> toJson(ScalaPrinter(ci.source))
          ))

         val search = synth.search
         val ctx    = synth.context
         val prog   = synth.program

         val solutions = search.search(synth.sctx)
         if(interruptManager.isInterrupted) {
          val (closed, total) = search.g.getStats()

          event("synthesis_result", Map(
            "result" -> toJson("failure"),
            "closed" -> toJson(closed),
            "total" -> toJson(total)
          ))

          // We refresh all synthesis state because an abort messes up with the search
          self ! OnUpdateCode(cstate)
        } else if(solutions.isEmpty) {
          val (closed, total) = search.g.getStats()
          event("synthesis_result", Map(
            "result" -> toJson("failure"),
            "closed" -> toJson(closed),
            "total" -> toJson(total)
          ))
          notifyError("Search failed.")
          logInfo("Synthesis search failed!")
        } else {
          val sol = solutions.head
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
          val newFd = ci.fd.duplicate()
          newFd.body = newFd.body.map(b => replace(Map(ci.source -> solCode), b))

          val resFd = flattenFunctions(newFd, ctx, prog)

          val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                        oldFd,
                                        resFd)(ctx)

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
          
          sender ! DispatchTo(shared.Module.disambiguation, NewSolutions(cstate, synth, solutions))

          logInfo("Synthesis search succeeded!")
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
