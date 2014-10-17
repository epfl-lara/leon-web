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

        case "doNextSolution" =>
          getNextSolution(cstate)

        case "doSearch" =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]

          doSearch(cstate, fname, chooseId)

        case "doExplore" =>
          val fname = (event \ "fname").as[String]
          val chooseId = (event \ "cid").as[Int]
          val offset = (event \ "offset").as[Int]
          val path = (event \ "path").as[List[Int]]

          doExploreNext(cstate, fname, chooseId, path, offset)
      }

    case _ =>
  }

  var savedCI: Option[ChooseInfo] = None
  var savedSolutions: Stream[Solution] = Stream.empty

  def doExploreNext(cstate: CompilationState, fname: String, cid: Int, path: List[Int], offset: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci @ ChooseInfo(ctx, prog, fd, pc, src, ch, sopts), search)) =>
        import search.g.{OrNode, AndNode, Node}

        val sctx = SynthesisContext(ctx, sopts, Some(fd), prog, ctx.reporter)

        try {
          search.traversePath(path) match {
            case Some(n) =>
              if (!n.isExpanded) {
                n.expand(sctx)
              }

              if (offset != 0) {
                if (n.solutions.isDefined) {
                  if (n.solutions.get.isDefinedAt(n.selectedSolution + offset)) {
                    n.selectedSolution += offset                  
                  }
                } else {
                  n.descendents.zipWithIndex.find { case(d, i) => n.selected contains d } match {
                    case Some((d, i)) =>
                      var continue = true;
                      var ni = i + offset;
                      while (continue) {
                        continue = false;

                        if (n.descendents.isDefinedAt(ni)) {
                          val nd = n.descendents(ni)

                          if (!nd.isExpanded) {
                            nd.expand(sctx)
                            if (nd.isClosed) {
                              continue = true;
                              ni += offset
                            }
                          }

                          if (!nd.isClosed) {
                            n.selected = List(nd);
                          }
                        }
                      }
                    case None =>
                      println("Woot")
                      //woot
                  }
                }
              }

              def solutionsTree(n: Node, path: List[Int]): String = {

                val osol = n.solutions.flatMap ( sols => 
                  if (sols.isDefinedAt(n.selectedSolution)) {
                    Some(sols(n.selectedSolution))
                  } else {
                    None
                  }).orElse {
                    if (n.isClosed) {
                      Some(Solution(BooleanLiteral(true), Set(), Error("Failed")))
                    } else {
                      val subSols = n.descendents.zipWithIndex.collect {
                        case (d, i) if n.selected contains d =>
                          Stream(Solution(BooleanLiteral(true), Set(), FreshIdentifier("@"+i).toVariable))
                      }
                      n.composeSolutions(subSols).headOption
                    }
                  }
                

                val sol = osol.getOrElse(Solution.choose(n.p)).toSimplifiedExpr(ctx, prog)

                var code = ScalaPrinter(sol)

                n.descendents.zipWithIndex.map{ case (d, i) =>
                  if (n.selected contains d) {
                    code = code.replaceAll("@"+i, solutionsTree(d, i:: path)) 
                  }
                }

                val hpath = path.reverse.mkString("-")

                def block(name: String, code: String) = {
                  s"""<pre class="code prettyprint exploreBlock lang-scala" path="$hpath"><span class="header">
                        <span class="knob fa fa-caret-left" offset="-1"></span>
                        <span class="knob fa fa-caret-right" offset="1"></span>
                        <span class="name">$name</span>
                      </span>$code</pre>"""
                }
                n match {
                  case on: OrNode =>
                    on.selected match {
                      case List(an: AndNode) =>
                        block(an.ri.toString, code)
                      case _ =>
                        block("?", code)
                    }
                  case an: AndNode if an.solutions.isDefined =>
                    block("Solutions of "+an.ri, code)
                  case _ =>
                    code
                }
              }

              event("synthesis_exploration", Map(
                "from"  -> toJson(path),
                "fname" -> toJson(fname),
                "cid"   -> toJson(cid),
                "html"  -> toJson(solutionsTree(n, path.reverse))
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
    savedCI        = None
    savedSolutions = Stream.empty

    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci @ ChooseInfo(ctx, prog, fd, pc, src, ch, sopts), search)) =>
        try {
          val sctx = SynthesisContext(ctx, sopts, Some(fd), prog, ctx.reporter)
          val path = List(rid)

          event("synthesis_result", Map(
            "result" -> toJson("init"),
            "fname" -> toJson(fname),
            "cid" -> toJson(cid),
            "problem" -> toJson(ScalaPrinter(ci.ch))
          ))

          savedCI        = Some(ci);
          savedSolutions = search.traversePath(path) match {
            case Some(an: search.g.AndNode) =>
              logInfo("Applying :"+an.ri.toString)
              
              if (!an.isExpanded) {
                an.expand(sctx)
              }

              an.solutions.getOrElse {
                an.composeSolutions(an.descendents.map { d =>
                  Stream(Solution.choose(d.p))
                })
              }

            case _ =>
              logInfo("Path "+path+" did not lead to an and node!")
              throw new Exception("WWOT")
          }

          sendSolution(cstate)

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

  def getNextSolution(cstate: CompilationState) {
    sendSolution(cstate)
  }

  def sendSolution(cstate: CompilationState) {
    val osol = savedSolutions.headOption
    savedSolutions = if (osol.isDefined) savedSolutions.tail else Stream.empty

    val ci = savedCI.get
    val ChooseInfo(ctx, prog, fd, pc, src, ch, _) = ci

    (ci.synthesizer.functionContext, osol) match {
      case (Some(fd), Some(sol)) =>
        import leon.purescala.PrinterHelpers._

        val solCode = sol.toSimplifiedExpr(ctx, prog)

        val (defs, expr) = liftClosures(solCode)

        val fInt = new FileInterface(new MuteReporter())

        fd.body = fd.body.map(b => Simplifiers.bestEffort(ctx, prog)(postMap{
          case ch if ch == ci.ch && ch.getPos == ci.ch.getPos =>
            Some(expr)
          case _ =>
            None
        }(b)))

        val fds = fd :: defs.toList.sortBy(_.id.name)

        val p = new ScalaPrinter(PrinterOptions())

        val allCode = fInt.substitute(cstate.code.getOrElse(""),
                                      fd,
                                      (indent) => {
          implicit val pctx = PrinterContext(fd, None, None, indent, p)
          p"${nary(fds, "\n\n")}"
          p.toString
        })

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

      case (ofd, _) =>
        event("synthesis_result", Map(
          "result" -> toJson("failure"),
          "closed" -> toJson(1),
          "total" -> toJson(1)
        ))

        if (ofd.isDefined) {
          logInfo("Application failed!")
        } else {
          logInfo("No function context?!")
        }
    }
  }

  def getRulesToApply(cstate: CompilationState, fname: String, cid: Int) {
    choosesInfo.get(fname).flatMap(_.lift.apply(cid)) match {
      case Some((ci, search)) =>
        try {
          val sctx = SynthesisContext(ci.ctx, ci.options, Some(ci.fd), ci.prog, ci.ctx.reporter)
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

          val sctx = SynthesisContext(ci.ctx, ci.options, Some(ci.fd), ci.prog, ci.ctx.reporter)

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
