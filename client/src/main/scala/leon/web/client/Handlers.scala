package leon.web
package client

import org.scalajs.dom
import dom.html.Element
import scala.scalajs.js
import js.annotation._
import org.scalajs.jquery
import jquery.{ jQuery => $, JQuery, JQueryEventObject }
import js.Dynamic.{ global => g, literal => l, newInstance => jsnew }
import js.JSConverters._
import com.scalawarrior.scalajs.ace._
import leon.web.shared.equal
import scala.collection.mutable.ListBuffer


@ScalaJSDefined
object Handlers extends js.Object {
  import Main._
  import JQueryExtended._
  import dom.console
  def window = g
  def alert = g.alert
  import leon.web.shared.messages._

  import Implicits._
  
  import equal.EqOps
  
  val callbacks = ListBuffer[PartialFunction[U forSome {type U <: MessageFromServer }, Unit]]()
  
  @JSName("apply")
  def apply(data: MessageFromServer): Unit = {
    callbacks.find(c => c.isDefinedAt(data)) match {
      case Some(callback) =>
        callbacks -= callback
        callback(data)
        return
      case None =>
    }
    //println("Processing " + data)
    data match {
      case data: GotPermalink => 
        $("#permalink-value input").value(window._leon_url + "#link/" + data.link)
        $("#permalink-value").show()
      case data: HMoveCursor =>
        Main.editor.selection.clearSelection();
        Main.editor.gotoLine(data.line);

      case data: HUpdateOverview =>
        overview.functions = js.Dictionary.empty[OverviewFunction];
  
        for ((i, fdata) <- data.overview) {
          val fdata = data.overview(i)
          val fname = fdata.name
          overview.functions(fname) = fdata
        }

      case data: HUpdateVerificationOverview =>
        overview.Data.verification = data.overview
    
        drawOverView()
        drawVerificationOverviewInGutter()
      case data: HUpdateTerminationOverview =>
        overview.Data.termination = data.overview

        drawOverView()
      case data: HUpdateInvariantsOverview =>
        overview.Data.invariant = data.overview

        drawOverView()
      case data: SynthesisOverview =>
        if (synthesisOverview.toString != data.toString) {
          synthesisOverview = data;
          drawSynthesisOverview();
          
          Main.onSynthesisTabDisplay match {
            case Some(handler) => handler()
              Main.onSynthesisTabDisplay = None
            case None =>
          }
          
          val hasFunctions = data.functions.isDefined && data.functions.get.keys.nonEmpty
          if (hasFunctions && Features.synthesis.active) {
            if($("#synthesisDialog").is(":visible") && compilationStatus == 1) { // Automatic retrieval of rules if the synthesis dialog is visible.
              val fname = (Handlers.synthesis_result_fname.getOrElse(""): String)
              val cid =  $("#synthesis_table td.fname[fname="+fname+"]").attr("cid").getOrElse("0").toInt
              console.log("Finding rules to apply 2 " + new js.Date())
              Backend.synthesis.getRulesToApply(fname, cid)
            }
          }
        }
        
      case data: HUpdateExplorationFacts =>
        updateExplorationFacts(data.newFacts)
      case data: HEditor =>
        if (data.annotations.isDefined) {
          val annotations = data.annotations.get
          val session = Main.editor.getSession();
    
          context = "unknown";
    
          $("#annotations").html("");
    
          for (a <- annotations) {
            if (a.tpe == "verification") {
              context = "verification";
            } else if (a.tpe == "synthesis") {
              context = "synthesis";
            }
            var tpe = a.tpe.kind
            
            if (tpe != "info" && tpe != "error" && tpe != "warning") {
              session.addGutterDecoration(a.line, "leon_gutter_" + tpe)
              tpe = "info";
            }
    
            if (tpe == "error") {
              val line = a.line + 1
              $("#annotations").append("<li class=\"clicktoline error\" line=\"" + line + "\"><code><i class=\"fa fa-warning\"></i> " + line + ":" + a.message + "</code></li>")
            } else if (tpe == "warning") {
              val line = a.line + 1
              $("#annotations").append("<li class=\"clicktoline warning\" line=\"" + line + "\"><code><i class=\"fa fa-warning\"></i> " + line + ":" + a.message + "</code></li>")
            }
          }
    
          addClickToLine("#annotations");
          session.setAnnotations(annotations.map(a => l(row = a.line-1, column = a.col-1, text = a.message, `type` = a.tpe.kind).asInstanceOf[com.scalawarrior.scalajs.ace.Annotation]).toJSArray);
          resizeEditor();
        }
        
      case data: HNotification =>
        Main.notify(data.content, data.`type`)
        
      case data: HLog =>
        val txt = $("#console")
        txt.append(data.message + "\n");
        txt.scrollTop((txt(0).scrollHeight - txt.height()).toInt)

      case data: HSynthesisResult => synthesis_result(data)
      case DisambiguationStarted =>  disambiguation_started()
      case DisambiguationNoresult => disambiguation_noresult()
      case data: HDisambiguationResult => disambiguation_result(data)
      case data: HSynthesisExploration => synthesis_exploration(data)
      case data: VerificationDetails => verification_result(data)
      case data: HReplaceCode => replace_code(data)
      case data: HCompilationProgress => compilation_progress(data)
      case data: HCompilation => compilation(data)
      case data: HRepairResult => repair_result(data)
      case data: HSynthesisRulesToApply => synthesis_rulesToApply(data)
      
      case RegisteredHandlers() => // OK
      case _ =>
        console.log("Unknown event type: " + data)
    }
  }

  object RegisteredHandlers {
    var handlers = ListBuffer[MessageFromServer => Boolean]()
    def unapply(data: MessageFromServer): Boolean = {
      var done = false
      for(h <- handlers if !done) {
        done = h(data)
      }
      done
    }
  }
  
  def registerMessageHandler(handler: MessageFromServer => Boolean) = {
    RegisteredHandlers.handlers += handler
  }

  def updateExplorationFacts(newResults: Array[NewResult]): Unit = {
    for (i <- 0 until newResults.length) {
      val n = newResults(i);

      explorationFacts.push(ExplorationFact(
        range = jsnew(aceRange)(n.fromRow-1, n.fromColumn-1, n.toRow-1, n.toColumn-1).asInstanceOf[Range],
        res = n.result
      ));
    }

    displayExplorationFacts()
  }

  def synthesis_result(data: HSynthesisResult) = {
    val pb = $("#synthesisProgress")
    val pbb = $("#synthesisProgress .progress-bar")
    
    // setup and open pane
    if (data.result == "init") {
      $("#synthesisResults").hide()
      $("#synthesisDialog .clarificationResults").hide()
      $("#synthesisDialog").attr("cid", data.cid)
      $("#synthesisDialog").attr("fname", data.fname)
      $("#synthesisDialog .exploreButton").hide()
      $("#synthesisDialog .importButton").hide()
      $("#synthesisDialog .closeButton").hide()
      $("#synthesisDialog .cancelButton").show()
      $("#synthesisDialog .code.problem").removeClass("prettyprinted")
      $("#synthesisDialog .code.problem").text(data.problem)
      g.prettyPrint();
      $("#synthesisDialog").modal("show")
      
      $("#synthesisDialog .engineResult > ul a").off("click.tabs")
      $("#synthesisDialog .engineResult > ul a").on("click.tabs", ((_this: Element, event: JQueryEventObject) => {
          event.preventDefault();
          $(_this).parent().addClass("current").show();
          $(_this).parent().siblings().removeClass("current");
          var tab = $(_this).attr("href");
          $("#synthesisDialog .engineResult > div").not(tab).css("display", "none");
          $(tab).fadeIn();
      }): js.ThisFunction);
      
      synthesis_result_fname = data.fname

      pbb.addClass("active progress-bar-striped")
      pbb.removeClass("progress-bar-success progress-bar-danger")
      pbb.width("100%")
      pbb.html("Synthesizing...");

      $("#synthesisProgressBox").show()
      synthesizing = true;
      $("#synthesisDialog").unbind("hide.bs.modal").on("hide.bs.modal", () => {
        if (synthesizing) Backend.main.cancel()
      })
    } else if (data.result == "progress") {
      val pc = (data.closed * 100) / data.total;
      pbb.width(pc + "%")
      pbb.html(data.closed + "/" + data.total);

    } else if (data.result == "failure") {
      pbb.removeClass("active progress-bar-striped")

      pbb.width("100%")
      pbb.html("Failed to apply");
      pbb.addClass("progress-bar-danger")

      $("#synthesisDialog .importButton").hide()
      $("#synthesisDialog .exploreButton").hide()
      $("#synthesisDialog .cancelButton").hide()
      $("#synthesisDialog .closeButton").show()
      synthesizing = false;

    } else if (data.result == "success") {
      pbb.removeClass("active progress-bar-striped")

      pbb.width("100%")
      pbb.html(data.closed + "/" + data.total);
      pbb.addClass("progress-bar-success")

      $("#synthesisResults .code.solution").removeClass("prettyprinted")
      $("#synthesisResults .code.solution").text(data.solCode)
      
      $("#synthesisDialog").find("a[href=#clarificationResults]").parent().hide()
      $("#synthesisDialog").find("a[href=#synthesisResults]").click()
      
      //$("#synthesisResults").show()
      g.prettyPrint();
      $("#synthesisDialog .exploreButton").show()
      $("#synthesisDialog .importButton").show()
      $("#synthesisDialog .importButton").unbind("click").click(() => {
        Handlers(HReplaceCode(newCode = data.allCode))
        if (data.cursor.isDefined) {
          js.timers.setTimeout(100) {
            Handlers(data.cursor.get)
          }
        }
      })
      $("#synthesisDialog .exploreButton").unbind("click").click(() => {
        val cid = $("#synthesisDialog").attr("cid").getOrElse("0").toInt
        val fname = $("#synthesisDialog").attr("fname").getOrElse("")

        $("#synthesisDialog").modal("hide")

        Backend.synthesis.explore(fname, cid)
      })
      $("#synthesisDialog .cancelButton").hide()
      $("#synthesisDialog .closeButton").show()
      synthesizing = false;
      disambiguationResultDisplay().empty()
    }
  }
  
  def inDialog(selector: String): JQuery = {
    $("#synthesisDialog .clarificationResults .clarificationQuestions")
  }
  
  def disambiguationResultDisplay(): JQuery = {
    disambiguationResultDisplayContainer.find(".clarificationQuestions")
  }
  /** The tab containing .clarificationQuestions */
  def disambiguationResultDisplayContainer(): JQuery = {
    engineResultDisplayContainer().find(".clarificationResults")
  }
  def engineResultDisplayContainer(): JQuery = {
    if($("#synthesisDialog").is(":visible")) {
      $("#synthesisDialog .engineResult")
    } else if($("#synthesisExploreDialog").is(":visible")) {
      $("#synthesisExploreDialog .engineResult")
    } else $("") // TODO: Exploration
  }
  
  def displayAlternative(alternative: HDisambiguationDisplay, current: Boolean, custom: HDisambiguationDisplay, directEdit: Boolean): JQuery = {
    val result: JQuery = $("<pre>")
      .addClass("disambiguationAlternative")
      .addClass(if(current) "current" else "")
      .attr("title", if(current) "current output" else "alternative")
      .text(alternative.display)
      .on("click.alternative", () => {
      Handlers(HReplaceCode(newCode = alternative.allCode))
      val toFill = disambiguationResultDisplay()
      toFill.empty().append($("<code>").text(alternative.display))
      toFill.append(" chosen. Looking for more ambiguities...")
      /*if (data.cursor.isDefined) {
        js.timers.setTimeout(100) {
          Handlers.move_cursor(data.cursor.get.asInstanceOf[HMoveCursor])
        }
      }*/
    }).dblclick(() => {
      
    })
    alternative.display = "(_edit_me_)+".r.replaceAllIn(alternative.display, "_edit_me_")
    val editbox = $("""<i class="fa fa-pencil-square-o"></i>""").addClass("toactivate").text("edit").hide()
    val edittext = $("<pre>").attr("contentEditable", "true").addClass("disambiguationAlternative editing").addClass(if(current) "current" else "").text(alternative.display).hide()
    edittext.html(edittext.html().replaceAll("_edit_me_", """<span class="placeholder" style="font-family:FontAwesome">&#xf059;</span>"""))    
    edittext.on("keyup paste click", () => {
      val pos = SelectionHandler.getSelection(edittext.get(0).asInstanceOf[dom.raw.Element])
      var changeSelection = false
      edittext.find("font[face=FontAwesome]").each{ (index: Int, _this: dom.Element) =>
        changeSelection = true
        $(_this).replaceWith($(_this).html())
      }
      edittext.find("span.placeholder").each{ (index: Int, _this: dom.Element) =>
        val oldHtml = $(_this).html()
        if(oldHtml != "&#xf059;" && oldHtml != "") {
          $(_this).replaceWith("&#xf059;|".r.replaceAllIn(oldHtml, ""))
          changeSelection = true
        }
        ().asInstanceOf[js.Any]
      }
      if(changeSelection) {
        SelectionHandler.setSelection(edittext.get(0).asInstanceOf[dom.raw.Element], pos)
      }
    })
    
    val validatebox = $("""<i class="fa fa-check"></i>""").addClass("validate").text("validate").hide()
    val container = $("<span>").addClass("menu-disambiguation").append(result).append(edittext).append(editbox).append(validatebox)
    container.mouseenter((e: JQueryEventObject) => {
      if(!validatebox.is(":visible")) {
        editbox.show()
        editbox.height(container.height())
      }
      ().asInstanceOf[js.Any]
    }).mouseleave((e: JQueryEventObject) => {
      editbox.hide()
      ().asInstanceOf[js.Any]
    })
    edittext.focus(() => {
      validatebox.addClass("active")
    }).blur(() => {
      validatebox.removeClass("active")
    })
    editbox.on("click", () => {
      validatebox.show()
      validatebox.height(container.height())
      val lineHeight = result.css("line-height")
      val a = lineHeight.substring(0, lineHeight.length - 2).toFloat
      if(result.height() != 0 && result.width() != 0) {
        edittext.height(result.height() + a)
        edittext.width(result.width() + a)
      }
      edittext.show()
      edittext.click()
      result.hide()
    })
    if(directEdit) {
      result.hide()
      js.timers.setTimeout(1){
        editbox.click()
        container.focus()
      }
    } else if(alternative.display.indexOf("_edit_me_") > -1) { // Must edit when clicking.
      result.hide()
      js.timers.setTimeout(1){
        editbox.click()
      }
      validatebox.show()
      validatebox.height(container.height())
    }
    
    validatebox.on("click", () => {
      val customCode = custom.allCode.replace("\""+leon.web.shared.Constants.disambiguationPlaceHolder + "\"", edittext.text())
      Handlers(HReplaceCode(newCode = customCode))
      val toFill = disambiguationResultDisplay()
      toFill.empty().append($("<code>").text(edittext.text()))
      toFill.append(" chosen. Looking for more ambiguities...")
    })
    container
  }
  
  def disambiguation_started() = {
    $("""<i class="fa fa-refresh fa-spin" title=""></i>""").appendTo(
      engineResultDisplayContainer().find("a[href=#clarificationResults]").parent().addClass("loading").show()
    )
  }
  
  def disambiguation_noresult() = {
    engineResultDisplayContainer().find("a[href=#clarificationResults]").parent().hide()
    .removeClass("loading").find("i.fa.fa-refresh.fa-spin").remove()
  }
  
  def disambiguation_result(data: HDisambiguationResult) = {
    console.log("Received disambiguation data " + data.toString)
    engineResultDisplayContainer().find("a[href=#clarificationResults]").parent()
    .removeClass("loading").find("i.fa.fa-refresh.fa-spin").remove()
    val toFill = disambiguationResultDisplay()
    
    val args = if(data.input(0) == '(') {
      data.input
    } else {
      "(" + data.input + ")"
    }
    toFill.empty()
    val (premessage, message) = if(data.alternatives.length == 0) {
           ("To ensure completeness, please edit the pretty-printing of ", " below:")
    } else ("What should be the output of ", "?")
    val html = premessage + "<code>" + data.fname + args + "</code>"+message+"<br>"
    toFill.append(html)
    
    if(data.alternatives.length == 0) {
      toFill.append(displayAlternative(data.confirm_solution, current=true, data.custom_alternative, true))
      toFill.find(".validate").addClass("active")
    } else {
      toFill.append(displayAlternative(data.confirm_solution, current=true, data.custom_alternative, false))
      for(alternative <- data.alternatives) {
        toFill.append("<br>")
        toFill.append(displayAlternative(alternative, false, data.custom_alternative, false))
      }
    }
    //disambiguationResultDisplayContainer().show()
    // Switch tabs:
    if(data.forceAsking) {
      engineResultDisplayContainer().find("a[href=#clarificationResults]").click()
      Main.showContextDemo(Main.demoClarification)
    } else {
      Main.showContextDemo(Main.demoClarificationMenu)
    }
  }

  def synthesis_exploration(data: HSynthesisExploration) = {
    val d = $("#synthesisExploreDialog");

    g.prettyPrint();

    if (!d.is(":visible")) {
      d.modal("show")
    }

    var working = false

    d.unbind("hide.bs.modal").on("hide.bs.modal", () => {
      if (working) {
        Backend.main.cancel()
      }
    })

    val node = d.find(".exploreBlock[path=\"" + data.from.mkString("-") + "\"]")
    node.replaceWith(data.html)
    g.prettyPrint();

    val wsOf = (e: Element) => {
      val b = $(e).closest(".exploreBlock")
      b.attr("ws").getOrElse("0").toInt
    }

    val pathOf = (e: Element) => {
      val b = $(e).closest(".exploreBlock")
      var path = List[Int]()
      if (new EqOps(b.attr("path").getOrElse("")) =!= "") {
        path = b.attr("path").getOrElse("").split("-").map((e: String) => e.toInt).toList
      }
      path
    }

    d.find("""select[data-action="select-alternative"]""").unbind("change").change(((_this: Element) => {
      $(_this).after(""" <span class="fa fa-spin fa-circle-o-notch"></span>""");
      Backend.synthesis.explore(
          fname  = data.fname,
          cid    = data.cid,
          path   = pathOf(_this),
          exploreAction = $(_this).attr("data-action").getOrElse(""),
          ws     = wsOf(_this),
          select = $(_this).value().asInstanceOf[String].toInt)
    }): js.ThisFunction);

    d.find("span.knob").unbind("click").click(((self: Element) => {
      $(self).removeClass("fa-arrow-right fa-arrow-left").addClass("fa-spin fa-refresh")
      Backend.synthesis.explore(
          fname = data.fname,
          cid   = data.cid, pathOf(self),
          exploreAction = $(self).attr("data-action").getOrElse(""),
          ws    = wsOf(self))
      working = true
    }): js.ThisFunction);

    d.find(".importButton").unbind("click").click(() => {
      Handlers(HReplaceCode(newCode = data.allCode))
      if (data.cursor.isDefined) {
        js.timers.setTimeout(100) {
          Handlers(data.cursor.get)
        }
      }
    })
  }
  
  var synthesis_chosen_rule: Option[String] = None
  var synthesis_result_fname: js.UndefOr[String] = js.undefined

  def synthesis_rulesToApply(data: HSynthesisRulesToApply) = {
    val fname = data.fname
    val cid = data.cid
    val rulesApps = data.rulesApps

    var html = "";

    // Start by removing temp content
    if (compilationStatus == 1) {
      for (i <- 0 until rulesApps.length) {
        val app = rulesApps(i);
        var statusIcon = ""
        var clazz = "temp"

        if (app.status == "closed") {
          statusIcon = """<i class="fa fa-exclamation-circle"></i> """
          clazz += " disabled"
        }
        html += """<li role="presentation" class="""" + clazz + """"><a role="menuitem" tabindex="-1" href="#" action="rule" cid="""" + cid + """" rid="""" + app.id + """">""" + statusIcon + app.name + """</a></li>"""
      }
    } else {
      html += """<li role="presentation" class="temp disabled"><a role="menuitem" tabindex="-1" href="#" fname="""" + fname + """">Not yet compiled...</a></li>"""
    }

    val selector = "#synthesis .problem[fname=\"" + fname + "\"][cid=\"" + cid + "\"] ul"
    $(selector + " li.temp").remove()
    $(selector).append(html)
    $(selector + " li a[action=\"search\"]").unbind("click.searchaction").on("click.searchaction", (() => {
      synthesis_chosen_rule = Some("search")
      Backend.synthesis.search(fname, cid)
    }))
    if($("#synthesisDialog").is(":visible") && (synthesis_result_fname.getOrElse("") == fname)) { // Was not closed maybe because of disambiguation. Relaunch synthesis for the same method.
      val cid =  $("#synthesis_table td.fname[fname="+fname+"]").attr("cid").getOrElse("0").toInt
      synthesis_chosen_rule match {
        case None => // Explore
        case Some("search") => // Search
          Backend.synthesis.search(synthesis_result_fname.getOrElse(""), cid)
        case Some(rid) => // Rule chosen
          Backend.synthesis.doApplyRule(fname, cid, rid.toInt)
      }
    }

    
    $(selector + " li a[action=\"explore\"]").unbind("click").click(() => {
      synthesis_chosen_rule = None
      Backend.synthesis.explore(fname, cid)
    })
    $(selector + " li a[action=\"rule\"]").click(((self: Element) => {
      val rid = $(self).attr("rid").getOrElse("0").toInt
      synthesis_chosen_rule = Some(rid.toString)
      Backend.synthesis.doApplyRule(fname, cid, rid)
    }): js.ThisFunction)
  }

  def repair_result(data: HRepairResult) = {
    val pb = $("#repairProgress")
    val pbb = $("#repairProgress .progress-bar")

    // setup and open pane
    if (data.result == "init") {
      $("#repairResults").hide()
      $("#repairFocused").hide()
      $("#repairDialog .importButton").hide()
      $("#repairDialog .closeButton").hide()
      $("#repairDialog .cancelButton").show()
      $("#repairDialog").modal("show")

      $("#repairDialog").unbind("hide.bs.modal").on("hide.bs.modal", () => {
        if (synthesizing) Backend.repair.cancel()
      })
    } else if (data.result == "progress") {
      pbb.addClass("active progress-bar-striped")
      pbb.removeClass("progress-bar-success progress-bar-danger")
      pbb.width("100%")
      pbb.html(data.progress);

      $("#repairProgressBox").show()
    } else if (data.result == "error") {
      pbb.removeClass("active progress-bar-striped")

      pbb.width("100%")
      pbb.html(data.error);
      pbb.addClass("progress-bar-danger")

      $("#repairDialog .cancelButton").hide()
      $("#repairDialog .closeButton").show()
    } else if (data.result == "focused") {
      $("#repairFocused .code.focused").removeClass("prettyprinted")
      $("#repairFocused .code.focused").html(data.focused)
      $("#repairFocused").show()
      g.prettyPrint();
    } else if (data.result == "success") {
      pbb.removeClass("active progress-bar-striped")

      pbb.width("100%")
      pbb.html(data.success);
      pbb.addClass("progress-bar-success")

      $("#repairResults .code.solution").removeClass("prettyprinted")
      $("#repairResults .code.solution").text(data.solCode)
      $("#repairResults").show()
      g.prettyPrint();
      $("#repairDialog .importButton").show()
      $("#repairDialog .importButton").unbind("click").click(() => {
        Handlers(HReplaceCode(newCode = data.allCode))
        if (data.cursor.isDefined) {
          js.timers.setTimeout(100) {
            Handlers(data.cursor.get)
          }
        }
      })
      $("#repairDialog .cancelButton").hide()
      $("#repairDialog .closeButton").show()
    }
  }

  def verification_result(data: VerificationDetails) = {
    displayVerificationDetails(data.fname, data.status, data.vcs, data.crashingInputs)
  }

  def replace_code(data: HReplaceCode) = {
    storeCurrent(editorSession.getValue())
    editorSession.setValue(data.newCode)
    recompile()
  }

  def compilation_progress(data: HCompilationProgress) = {
    updateCompilationProgress(Math.round((data.current * 100) / data.total))
  }

  def compilation(data: HCompilation) = {
    if (data.status == "success") {
      updateCompilationStatus("success")
    } else {
      updateCompilationStatus("failure")
    }
  }
}
