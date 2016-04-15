package leon.web
package client

import org.scalajs.dom

import dom.html.Element
import dom.document
import scala.scalajs.js
import js.annotation._
import org.scalajs.jquery
import jquery.{ jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject }

import js.Dynamic.{ global => g, literal => l, newInstance => jsnew }
import js.JSConverters._

import com.scalawarrior.scalajs.ace._

import leon.web.client.data.User

object HandlersTypes {

  @ScalaJSDefined
  trait HPermalink extends js.Object {
    val link: String
  }

  @ScalaJSDefined
  trait HRepository extends js.Object {
    val id: Long
    val name: String
    val fullName: String
    val owner: String
    val visibility: String
    val fork: Boolean
    val size: Long
    val cloneURL: String
    val defaultBranch: String
    val branches: js.Array[String]
  }

  @ScalaJSDefined
  trait HRepositories extends js.Object {
    val repos: js.Array[HRepository]
  }

  @ScalaJSDefined
  trait HBranch extends js.Object {
    val name: String
    val sha: String
  }

  @ScalaJSDefined
  trait HRepositoryLoaded extends js.Object {
    val repository: HRepository
    val files: js.Array[String]
    val branches: js.Array[HBranch]
    val currentBranch: String
  }

  @ScalaJSDefined
  trait HFileLoaded extends js.Object {
    val file: String
    val content: String
  }

  @ScalaJSDefined
  trait HBranchChanged extends js.Object {
    val success: Boolean
    val branch: js.UndefOr[String]
    val files: js.UndefOr[js.Array[String]]
    val error: js.UndefOr[String]
  }

  @ScalaJSDefined
  trait HGitProgress extends js.Object {
    val taskName: String
    val status: String
    val percentage: js.UndefOr[String]
  }

  @ScalaJSDefined
  trait HGitOperationResult extends js.Object {
    val op: String
    val success: Boolean
    val data: Any
  }

  @ScalaJSDefined
  trait HCommit extends js.Object {
    val hash: String
    val shortHash: String
    val shortMessage: String
    val fullMessage: String
    val commitTime: String
    val author: String
    val committer: String
    val desc: String
  }

  @ScalaJSDefined
  trait HUserUpdated extends js.Object {
    val user: User.Raw
  }
  
  @ScalaJSDefined 
  trait HMoveCursor extends js.Object { val line: Double }
  
  type DataOverView = js.Dictionary[OverviewFunction]
  @ScalaJSDefined 
  trait HUpdateOverview extends js.Object {
    val module: String
    val overview: DataOverView
  }
  
  @ScalaJSDefined trait HUpdateExplorationFacts extends js.Object {val newFacts: js.Array[NewResult]}
  
  @ScalaJSDefined
  trait NewResult extends js.Object {
    val fromRow: Int
    val fromColumn: Int
    val toRow: Int
    val toColumn: Int
    val result: String
  }
  
  @ScalaJSDefined trait HEditor extends js.Object {
     val annotations: js.UndefOr[js.Array[Annotation]]
  }

  @ScalaJSDefined
  trait HNotification extends js.Object {
    val content: String
    val `type`: String
  }
  
  @ScalaJSDefined
  trait HLog extends js.Object {
    val message: String
  }
  
  
  @ScalaJSDefined
  trait HSynthesisResult extends js.Any {
    val result: String;
    val cid: Int;
    val fname: String;
    val problem: String;
    val closed: Double;
    val total: Double;
    val solCode: String;
    val allCode: String;
    val cursor: js.UndefOr[String]
  }
  
  @ScalaJSDefined
  trait HDisambiguationDisplay extends js.Any {
    var display: String
    val allCode: String
  }
  
  @ScalaJSDefined
  trait HDisambiguationResult extends js.Any {
    val input: String
    val fname: String
    val confirm_solution: HDisambiguationDisplay
    val custom_alternative: HDisambiguationDisplay
    val alternatives: js.Array[HDisambiguationDisplay]
  }
  
  @ScalaJSDefined
  trait HSynthesisExploration extends js.Object {
    val html: String
    val fname: String
    val cid: Int
    val from: js.Array[String]
    val allCode: String
    val cursor: js.UndefOr[String]
  }

  @ScalaJSDefined trait HRulesApps extends js.Object with Status {
    val id: Int
    val name: String
  }
  
  @ScalaJSDefined trait HSynthesisRulesToApply extends js.Object {
    val fname: String
    val cid: Int
    val rulesApps: js.Array[HRulesApps]
  }

  @ScalaJSDefined
  trait HRepairResult extends js.Object {
    val result: String
    val progress: String
    val error: String
    val focused: String
    val success: String
    val solCode: String
    val allCode: String
    val cursor: js.UndefOr[String]
  }
  
  @ScalaJSDefined
  trait ResultOutput extends js.Object {
    val result: String
    val output: DualOutput
  }
  
  @ScalaJSDefined
  trait DualOutput extends js.Object {
    val rawoutput: String
    val prettyoutput: String
    var modifying: js.UndefOr[String]
  }
  
  @ScalaJSDefined 
  trait VC extends js.Object with Status {
    val fun: String
    val kind: String
    val time: String
    val counterExample: js.UndefOr[js.Dictionary[DualOutput]]
    val execution: js.UndefOr[ResultOutput]
  }
  
  @ScalaJSDefined
  trait HReplaceCode extends js.Object {val newCode: String}
  
  @ScalaJSDefined
  trait HCompilationProgress extends js.Object { val total: Float; val current: Float }

  @ScalaJSDefined
  trait HCompilation extends js.Object { val status: String}
  
  @ScalaJSDefined trait StatusCode extends js.Object {
    val status: String
    val code: String
  }
  
  @ScalaJSDefined 
  trait Status extends js.Object {
    val status: String
  }
  
  @ScalaJSDefined 
  trait VerificationDetails extends js.Object with Status {
    val vcs: VCS
  }
  
  @ScalaJSDefined
  trait TerminationDetails extends js.Object with Status {
    val call: String
    val calls: js.Array[String]
    val reason: js.UndefOr[String]
  }
  
  @ScalaJSDefined
  trait InvariantDetails extends js.Object with Status {
    val fun: String
    val oldInvariant: String
    val newInvariant: String
    val newCode: String
    val time: Double
  }
  
  @ScalaJSDefined
  trait HInvariants extends js.Object {
    val invariants: js.Array[InvariantDetails]
    val kind: String
    val module: String
    val code: String
  }
  
  type Html = String

  type VCS = js.Array[HandlersTypes.VC]
  
  @ScalaJSDefined
  trait OverviewFunction extends js.Object {
    val name: String
    val displayName: String
    val line: Int
    val column: Int
  }
}

@ScalaJSDefined
object Handlers extends js.Object {
  import Main._
  import JQueryExtended._
  import js.JSON
  import leon.web.shared.Action;
  import dom.alert
  import dom.console
  import HandlersTypes._
  def window = g

  import Implicits._
  val permalink = (data: HPermalink) => {
    $("#permalink-value input").value(window._leon_url + "#link/" + data.link)
    $("#permalink-value").show()
  }

  val move_cursor = (data: HMoveCursor) => {
    Main.editor.selection.clearSelection();
    Main.editor.gotoLine(data.line);
  }

  val update_overview = (data: HUpdateOverview) => {
    if (data.module == "main") {
      overview.functions = js.Dictionary.empty[OverviewFunction];

      for ((i, fdata) <- data.overview) {
        val fdata = data.overview(i)
        val fname = fdata.name
        overview.functions(fname) = fdata
      }
    } else {
      overview.Data(data.module) = data.overview
    }

    drawOverView()
  }

  val update_synthesis_overview = (data: SynthesisOverview) => {
    if (JSON.stringify(synthesisOverview) != JSON.stringify(data)) {
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
          val cid =  $("#synthesis_table td.fname[fname="+fname+"]").attr("cid").orIfNull("0").toInt
          console.log("Finding rules to apply 2 " + new js.Date())
          Backend.synthesis.getRulesToApply(fname, cid)
        }
      }
    }
  }

  val update_exploration_facts = (data: HUpdateExplorationFacts) => {
    updateExplorationFacts(data.newFacts);
  }

  def updateExplorationFacts(newResults: js.Array[NewResult]): Unit = {
    for (i <- 0 until newResults.length) {
      val n = newResults(i);

      explorationFacts.push(ExplorationFact(
        range = jsnew(aceRange)(n.fromRow, n.fromColumn, n.toRow, n.toColumn).asInstanceOf[Range],
        res = n.result
      ));
    }

    displayExplorationFacts()
  }

  val editor = (data: HEditor) => {
    if (data.annotations.isDefined) {
      val annotations = data.annotations.get
      val session = Main.editor.getSession();

      context = "unknown";

      $("#annotations").html("");

      for (a <- annotations) {
        if (a.`type` == "verification") {
          context = "verification";
        } else if (a.`type` == "synthesis") {
          context = "synthesis";
        }

        if (a.`type` != "info" && a.`type` != "error" && a.`type` != "warning") {
          session.addGutterDecoration(a.row, "leon_gutter_" + a.`type`)
          a.`type` = "info";
        }

        if (a.`type` == "error") {
          val line = a.row + 1
          $("#annotations").append("<li class=\"clicktoline error\" line=\"" + line + "\"><code><i class=\"fa fa-warning\"></i> " + line + ":" + a.text + "</code></li>")
        } else if (a.`type` == "warning") {
          val line = a.row + 1
          $("#annotations").append("<li class=\"clicktoline warning\" line=\"" + line + "\"><code><i class=\"fa fa-warning\"></i> " + line + ":" + a.text + "</code></li>")
        }
      }

      addClickToLine("#annotations");
      session.setAnnotations(annotations);
      resizeEditor();
    }
  }

  val notification = (data: HNotification) => {
    Main.notify(data.content, data.`type`);
  }

  val log = (data: HLog) => {
    val txt = $("#console")
    txt.append(data.message + "\n");
    txt.scrollTop((txt(0).scrollHeight - txt.height()).toInt)
  }
  
  val synthesis_result = (data: HSynthesisResult) => {
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
        Handlers.replace_code(new HReplaceCode { val newCode = data.allCode })
        if (data.cursor.isDefined) {
          js.timers.setTimeout(100) {
            Handlers.move_cursor(data.cursor.get.asInstanceOf[HMoveCursor])
          }
        }
      })
      $("#synthesisDialog .exploreButton").unbind("click").click(() => {
        val cid = $("#synthesisDialog").attr("cid").toInt
        val fname = $("#synthesisDialog").attr("fname")

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
      Handlers.replace_code(new HReplaceCode { val newCode = alternative.allCode })
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
      edittext.find("font[face=FontAwesome]").each{ (index: js.Any, _this: dom.Element) =>
        changeSelection = true
        $(_this).replaceWith($(_this).html())
      }
      edittext.find("span.placeholder").each{ (index: js.Any, _this: dom.Element) =>
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
      Handlers.replace_code(new HReplaceCode { val newCode = customCode })
      val toFill = disambiguationResultDisplay()
      toFill.empty().append($("<code>").text(edittext.text()))
      toFill.append(" chosen. Looking for more ambiguities...")
    })
    container
  }
  
  val disambiguation_started = (data: js.Dynamic) => {
    $("""<i class="fa fa-refresh fa-spin" title=""></i>""").appendTo(
      engineResultDisplayContainer().find("a[href=#clarificationResults]").parent().addClass("loading").show()
    )
  }
  
  val disambiguation_noresult = (data: js.Dynamic) => {
    engineResultDisplayContainer().find("a[href=#clarificationResults]").parent().hide()
    .removeClass("loading").find("i.fa.fa-refresh.fa-spin").remove()
  }
  
  val disambiguation_result = (data: HDisambiguationResult) => {
    console.log("Received disambiguation data", data)
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
    engineResultDisplayContainer().find("a[href=#clarificationResults]").click()
    Main.showContextDemo(Main.demoClarification)
  }

  val synthesis_exploration = (data: HSynthesisExploration) => {
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

    val node = d.find(".exploreBlock[path=\"" + data.from.join("-") + "\"]")
    node.replaceWith(data.html)
    g.prettyPrint();

    val wsOf = (e: Element) => {
      val b = $(e).closest(".exploreBlock")
      b.attr("ws").toInt
    }

    val pathOf = (e: Element) => {
      val b = $(e).closest(".exploreBlock")
      var path = js.Array[Int]()
      if (b.attr("path") =!= "") {
        path = b.attr("path").split("-").toJSArray.map((e: String) => e.toInt)
      }
      path
    }

    d.find("""select[data-action="select-alternative"]""").unbind("change").change(((_this: Element) => {
      $(_this).after(""" <span class="fa fa-spin fa-circle-o-notch"></span>""");
      Backend.synthesis.explore(
          fname  = data.fname,
          cid    = data.cid,
          path   = pathOf(_this),
          exploreAction = $(_this).attr("data-action"),
          ws     = wsOf(_this),
          select = $(_this).value().asInstanceOf[String].toInt)
    }): js.ThisFunction);

    d.find("span.knob").unbind("click").click(((self: Element) => {
      $(self).removeClass("fa-arrow-right fa-arrow-left").addClass("fa-spin fa-refresh")
      Backend.synthesis.explore(
          fname = data.fname,
          cid   = data.cid, pathOf(self),
          exploreAction = $(self).attr("data-action"),
          ws    = wsOf(self))
      working = true
    }): js.ThisFunction);

    d.find(".importButton").unbind("click").click(() => {
      Handlers.replace_code(new HReplaceCode { val newCode = data.allCode })
      if (data.cursor.isDefined) {
        js.timers.setTimeout(100) {
          Handlers.move_cursor(data.cursor.get.asInstanceOf[HMoveCursor])
        }
      }
    })
  }
  
  var synthesis_chosen_rule: Option[String] = None
  var synthesis_result_fname: js.UndefOr[String] = js.undefined

  val synthesis_rulesToApply = (data: HSynthesisRulesToApply) => {
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
      val cid =  $("#synthesis_table td.fname[fname="+fname+"]").attr("cid").orIfNull("0").toInt
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
      val rid = $(self).attr("rid").toInt
      synthesis_chosen_rule = Some(rid.toString)
      Backend.synthesis.doApplyRule(fname, cid, rid)
    }): js.ThisFunction)
  }

  val repair_result = (data: HRepairResult) => {
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
        Handlers.replace_code(new HReplaceCode { val newCode = data.allCode })
        if (data.cursor.isDefined) {
          js.timers.setTimeout(100) {
            Handlers.move_cursor(data.cursor.get.asInstanceOf[HMoveCursor])
          }
        }
      })
      $("#repairDialog .cancelButton").hide()
      $("#repairDialog .closeButton").show()
    }
  }

  val verification_result = (data: VerificationDetails) => {
    displayVerificationDetails(data.status, data.vcs)
  }

  val replace_code = (data: HReplaceCode) => {
    storeCurrent(editorSession.getValue())
    editorSession.setValue(data.newCode)
    recompile()
  }

  val compilation_progress = (data: HCompilationProgress) => {
    updateCompilationProgress(Math.round((data.current * 100) / data.total))
  }

  val compilation = (data: HCompilation) => {
    if (data.status == "success") {
      updateCompilationStatus("success")
    } else {
      updateCompilationStatus("failure")
    }
  }
}
