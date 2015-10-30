package leon.web.client

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
  }

  @ScalaJSDefined
  trait HRepositories extends js.Object {
    val repos: js.Array[HRepository]
  }

  @ScalaJSDefined
  trait HLoadRepository extends js.Object {
    val files: js.Array[String]
  }

  @ScalaJSDefined
  trait HLoadFile extends js.Object {
    val file: String
    val content: String
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
    val output: String
  }
  
  @ScalaJSDefined 
  trait VC extends js.Object with Status {
    val fun: String
    val kind: String
    val time: String
    val counterExample: js.UndefOr[js.Dictionary[String]]
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
  import Bool._
  import JQueryExtended._
  import js.JSON
  import leon.web.shared.Action;
  import dom.alert
  import dom.console
  import HandlersTypes._
  def window = g

  val permalink = (data: HPermalink) => {
    $("#permalink-value input").value(window._leon_url + "#link/" + data.link)
    $("#permalink-value").show()
  }

  val move_cursor = (data: HMoveCursor) => {
    Main.editor.selection.clearSelection();
    Main.editor.gotoLine(data.line);
  }

  val update_overview = (data: HUpdateOverview) => {
    console.log("Received overview:",data)
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
    }
  }

  val update_exploration_facts = (data: HUpdateExplorationFacts) => {
    updateExplorationFacts(data.newFacts);
  }

  def updateExplorationFacts(newResults: js.Array[NewResult]) {
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

      if(annotations.length > 0) dom.console.log(annotations)

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
      $("#synthesisDialog").attr("cid", data.cid)
      $("#synthesisDialog").attr("fname", data.fname)
      $("#synthesisDialog .exploreButton").hide()
      $("#synthesisDialog .importButton").hide()
      $("#synthesisDialog .closeButton").hide()
      $("#synthesisDialog .cancelButton").show()
      $("#synthesisDialog .code.problem").removeClass("prettyprinted")
      $("#synthesisDialog .code.problem").html(data.problem)
      g.prettyPrint();
      $("#synthesisDialog").modal("show")

      pbb.addClass("active progress-bar-striped")
      pbb.removeClass("progress-bar-success progress-bar-danger")
      pbb.width("100%")
      pbb.html("Synthesizing...");

      $("#synthesisProgressBox").show()
      synthesizing = true;
      $("#synthesisDialog").unbind("hide.bs.modal").on("hide.bs.modal", () => {
        if (synthesizing) {
          val msg = JSON.stringify(l(
            module = "main",
            action = Action.doCancel))

          leonSocket.send(msg)
        }
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
      $("#synthesisResults .code.solution").html(data.solCode)
      $("#synthesisResults").show()
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

        val msg = JSON.stringify(
          l("action" -> Action.doExplore,
            "module" -> "synthesis",
            "fname" -> fname,
            "cid" -> cid,
            "explore-action" -> "init",
            "path" -> js.Array[Int](),
            "ws" -> 0)
        )

        leonSocket.send(msg)
      })
      $("#synthesisDialog .cancelButton").hide()
      $("#synthesisDialog .closeButton").show()
      synthesizing = false;
    }
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
        val msg = JSON.stringify(l(
          module = "main",
          action = Action.doCancel))

        leonSocket.send(msg)
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
      if (b.attr("path") != "") {
        path = b.attr("path").split("-").toJSArray.map((e: String) => e.toInt)
      }
      path
    }

    d.find("""select[data-action="select-alternative"]""").unbind("change").change(((_this: Element) => {

      $(_this).after(""" <span class="fa fa-spin fa-circle-o-notch"></span>""");

      val msg = JSON.stringify(
        l("action" -> Action.doExplore,
          "module" -> "synthesis",
          "plop" -> "plap",
          "fname" -> data.fname,
          "cid" -> data.cid,
          "path" -> pathOf(_this),
          "explore-action" -> $(_this).attr("data-action"),
          "select" -> $(_this).value().asInstanceOf[String].toInt,
          "ws" -> wsOf(_this))
        )

      leonSocket.send(msg)
    }): js.ThisFunction);

    d.find("span.knob").unbind("click").click(((self: Element) => {
      $(self).removeClass("fa-arrow-right fa-arrow-left").addClass("fa-spin fa-refresh")

      val msg = JSON.stringify(
        l("action" -> Action.doExplore,
          "module" -> "synthesis",
          "fname" -> data.fname,
          "cid" -> data.cid,
          "path" -> pathOf(self),
          "explore-action" -> $(self).attr("data-action"),
          "ws" -> wsOf(self))
      )

      leonSocket.send(msg)

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
    $(selector + " li a[action=\"search\"]").unbind("click").click(() => {
      val msg = JSON.stringify(
        l(action = Action.doSearch, module = "synthesis", fname = fname, cid = cid))

      leonSocket.send(msg)
    })
    $(selector + " li a[action=\"explore\"]").unbind("click").click(() => {
      val msg = JSON.stringify(
          l("action" -> Action.doExplore,
            "module" -> "synthesis",
            "fname" -> fname,
            "cid" -> cid,
            "explore-action" -> "init",
            "path" -> js.Array[js.Any](),
            "ws" -> 0)
        )
      leonSocket.send(msg)
    })
    $(selector + " li a[action=\"rule\"]").click(((self: Element) => {
      val rid = $(self).attr("rid").toInt

      val msg = JSON.stringify(
        l(action = Action.doApplyRule, module = "synthesis", fname = fname, cid = cid, rid = rid))

      leonSocket.send(msg)
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
        if (synthesizing) {
          val msg = JSON.stringify(l(
            module = "repair",
            action = Action.doCancel))

          leonSocket.send(msg)
        }
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
      $("#repairResults .code.solution").html(data.solCode)
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
