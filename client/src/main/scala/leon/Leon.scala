package leon

import japgolly.scalajs.react.React
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import dom.html.Element
import dom.document
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.collection.mutable.{HashMap => MMap}
import js.annotation._
import com.scalawarrior.scalajs.ace._
import org.scalajs.jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}

import js.Dynamic.{global => g, literal => l}
import js.JSConverters._

@ScalaJSDefined
class ExplorationFact(val range: Range, val res: String) extends js.Object

object ExplorationFact {
  def apply(range: Range, res: String): ExplorationFact = new ExplorationFact(range, res)
}

class Feature(_a: Boolean, _n: String) {
  @JSExport var active: Boolean = _a
  @JSExport val name: String = _n
}
object Feature { def apply(active: Boolean, name: String) = new Feature(active, name).asInstanceOf[js.Any] }


@JSExport("Main")
object Main extends js.JSApp {
  import Bool._
  import JQueryExtended._
  import js.JSON
  import dom.alert
  import dom.console
  def window = g

  @JSExport
  def main(): Unit = {
    println("Application starting")
  }
  
  @ScalaJSDefined
  trait LocalStorage extends js.Any {
    def getItem(name: String): String
    def setItem(name: String, value: String): Unit
  }
  
  def localStorage = window.localStorage.asInstanceOf[LocalStorage]
  
  var editor = ace.edit("codebox");
  var aceRange = ace.require("ace/range").Range;
  ace.require("ace/token_tooltip");
  editor.setTheme("ace/theme/chrome");
  editor.getSession().setMode("ace/mode/scala")
  editor.getSession().setUseWrapMode(true)
  editor.setShowPrintMargin(false);
  editor.setAutoScrollEditorIntoView();
  editor.setHighlightActiveLine(false);
  editor.getSession().setTabSize(2)

  var hash = window.location.hash.asInstanceOf[js.UndefOr[String]]

  @JSExport var WS = js.isUndefined(g.MozWebSocket) ? g.MozWebSocket | g.WebSocket
  var leonSocket: js.Dynamic = null

  var headerHeight = $("#title").height()+20

  var lastRange: Range = null;
  var lastDisplayedRange: Range = null;
  var lastProcessedRange: Range = null;

  var explorationFacts = new js.Array[ExplorationFact]();

  var displayedMarker = -1;
  
  def clearExplorationFacts() = {
    lastRange = null;
    lastProcessedRange = null;

    hideHighlight();

    explorationFacts = new js.Array[ExplorationFact]();
  }
  
  def hideHighlight() = {
    if (displayedMarker > 0) {
      editor.getSession().removeMarker(displayedMarker);

      $(".leon-explore-location.ace_start").each((index: js.Any, _this: dom.Element) => 
        $(_this).tooltip("destroy").asInstanceOf[js.Any]
      );

    }

    lastDisplayedRange = null;
    displayedMarker = -1
  }
  
  @ScalaJSDefined
  trait NewResult extends js.Object {
    val fromRow: Int
    val fromColumn: Int
    val toRow: Int
    val toColumn: Int
    val result: String
  }

  def updateExplorationFacts(newResults: js.Array[NewResult]) {
    for (i <- 0 until newResults.length) {
      var n = newResults(i);

      explorationFacts.push(ExplorationFact(
        range = js.Dynamic.newInstance(aceRange)(n.fromRow, n.fromColumn, n.toRow, n.toColumn).asInstanceOf[Range],
        res = n.result
      ));
    }

    displayExplorationFacts()
  }
  
  def showHighlight(range: Range, content: String) = {
    if (range != lastDisplayedRange) {
      hideHighlight()

      lastDisplayedRange = range;

      displayedMarker = editor.getSession().addMarker(range, "leon-explore-location", "text", true);

      js.timers.setTimeout(50){
        $(".leon-explore-location.ace_start").tooltip(l(
            title = content,
            container = "#codebox",
            placement = "top",
            trigger = "manual"
        ))
        $(".leon-explore-location.ace_start").tooltip("show");
      }
    }
  }

  

  editor.getSession().on("changeScrollTop", (_ :js.Any) => {
      hideHighlight();
  });

  def rangeScore(start: Position, end: Position): Double = {
    if (start.row == end.row) {
      (end.row - start.row)*80 + end.column - start.column;
    } else {
      (end.row - start.row)*80 + end.column - start.column;
    }
  }
  
  val _features = l(
    verification=   Feature(active= true, name= "Verification"),
    synthesis=      Feature(active= true, name= "Synthesis"),
    termination=    Feature(active= false, name= "Termination <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>"),
    presentation=   Feature(active= false, name= "Presentation Mode"),
    execution=      Feature(active= true, name= "Execution"),
    repair=         Feature(active= true, name= "Repair <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>")
  )
  
  def features = _features.asInstanceOf[js.Dictionary[Feature]]

  def displayExplorationFacts() = {
      if (_features.execution.active && explorationFacts.length > 0) {
          var lastRange = editor.selection.getRange();

          if (js.isUndefined(lastProcessedRange) || !lastRange.isEqual(lastProcessedRange)) {
              var maxScore = 0.0
              var maxRes: ExplorationFact = null

              for(r <- explorationFacts) {
                  var score = 0.0;

                  var cmp = lastRange.compareRange(r.range)

                  var found = ((cmp >= -1) && (cmp <= 1));

                  if (cmp == -1) {
                      var match_s = lastRange.start
                      var match_e = r.range.end
                      var before_s = r.range.start
                      var after_e = lastRange.end

                      score = rangeScore(match_s, match_e) -
                              rangeScore(before_s, match_s) -
                              rangeScore(match_e, after_e);

                  } else if (cmp == 0) {
                      if (lastRange.containsRange(r.range)) {
                          var match_s = r.range.start
                          var match_e = r.range.end
                          var before_s = lastRange.start
                          var after_e = lastRange.end

                          score = rangeScore(match_s, match_e) -
                                  rangeScore(before_s, match_s) -
                                  rangeScore(match_e, after_e);
                      } else {
                          var match_s = lastRange.start
                          var match_e = lastRange.end
                          var before_s = r.range.start
                          var after_e = r.range.end

                          score = rangeScore(match_s, match_e) -
                                  rangeScore(before_s, match_s) -
                                  rangeScore(match_e, after_e);
                      }
                  } else if (cmp == 1) {
                      var match_s = r.range.start
                      var match_e = lastRange.end
                      var before_s = lastRange.start
                      var after_e = r.range.end

                      score = rangeScore(match_s, match_e) -
                              rangeScore(before_s, match_s) -
                              rangeScore(match_e, after_e);
                  }

                  if (found && (maxRes == null || maxScore < score)) {
                      maxScore = score
                      maxRes = r
                  }
              }

              if (maxRes != null) {
                  showHighlight(maxRes.range, maxRes.res)
              } else {
                  hideHighlight();
              }
          }

          lastProcessedRange = lastRange
      }
  }

  $("#codecolumn").mouseup((e: JQueryEventObject) => {
      displayExplorationFacts();
  }.asInstanceOf[js.Any])

  $("#codecolumn").keyup((e: JQueryEventObject) => {
      displayExplorationFacts();
  }.asInstanceOf[js.Any])

  $(".menu-button").click(((self: Element, event: JQueryEventObject) => {
      var target = $(self).attr("ref")
      var sel = "#"+target

      if ($(sel).is(":visible")) {
          $(sel).hide()
          $(self).addClass("disabled")
      } else {
          $(sel).show()
          $(self).removeClass("disabled")
      }

  }): js.ThisFunction);

  $("#button-save").click((event: JQueryEventObject) => {
      recompile()
      event.preventDefault()
  });

  $("#button-undo").click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
          doUndo()
      }
      event.preventDefault()
  }): js.ThisFunction);

  $("#button-redo").click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
          doRedo()
      }
      event.preventDefault()
  }): js.ThisFunction);

  def hasLocalStorage(): Boolean = {
    try {
      !js.isUndefined(window.localStorage) && window.localStorage != null;
    } catch {
      case e: Exception =>
      false
    }
  }

  var handlers = js.Dictionary.empty[Any]
  var compilationStatus = 0
  var searchFinished = false
  var context = "unknown";

  var maxHistory = 20;
  // Undo/Redo
  var backwardChanges = JSON.parse(localStorage.getItem("backwardChanges")).asInstanceOf[js.Array[String]]
  if (js.isUndefined(backwardChanges)) {
    backwardChanges = new js.Array[String]()
  }
  var forwardChanges  = JSON.parse(localStorage.getItem("forwardChanges").asInstanceOf[String]).asInstanceOf[js.Array[String]]
  if (js.isUndefined(forwardChanges)) {
    forwardChanges = new js.Array[String]()
  }

  def doUndo() {
    forwardChanges.push(editor.getValue());
    var code = backwardChanges.pop();
    editor.setValue(code)
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
    updateUndoRedo()
  }

  def doRedo() {
    backwardChanges.push(editor.getValue());
    var code = forwardChanges.pop();
    editor.setValue(code)
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
    updateUndoRedo()
  }

  def storeCurrent(code: String) {
    forwardChanges = new js.Array[String]()
    if (backwardChanges.length >= 1) {
      if (code != backwardChanges(backwardChanges.length-1)) {
        backwardChanges.push(code)
      }
    } else {
        backwardChanges.push(code)
    }
    updateUndoRedo()
  }

  def updateUndoRedo() {
    var ub = $("#button-undo") 
    var rb = $("#button-redo") 

    if (backwardChanges.length > 0) {
      ub.removeClass("disabled") 
    } else {
      ub.addClass("disabled") 
    }

    if (forwardChanges.length > 0) {
      rb.removeClass("disabled") 
    } else {
      rb.addClass("disabled") 
    }

    if (backwardChanges.length > maxHistory) {
      backwardChanges.splice(0, backwardChanges.length-maxHistory)
    }

    if (forwardChanges.length > maxHistory) {
      forwardChanges.splice(0, forwardChanges.length-maxHistory)
    }

    localStorage.setItem("backwardChanges", JSON.stringify(backwardChanges));
    localStorage.setItem("forwardChanges",  JSON.stringify(forwardChanges));
  }

  updateUndoRedo()

  $("#button-permalink").click(((self: Element, event: JQueryEventObject) => {
      if (!$(self).hasClass("disabled")) {
          var msg = JSON.stringify(
            l(action= "storePermaLink", module= "main", code= editor.getValue())
          )
          leonSocket.send(msg)
      }
      event.preventDefault()
  }): js.ThisFunction);

  handlers("permalink") = (data: js.Dynamic) => {
      $("#permalink-value input").value(window._leon_url+"#link/"+data.link)
      $("#permalink-value").show()
  }

  $("#button-permalink-close").click((event: JQueryEventObject) => {
      $("#permalink-value").hide()
  })

  /**
   * Compilation
   */

  def updateCompilationProgress(percents: Int) {
    $("#overview .progress-bar").css("width", percents+"%");
  }

  def updateCompilationStatus(status: String) {
      var e = $(".compilation-status")
      var codebox = $("div#codebox")
      var boxes = $(".results_table")

      e.attr("class", "compilation-status");
      $(".results_table > .overlay").remove();

      if (status == "success") {
        compilationStatus = 1

        e.addClass("success")
        e.html("""Compiled <i class="fa fa-check" title="Compilation succeeded"></i>""")


        codebox.removeClass("compilation-error")
      } else if (status == "failure") {
        compilationStatus = -1

        e.addClass("failure")
        e.html("""Compilation Failed <i class="fa fa-warning" title="Compilation failed"></i>""")

        codebox.addClass("compilation-error")

        boxes.append("""<div class="overlay" />""")
      } else if (status == "disconnected") {
        compilationStatus = 0

        e.addClass("failure")
        e.html("""Disconnected <i class="fa fa-unlink"></i>""")

        boxes.append("""<div class="overlay" />""")

      } else if (status == "unknown") {
        compilationStatus = 0

        e.html("""Compiling <i class="fa fa-refresh fa-spin" title="Compiling..."></i>""")
      } else {
        alert("Unknown status: "+status)
      }

      if (status == "unknown") {
        updateCompilationProgress(0);
      } else {
        updateCompilationProgress(100);
      }

      clearExplorationFacts();
      drawSynthesisOverview()
  }
  handlers("compilation_progress") = (data: { val total: Float; val current: Float } ) => {
    updateCompilationProgress(Math.round((data.current*100)/data.total))
  }

  handlers("compilation") = (data: { val status: String}) => {
      if(data.status == "success") {
          updateCompilationStatus("success")
      } else {
          updateCompilationStatus("failure")
      }
  }
  
  trait HMoveCursor { val line: Double }

  handlers("move_cursor") = (data: HMoveCursor) => {
    editor.selection.clearSelection();
    editor.gotoLine(data.line);
  }

  

  var localFeatures = localStorage.getItem("leonFeatures")
  if (localFeatures != null) {
    var locFeatures = JSON.parse(localFeatures) //TODO: Better serialization
    for (f <- js.Object.keys(locFeatures.asInstanceOf[js.Object])) {
      if (!js.isUndefined(features(f))) {
          features(f).active = locFeatures(f).active
      }
    }
  }

  var fts = $("#params-panel ul")
  for (f <- js.Object.keys(features.asInstanceOf[js.Object])) {
      fts.append("""<li><label class="checkbox"><input id="feature-""""+f+" class=\"feature\" ref=\""+f+"\" type=\"checkbox\""+(features(f).active ? """ checked="checked"""" | "")+">"+features(f).name+"</label></li>")
  }

  $(".feature").click(((self: Element) => {
      var f = $(self).attr("ref")
      features(f).active = !features(f).active

      var msg = JSON.stringify(
        l(action= "featureSet", module= "main", feature= f, active= features(f).active)
      )
      leonSocket.send(msg)


      localStorage.setItem("leonFeatures", JSON.stringify(features));

      recompile()

      drawOverView()
      drawSynthesisOverview()
      setPresentationMode()
  }): js.ThisFunction)

  setPresentationMode()

  @ScalaJSDefined
  trait Overview extends js.Object {
    val modules: js.Dictionary[js.Dynamic]
    val data: js.Dictionary[js.Dynamic]
    var functions: js.Dictionary[js.Dynamic]
  }
  
  trait D {
    val status: String
    val vcs: VCS
  }
  
  val overview: Overview = new Overview {
      val modules= l(
          verification= l(
              column= "Verif.",
              html= (name: String, d: D) => {
                  val vstatus = d.status match {
                    case "crashed" =>
                      """<i class="fa fa-bolt text-danger" title="Unnexpected error during verification"></i>"""
                    case "undefined" =>
                      """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
                    case "cond-valid" =>
                      """<span class="text-success" title="Conditionally valid">(<i class="fa fa-check"></i>)</span>"""
                    case "valid" =>
                      """<i class="fa fa-check text-success" title="Valid"></i>"""
                    case "invalid" =>
                      """<i class="fa fa-exclamation-circle text-danger" title="Invalid"></i>""";
                    case "timeout" =>
                      """<i class="fa fa-clock-o text-warning" title="Timeout"></i>"""
                    case _ =>
                      """<i class="fa fa-refresh fa-spin" title="Verifying..."></i>"""
                  }

                  "<td class=\"status verif\" fname=\""+name+"\">"+vstatus+"</td>"
              },
              missing= (name: String) => {
                  "<td class=\"status verif\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
              },
              handlers= () => {
                  $("td.verif").click(((self: Element) => {
                      var fname = $(self).attr("fname")
                      if (!js.isUndefined(overview.data("verification")(fname))) {
                          var d = overview.data("verification")(fname).asInstanceOf[D]

                          openVerifyDialog()

                          displayVerificationDetails(d.status, d.vcs)
                      } else {
                          openVerifyDialog()

                          displayVerificationDetails("unknown", new VCS())
                      }
                  }): js.ThisFunction)
              }
          ),
          termination= l(
              column= "Term.",
              html= (name: String, d: D) => {
                  val tstatus = d.status match {
                      case "wip" =>
                          """<i class="fa fa-refresh fa-spin" title="Checking termination..."></i>""";
                      case "terminates" =>
                          """<i class="fa fa-check text-success" title="Termination guaranteed"></i>""";
                      case "loopsfor" =>
                          """<i class="fa fa-exclamation-circle text-danger" title="Non-terminating"></i>""";
                      case "callsnonterminating" =>
                          """<span class="text-success" title="Calls non-terminating functions">(<i class="fa fa-check text-success"></i>)</span>""";
                      case "noguarantee" =>
                          """<i class="fa fa-question text-danger" title="No termination guarantee"></i>""";
                      case _ =>
                          """<i class="fa fa-question" title="Unknown" />""";
                  }

                  "<td class=\"status termin\" fname=\""+name+"\">"+tstatus+"</td>"
              },
              missing= (name: String) => {
                  "<td class=\"status termin\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
              },
              handlers= () => {
                  $("td.termin").click(((self: Element) => {
                      var fname = $(self).attr("fname")
                      openTerminationDialog()
                      if (!js.isUndefined(overview.data("termination")(fname))) {
                          var d = overview.data("termination")(fname).asInstanceOf[D]
                          displayTerminationDetails(d.status, d.asInstanceOf[{val call: String; val calls: scala.scalajs.js.Array[String]}]) // Comes from javascript. Is it correct?
                      } else {
                          displayTerminationDetails("unknown", null)
                      }
                  }): js.ThisFunction);
              }
          )
      ).asInstanceOf[js.Dictionary[js.Dynamic]]
      var functions= js.Dictionary.empty[js.Dynamic]
      val data= l(
          verification= l(),
          termination= l()
      ).asInstanceOf[js.Dictionary[js.Dynamic]]
  }
  
  type DataOverView = js.Array[{val name: String}]
  trait HUpdateOverview {
    val module: String
    val overview: DataOverView
  }

  handlers("update_overview") = (data: HUpdateOverview) => {
      if (data.module == "main") {
          overview.functions = js.Dictionary.empty[js.Dynamic];

          for (i <- 0 until data.overview.length) {
              var fdata = data.overview(i)
              var fname = fdata.name
              overview.functions(fname) = fdata.asInstanceOf[js.Dynamic]
          }
      } else {
          overview.data(data.module) = data.overview.asInstanceOf[js.Dynamic]
      }

      drawOverView()
  }

  var synthesisOverview = js.Dictionary.empty[js.Dynamic]

  handlers("update_synthesis_overview") = (data: js.Dictionary[js.Dynamic]) => {
      if (JSON.stringify(synthesisOverview) != JSON.stringify(data)) {
          synthesisOverview = data;
          drawSynthesisOverview();
      }
  }

  def drawSynthesisOverview(): Unit = {
      val t = $("#synthesis_table")
      var html = "";

      def addMenu(index: Int, fname: String, description: String): Unit = {
          var id = """menu"""+fname+index

          html += """ <div class="dropdown">"""
          html += """  <a id=""""+id+"""" href="#" role="button" class="dropdown-toggle" data-toggle="dropdown"> <i class="fa fa-magic"></i> """+description+"""</a>"""
          html += """  <ul class="dropdown-menu" role="menu" aria-labelledby=""""+id+"""">"""
          if (compilationStatus == 1) {
              html += """    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="search" cid=""""+index+"""">Search</a></li>"""
              html += """    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="explore" cid=""""+index+"""">Explore</a></li>"""
              html += """    <li role="presentation" class="divider"></li>"""
              html += """    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><img src="/assets/images/loader.gif" /></a></li>"""
          } else {
              html += """    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><i class="fa fa-ban"></i> Not compiled</a></li>"""
          }

          html += """  </ul>"""
          html += """ </div>"""
      }

      var data = synthesisOverview

      var fnames = new js.Array[String]
      for (f <- js.Object.keys(data("functions").asInstanceOf[js.Object])) {
        fnames.push(f)
      }
      fnames.sort()
      trait SP {val index: Int; val line: Int; val description: String }
      for (fi <- 0 until fnames.length) {
          var  f = fnames(fi);
        if (!js.isUndefined(overview.functions(f))) {
          if (data("functions").asInstanceOf[js.Dictionary[js.Dynamic]](f).length == 1) {
            var sp = data("functions").asInstanceOf[js.Dictionary[js.Dynamic]](f)(0).asInstanceOf[SP]
            html += "<tr><td class=\"fname problem  clicktoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
            addMenu(sp.index, f, overview.functions(f).displayName.asInstanceOf[String])
            html += "</td></tr>"
          } else {
            html += "<tr><td class=\"fname clicktoline\" line=\""+overview.functions(f).line+"\">"+overview.functions(f).displayName+"</td></tr>"
            val spArray = data("functions")(f).asInstanceOf[js.Array[SP]]
            for (i <- 0 until spArray.length) {
              var sp = spArray(i)
              html += "<tr>"
              html += "<td class=\"problem subproblem clicktoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
              addMenu(sp.index, f, sp.description)
              html += "</td></tr>"
            }
          }
        }
      }

      t.html(html);

      if (compilationStatus == 1) {
          $("#synthesis .dropdown-toggle").click(((self: Element, e: JQueryEventObject) => {
              val p = $(self).parents(".problem")

              val msg = JSON.stringify(l(
                  module= "synthesis",
                  action= "getRulesToApply",
                  fname= p.attr("fname"),
                  cid= p.attr("cid").toInt
              ))

              leonSocket.send(msg)
          }): js.ThisFunction)
      }

      if (!js.isUndefined(data("functions")) && js.Object.keys(data("functions").asInstanceOf[js.Object]).length > 0 && features("synthesis").active) {
          $("#synthesis").show()
      } else {
          $("#synthesis").hide()
      }
  }

  def setPresentationMode() {
      if(features("presentation").active) {
          $("body").addClass("presentation")
      } else {
          $("body").removeClass("presentation")
      }
      resizeEditor()
  }

  handlers("update_exploration_facts") = (data: {val newFacts: js.Array[NewResult]}) => {
      updateExplorationFacts(data.newFacts);
  }

  def drawOverView() {
    val t = $("#overview_table")
    var html = "";

    html += "<tr>"
    html += "<th>Function</th>"
    for (m <- js.Object.keys(overview.modules.asInstanceOf[js.Object])) {
        if (features(m).active) {
            html += "<th>"+overview.modules(m).column+"</th>"
        }
    }
    html += "</tr>"

    for (fname <- js.Object.keys(overview.functions.asInstanceOf[js.Object])) {
      var fdata = overview.functions(fname)

      html += "<tr>"
      html += "  <td class=\"fname clicktoline\" line=\""+fdata.line+"\">"+fdata.displayName+"</td>"
      for (m <- js.Object.keys(overview.modules.asInstanceOf[js.Object])) {
        if (features(m).active) {
          var mod = overview.modules(m)
          var data = overview.data(m).asInstanceOf[js.Dictionary[js.Any]]
          if (!js.isUndefined(data(fname))) {
            html += mod.html(fname, data(fname))
          } else {
            html += mod.missing(fname)
          }
        }
      }
      html += "</tr>"
    }

    t.html(html);

    for (m <- js.Object.keys(overview.modules.asInstanceOf[js.Object])) {
      if (!js.isUndefined(overview.modules(m)("handlers"))) {
        overview.modules(m).handlers()
      }
    }


    addClickToLine("#overview_table");
    addHoverToLine("#overview_table");

    if (js.Object.keys(overview.functions.asInstanceOf[js.Object]).length == 0) {
      t.hide()
    } else {
      t.show()
    }
  }

  def addClickToLine(within: String) {
    $(within+" .clicktoline[line]").click(((_this: Element) => {
        var line = $(_this).attr("line").toDouble
        editor.gotoLine(line);
    }): js.ThisFunction)
  }

  def addHoverToLine(within: String): Unit = {
    $("").click(((_this: Element, event: JQueryEventObject) => {
      }): js.ThisFunction)
 
    $(within+" .hovertoline[line]").hover((((_this: Element, event: JQueryEventObject) => {
        var line = $(_this).attr("line").toDouble
        editor.gotoLine(line).asInstanceOf[js.Any]
    }): js.ThisFunction).asInstanceOf[js.Function1[org.scalajs.jquery.JQueryEventObject,scala.scalajs.js.Any]], handlerOut = (event: JQueryEventObject) => ().asInstanceOf[js.Any])
  }
  
  trait HEditor {
     val annotations: js.Array[Annotation]
  }

  handlers("editor") = (data: HEditor) => {
      if (!js.isUndefined(data.annotations)) {
          var session = editor.getSession();

          context = "unknown";

          $("#annotations").html("");

          for (i <- 0 until data.annotations.length) {
              var a = data.annotations(i);
              if (a.`type` == "verification") {
                  context = "verification";
              } else if (a.`type` == "synthesis") {
                  context = "synthesis";
              }

              if (a.`type` != "info" && a.`type` != "error") {
                  session.addGutterDecoration(a.row, "leon_gutter_"+a.`type`)
                  a.`type` = "info";
              }

              if (a.`type` == "error") {
                var line = a.row+1
                $("#annotations").append("<li class=\"clicktoline\" line=\""+line+"\"><code><i class=\"fa fa-warning\"></i> "+line+":"+a.text+"</code></li>")
              }
          }


          addClickToLine("#annotations");
          session.setAnnotations(data.annotations);
          resizeEditor();
      }
  }

  handlers("notification") = (data: {val content: String; val `type`: String}) => {
      notify(data.content, data.`type`);
  }

  handlers("log") = (data: {val message: String}) => {
      var txt = $("#console")
      txt.append(data.message+"\n");
      txt.scrollTop((txt(0).scrollHeight - txt.height()).toInt)
  }

  var synthesizing = false;

  @ScalaJSDefined
  trait HSynthesisResult extends js.Any {
    val result: String;
    val cid: String;
    val fname: String;
    val problem: String;
    val closed: Double;
    val total: Double;
    val solCode: String;
    val allCode: String;
    val cursor: js.UndefOr[String]
  }
  
  handlers("synthesis_result") = (data: HSynthesisResult) => {
      var pb = $("#synthesisProgress")
      var pbb = $("#synthesisProgress .progress-bar")

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
          $("#synthesisDialog").unbind("hide.bs.modal").on("hide.bs.modal",  () => {
              if (synthesizing) {
                  var msg = JSON.stringify(l(
                      module= "main",
                      action= "doCancel"
                  ))

                  leonSocket.send(msg)
              }
          })
      } else if (data.result == "progress") {
          var pc = (data.closed*100)/data.total;
          pbb.width(pc+"%")
          pbb.html(data.closed+"/"+data.total);

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
          pbb.html(data.closed+"/"+data.total);
          pbb.addClass("progress-bar-success")

          $("#synthesisResults .code.solution").removeClass("prettyprinted")
          $("#synthesisResults .code.solution").html(data.solCode)
          $("#synthesisResults").show()
          g.prettyPrint();
          $("#synthesisDialog .exploreButton").show()
          $("#synthesisDialog .importButton").show()
          $("#synthesisDialog .importButton").unbind("click").click(() =>{
              handlers("replace_code").asInstanceOf[Function1[HReplaceCode, Unit]](new HReplaceCode { val newCode= data.allCode })
              if (data.cursor.isDefined) {
                js.timers.setTimeout(100){
                  handlers("move_cursor").asInstanceOf[Function1[HMoveCursor, Unit]](data.cursor.get.asInstanceOf[HMoveCursor])
                }
              }
          })
          $("#synthesisDialog .exploreButton").unbind("click").click(() => {
            var cid    = $("#synthesisDialog").attr("cid").toInt
            var fname  = $("#synthesisDialog").attr("fname")

            $("#synthesisDialog").modal("hide")

            val msg = JSON.stringify(
              l(action= "doExplore", module= "synthesis", fname= fname, cid= cid, `explore-action`= "init", path= js.Array[js.Any](), ws= 0)
            )

            leonSocket.send(msg)
          })
          $("#synthesisDialog .cancelButton").hide()
          $("#synthesisDialog .closeButton").show()
          synthesizing = false;
      }
  }

  handlers("synthesis_exploration") = (data: {
    val html: String
    val fname: String
    val cid: String
    val from: js.Array[String]
    val allCode: String
    val cursor: js.UndefOr[String]
  }) => {
      var d = $("#synthesisExploreDialog");

      g.prettyPrint();

      if (!d.is(":visible")) {
          d.modal("show")
      }

      var working = false

      d.unbind("hide.bs.modal").on("hide.bs.modal", () => {
        if (working) {
          var msg = JSON.stringify(l(
              module= "main",
              action= "doCancel"
          ))

          leonSocket.send(msg)
        }
      })

      var node = d.find(".exploreBlock[path=\""+data.from.join("-")+"\"]")
      node.replaceWith(data.html)
      g.prettyPrint();

      var wsOf = (e: Element) => {
        var b = $(e).closest(".exploreBlock")
        b.attr("ws").toInt
      }

      var pathOf = (e: Element) => {
        var b = $(e).closest(".exploreBlock")
        var path = js.Array[Int]()
        if (b.attr("path") != "") {
          path = b.attr("path").split("-").toJSArray.map((e: String) => e.toInt)
        }
        path
      }

      d.find("""select[data-action="select-alternative"]""").unbind("change").change(((_this: Element) => {

        $(_this).after(""" <span class="fa fa-spin fa-circle-o-notch"></span>""");
        var msg = JSON.stringify(
          l(action= "doExplore", module= "synthesis", fname= data.fname, cid= data.cid, path= pathOf(_this), ws= wsOf(_this),
           `explore-action`= $(_this).attr("data-action"),
           select= $(_this).value().toInt
          )
        )

        leonSocket.send(msg)
      }): js.ThisFunction);

      d.find("span.knob").unbind("click").click(((self: Element) => {
        $(self).removeClass("fa-arrow-right fa-arrow-left").addClass("fa-spin fa-refresh")

        var msg = JSON.stringify(
          l(action= "doExplore", module= "synthesis", fname= data.fname, cid= data.cid, path= pathOf(self), ws= wsOf(self),
           `explore-action`= $(self).attr("data-action")
          )
        )


        leonSocket.send(msg)

        working = true
      }): js.ThisFunction);

      d.find(".importButton").unbind("click").click(() => {
          handlers("replace_code").asInstanceOf[Function1[HReplaceCode, Unit]](new HReplaceCode { val newCode= data.allCode })
          if (data.cursor.isDefined) {
            js.timers.setTimeout(100){
              handlers("move_cursor").asInstanceOf[Function1[HMoveCursor, Unit]](data.cursor.get.asInstanceOf[HMoveCursor])
            }
          }
      })
  }

  trait HSynthesisRulesToApply {
    val fname: String
    val cid: String
    val rulesApps: js.Array[{val status: String; val id: String; val name: String}]
  }
  
  handlers("synthesis_rulesToApply") = (data: HSynthesisRulesToApply) => {
      var fname       = data.fname
      var cid         = data.cid
      var rulesApps   = data.rulesApps

      var html = "";

      // Start by removing temp content
      if (compilationStatus == 1) {
          for (i <- 0 until rulesApps.length) {
              var app = rulesApps(i);
              var statusIcon = ""
              var clazz = "temp"

              if (app.status == "closed") {
                  statusIcon = """<i class="fa fa-exclamation-circle"></i> """
                  clazz += " disabled"
              }
              html += """<li role="presentation" class=""""+clazz+""""><a role="menuitem" tabindex="-1" href="#" action="rule" cid=""""+cid+"""" rid=""""+app.id+"""">"""+statusIcon+app.name+"""</a></li>"""
          }
      } else {
          html += """<li role="presentation" class="temp disabled"><a role="menuitem" tabindex="-1" href="#" fname=""""+fname+"""">Not yet compiled...</a></li>"""
      }

      var selector = "#synthesis .problem[fname=\""+fname+"\"][cid=\""+cid+"\"] ul"
      $(selector+" li.temp").remove()
      $(selector).append(html)
      $(selector+" li a[action=\"search\"]").unbind("click").click(() => {
          var msg = JSON.stringify(
            l(action= "doSearch", module= "synthesis", fname= fname, cid= cid)
          )

          leonSocket.send(msg)
      })
      $(selector+" li a[action=\"explore\"]").unbind("click").click(() => {
          var msg = JSON.stringify(
            l(action= "doExplore", module= "synthesis", fname= fname, cid= cid, `explore-action`= "init", path= js.Array[Any](), ws= 0)
          )

          leonSocket.send(msg)
      })
      $(selector+" li a[action=\"rule\"]").click(((self: Element) => {
          var rid = $(self).attr("rid").toInt

          var msg = JSON.stringify(
            l(action= "doApplyRule", module= "synthesis",  fname= fname, cid= cid, rid= rid)
          )

          leonSocket.send(msg)
      }): js.ThisFunction)
  }

  handlers("repair_result") = (data : {
    val result: String
    val progress: String
    val error: String
    val focused: String
    val success: String
    val solCode: String
    val allCode: String
    val cursor: js.UndefOr[String]
  }) => {
    var pb = $("#repairProgress")
    var pbb = $("#repairProgress .progress-bar")

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
          var msg = JSON.stringify(l(
            module= "repair",
            action= "doCancel"
          ))

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
          handlers("replace_code").asInstanceOf[Function1[HReplaceCode, Unit]](new HReplaceCode { val newCode= data.allCode })
          if (data.cursor.isDefined) {
            js.timers.setTimeout(100){
              handlers("move_cursor").asInstanceOf[Function1[HMoveCursor, Unit]](data.cursor.get.asInstanceOf[HMoveCursor])
            }
          }
      })
      $("#repairDialog .cancelButton").hide()
      $("#repairDialog .closeButton").show()
    }
  }
  
  trait VC extends js.Any {
    val status: String
    val fun: String
    val kind: String
    val time: Double
    val counterExample: js.UndefOr[js.Dictionary[String]]
    val execution: js.UndefOr[{ val result: String; val output: String}]
  }
  
  type VCS = js.Array[VC]

  def displayVerificationDetails(status: String, vcs: VCS) {
      var pb = $("#verifyProgress")
      var pbb = pb.children(".progress-bar")

      pbb.width("100%")
      pb.removeClass("active")
      pb.addClass("progress-bar-striped")

      pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

      var canRepair = false

      status match {
      case "cond-valid" =>
        pbb.html("Conditionally Valid!")
        pbb.addClass("progress-bar-warning")

      case "valid" =>
        pbb.html("Valid!")
        pbb.addClass("progress-bar-success")

      case "invalid" =>
        pbb.html("Invalid!")
        pbb.addClass("progress-bar-danger")
        canRepair = true

      case "unknown" =>
        pbb.html("Unknown ?!")
        pbb.addClass("progress-bar-warning")

      case "timeout" =>
        pbb.html("Timeout!")
        pbb.addClass("progress-bar-warning")
          
      case _ =>
      }

      var tbl = $("#verifyResults tbody")
      tbl.html("");

      var targetFunction: String = null
      for (i <- 0 until vcs.length) {
        val vc = vcs(i)
        targetFunction = vc.fun
        var icon = "check"
        if (vc.status == "invalid") {
            icon = "warning"
        } else if (vc.status == "unknown") {
            icon = "question"
        } else if (vc.status == "timeout") {
            icon = "clock-o"
        }

        var clas = "success"
        if (vc.status == "invalid") {
          clas = "danger"
        } else if (vc.status == "unknown" || vc.status == "timeout") {
          clas = "warning"
        }

        tbl.append("<tr class=\""+clas+"\"> <td>"+vc.fun+"</td> <td>"+vc.kind+"</td> <td><i class=\"fa fa-"+icon+"\"></i> "+vc.status+"</td> <td>"+vc.time+"</td> </tr>")

        if (vc.counterExample.isDefined) {
          var html = "<tr class=\""+clas+" counter-example\"><td colspan=\"4\">"
          html += "<div>"
          html += "  <p>The following inputs violate the VC:</p>";
          html += "  <table class=\"input\">";
          for (v <- js.Object.keys(vc.counterExample.get.asInstanceOf[js.Object])) {
              html += "<tr><td>"+v+"</td><td>&nbsp;:=&nbsp;</td><td>"+vc.counterExample.get(v)+"</td></tr>";
          }
          html += "  </table>"

          if (vc.execution.isDefined && vc.execution.get.result == "success" && features("execution").active) {
              html += "  <p>It produced the following output:</p>";
              html += "  <div class=\"output\">"+vc.execution.get.output+"</div>"
          }

          html += "    </div>"
          html += "  </td>"
          html += "</tr>"

          tbl.append(html)
        }
      }

      if (vcs.length == 0) {
        tbl.append("<tr class=\"empty\"><td colspan=\"4\"><div>No VC found</div></td></tr>")
      }

      $("div[aria-describedby='verifyDialog'] span.ui-button-text").html("Close")

      if (canRepair && features("repair").active) {
        $(".repairButton").unbind("click").click(() => {
          var fname = targetFunction

          var msg = JSON.stringify(
            l(action= "doRepair", module= "repair", fname= fname)
          )

          leonSocket.send(msg)

          $("#verifyDialog").modal("hide")
        });
        $(".repairButton").show();
      } else {
        $(".repairButton").hide();
      }

      $("#verifyResults").show("fade");
  }

  trait HVerification_Result {val status: String; val vcs: VCS}
  
  handlers("verification_result") = (data: HVerification_Result) => {
      displayVerificationDetails(data.status, data.vcs)
  }

  def displayTerminationDetails(
    status: String,
    fdata: {val call: String; val calls: js.Array[String]}) {
      var pb = $("#terminationProgress")
      var pbb = pb.children(".progress-bar")

      pbb.width("100%")
      pb.removeClass("active")
      pb.addClass("progress-bar-striped")

      pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

      var tbl = $("#terminationResults table")
      tbl.html("");

      status match {
          case "terminates" =>
              pbb.html("Terminates!")
              pbb.addClass("progress-bar-success")
              tbl.append("""<tr class="success"> <td>This function terminates for all inputs.</td> </tr>""")

          case "loopsfor" =>
              pbb.html("Non-terminating!")
              pbb.addClass("progress-bar-danger")
              var html = """<tr class="danger counter-example"><td><div>"""
              html += "<p>The function does not terminate for the following call:</p>";
              html += "<table class=\"input\">";
              html += "  <tr><td>"+fdata.call+"</td></tr>";
              html += "</table>"
              html += "</div></td></tr>"
              tbl.append(html)

          case "callsnonterminating" =>
              pbb.html("Calls non-terminating functions!")
              pbb.addClass("progress-bar-warning")
              var html = """<tr class="warning counter-example"><td><div>"""
              html += "<p>The function calls the following non-terminating function(s):</p>";
              html += "<table class=\"input\">";
              for (i <- 0 until fdata.calls.length) {
                  html += "<tr><td>"+fdata.calls(i)+"</td></tr>";
              }
              html += "</table>"
              html += "</div></td></tr>"
              tbl.append(html)

          case "noguarantee" =>
              pbb.html("No guarantee!")
              pbb.addClass("progress-bar-warning")
              tbl.append("""<tr class="warning"> <td>Leon could not determine whether or not this function terminates.</td> </tr>""")

          case _ =>
              pbb.html("Unknown!")
              pbb.addClass("progress-bar-warning")
      }

      $("div[aria-describedby='terminationDialog'] span.ui-button-text").html("Close")
      $("#terminationResults").show("fade");
  }

  def error(msg: String) {
      alert(msg);
  }
  
  trait Kind extends js.Any { val kind: String }
  var receiveEvent = (event: JQueryEventObject) => {
      
      var data = JSON.parse(event.data.asInstanceOf[String]).asInstanceOf[Kind]
      if (!js.isUndefined(handlers(data.kind))) {
          handlers(data.kind).asInstanceOf[Function1[Kind, Unit]](data);
      } else {
          console.log("Unknown event type: "+data.kind)
          console.log(data)
      }
  }

  var connected = false

  var lastReconnectDelay = 0;
  var reconnectIn = 0;

  var closeEvent = (event: JQueryEventObject) => {
      if (connected) {
          setDisconnected()
      }
  }

  var openEvent = (event: JQueryEventObject) => {
      if (lastReconnectDelay != 0) {
        notify("And we are back online!", "success")
        updateCompilationStatus("unknown")
        oldCode = ""
      }

      setConnected()

      for (f <- js.Object.keys(_features)) {
          var msg = JSON.stringify(
            l(action= "featureSet", module= "main", feature= f, active= features(f).active)
          )

          leonSocket.send(msg)
      }

      if(hash.isDefined) {
          loadStaticLink(hash.get)
      } else {
          recompile()
      }
  }

  def loadStaticLink(hash: String) {
      if (hash.indexOf("#link/") == 0) {
          var msg = JSON.stringify(
            l(action= "accessPermaLink", module= "main", link= hash.substring("#link/".length))
          )

          leonSocket.send(msg)
          window.location.hash = ""
      }
      if (hash.indexOf("#demo/") == 0) {
          loadExample("demo", hash.substring("#demo/".length))
          window.location.hash = ""
      }
  }

  $(window).on("hashchange", () => {
      var hash = window.location.hash.asInstanceOf[String];
      loadStaticLink(hash);
  });

  def setDisconnected() {
      connected = false
      updateCompilationStatus("disconnected")
      lastReconnectDelay = 5;
      reconnectIn = lastReconnectDelay;

      checkDisconnectStatus()
  }

  def setConnected() {
      connected = true

      $("#connectError").hide();
      $("#disconnectError").hide();

      lastReconnectDelay = 0;
      reconnectIn = -1;
  }

  def checkDisconnectStatus() {
      if (reconnectIn == 0) {
          reconnectIn = -1;
          $("#disconnectError #disconnectMsg").html("Attempting reconnection...");

          connectWS()

          // If still not connected after 2 seconds, consider failed
          js.timers.setTimeout(2000){
              if (!connected) {
                  if (lastReconnectDelay == 0) {
                      lastReconnectDelay = 5;
                  } else {
                      lastReconnectDelay *= 2;
                  }

                  reconnectIn = lastReconnectDelay;
              }
          }
      } else if (reconnectIn > 0) {
          $("#disconnectError #disconnectMsg").html("Retrying in "+reconnectIn+""" seconds... <button id="tryReconnect" class="btn btn-danger btn-xs">Try now</button>""");

          $("#tryReconnect").click(() => {
              reconnectIn = 0;
              checkDisconnectStatus();
          })

          $("#disconnectError").show().alert();

          reconnectIn -= 1;
      }
  }

  js.timers.setInterval(2000){ checkDisconnectStatus() };

  var errorEvent = (event: JQueryEventObject) => {
      console.log("ERROR")
      console.log(event)
  }

  connectWS()
  js.timers.setTimeout(3000){
      if (!connected) {
          $("#disconnectError").hide();
          $("#connectError").show().alert();
      }
  }

  def connectWS() {
      leonSocket = js.Dynamic.newInstance(WS)(g._leon_websocket_url)
      leonSocket.onopen = openEvent _
      leonSocket.onmessage = receiveEvent _
      leonSocket.onclose = closeEvent _
      leonSocket.onerror = errorEvent _
  }

  var lastChange      = 0.0;
  var lastSavedChange = lastChange;
  var timeWindow      = 2000;

  def updateSaveButton() {
      var e = $("#button-save")
      if (lastChange == lastSavedChange) {
         e.addClass("disabled"); 
      } else {
         e.removeClass("disabled"); 
      }
  }

  def notify(content: String, _type: String, fade: Double = 3000) {
    val `type` = if (_type == "error") "danger" else _type

    var note = $("<div>", l(
        `class`= "alert fade in alert-"+`type`
    )).html("""<button type="button" class="close" data-dismiss="alert">×</button>"""+content)

    $("#notifications").append(note);

    js.timers.setTimeout(fade){
      note.hide();
    }
  }

  var oldCode = ""

  def recompile() = {
      var currentCode = editor.getValue()

      if (oldCode != "" && oldCode != currentCode) {
          if (forwardChanges.length == 0) {
              storeCurrent(oldCode)
          }
      }

      if (connected && oldCode != currentCode) {
          var msg = JSON.stringify(
            l(action= "doUpdateCode", module= "main", code= currentCode)
          )
          oldCode = currentCode;
          lastSavedChange = lastChange;
          updateSaveButton();
          leonSocket.send(msg)
          updateCompilationStatus("unknown")
          updateCompilationProgress(0)
      }
  }

  def onCodeUpdate() {
    var now = new js.Date().getTime()

    if (lastChange < (now - timeWindow)) {
      if(lastChange > 0) { 
        recompile()
      }
      lastChange = new js.Date().getTime();
    }

    localStorage.setItem("leonEditorCode", editor.getValue());
  }

  def loadSelectedExample(): Unit = {
      var selected = $("""#example-loader""").find(":selected[id]")

      var id = selected.attr("id")
      var group = selected.attr("group")

      loadExample(group, id)
  }
  def loadExample(group: String, id: String) {
    if (!js.isUndefined(id)) {
      $.ajax(l(
        url= "/ajax/getExample/"+group+"/"+id,
        dataType= "json",
        success= (data: {
          val status: String
          val code: String
        }, textStatus: String, jqXHR: JQueryXHR) => {
          if (data.status == "success") {
            storeCurrent(editorSession.getValue())
            editor.setValue(data.code);
            editor.selection.clearSelection();
            editor.gotoLine(0);
            recompile();
            $("#example-loader").get(0).selectedIndex = 0;
          } else {
            notify("Loading example failed :(", "error")
          }
        },
        error= (jqXHR: JQueryXHR, textStatus: String, errorThrown: js.Dynamic) => {
          notify("Loading example failed :(", "error")
        }
      ).asInstanceOf[JQueryAjaxSettings]);
    }
  }

  $("#example-loader").change(loadSelectedExample _);

  var editorSession = editor.getSession();

  editor.commands.addCommand(js.use(new js.Object {
    var name= "save"
    var bindKey= l(win= "Ctrl-S",  mac= "Command-S").asInstanceOf[js.Any]
    var exec= ((editor: Editor) => {
      recompile()
    }).asInstanceOf[js.Function]
    var readOnly= true
  }).as[EditorCommand]);

  editor.commands.removeCommand("replace");
  editor.commands.removeCommand("transposeletters");

  editorSession.on("change", (e: js.Any) => {
      lastChange = new js.Date().getTime();
      updateSaveButton();
      js.timers.setTimeout(timeWindow+50){ onCodeUpdate }
      ().asInstanceOf[js.Any]
  });

  def resizeEditor() {

      var h = $(window).height()-$("#title").height()-6
      var ah = $("#annotations").height()
      var w = $("#codecolumn").width()

      $("#codecolumn").height(h);
      $("#panelscolumn").height(h);
      $("#leoninput").height(h-ah).width(w);
      $("#codebox").height(h-ah).width(w);

      editor.resize();
  };

  $(window).resize(resizeEditor);

  resizeEditor();

  @ScalaJSDefined
  trait HReplaceCode extends js.Object {val newCode: String}

  handlers("replace_code") = (data: HReplaceCode) => {
    storeCurrent(editorSession.getValue())
    editorSession.setValue(data.newCode)
    recompile()
  }

  var currentMousePos = l(x= -1, y= -1);

  $(document).mousemove((event: JQueryEventObject) => {
    currentMousePos = l(x= event.pageX, y= event.pageY);
  }.asInstanceOf[js.Any]);

  def openVerifyDialog() {
    $("#verifyDialog").modal("show")
  }

  def openTerminationDialog() {
    $("#terminationDialog").modal("show")
  }

  var storedCode = localStorage.getItem("leonEditorCode")

  var seenDemo = localStorage.getItem("leonSeenDemo").toInt
  var demos = js.Array(
      l(
          placement = "modal",
          title = "Welcome to Leon!",
          content = "Leon is an automated system for <strong>synthesizing</strong> and <strong>verifying</strong> functional Scala programs."
      ),
      l(
          where = () => $("#example-loader"),
          placement = "left",
          title = "Select from examples",
          content = "You can try <em>Leon</em> on a list of selected examples, covering both synthesis and verification problems."
      ),
      l(
          where = () => $($(".ace_line_group")(13)).find("span").last(),
          placement = "right",
          title = "Edit at will",
          content = "Feel free to modify or extend the selected examples with your own code."
      ),
      l(
          where = () => $("#overview_table"),
          placement = "left",
          title = "Live results",
          content = "Leon will verify your code in the background and display live verification results here."
      ),
      l(
          where = () => $($("#overview_table td.status.verif")(2)),
          placement = "left",
          title = "Display details",
          content = "Click on the verification status of each function to get more information!"
      ),
      l(
          where = () => $("#synthesis_table td.problem").first(),
          placement = "left",
          title = "Synthesize",
          content = "Click on a synthesis problem to solve it! You can either ask <em>Leon</em> to <strong>search</strong> for a solution, or perform individual steps yourself."
      ),
      l(
          where = () => $("#button-permalink"),
          placement = "bottom",
          title = "Permalinks",
          content = "You can generate permalinks to the editor session. If you experience any problem with the interface or if you do not understand the result, send us a link!"
      )
  );

  if (seenDemo == 0 || (seenDemo < demos.length-1)) {

    var lastDemo: js.Dynamic = null

    def showDemo(id: Int): Unit = {
      var demo = demos(id)

      if (demo.placement == "modal") {
        // Assume only the first demo is modal
        var html  = """<div id="demoPane" class="modal fade" tabindex="-1" role="dialog" aria-labelledby="demoModal" aria-hidden="true" data-backdrop="static">"""
        html     += """  <div class="modal-dialog">"""
        html     += """    <div class="modal-content">"""
        html     += """      <div class="modal-header">"""
        html     += """        <button type="button" class="close" demo-action="close" data-dismiss="modal" aria-hidden="true">×</button>"""
        html     += """        <h3 id="demoModal">"""+demo.title+"""</h3>"""
        html     += """      </div>"""
        html     += """      <div class="modal-body">"""
        html     += """        """+demo.content
        html     += """      </div>"""
        html     += """      <div class="modal-footer">"""
        html     += """        <button class="btn btn-success" data-dismiss="modal" aria-hidden="true" demo-action="next">Take the tour <i class="fa fa-play"></i></button>"""
        html     += """        <button class="btn" data-dismiss="modal" aria-hidden="true" demo-action="close">No thanks</button>"""
        html     += """      </div>"""
        html     += """    </div>"""
        html     += """  </div>"""
        html     += """</div>"""

        $("body").append(html);

        $("#demoPane").modal("show")

        var action = "close"

        $("#demoPane button").click(((self: Element) => {
            action = $(self).attr("demo-action")
            hideDemo(id)
        }): js.ThisFunction)

        $("#demoPane").unbind("hide.bs.modal").on("hide.bs.modal", () => {
            if (action == "next") {
                localStorage.setItem("leonSeenDemo", (id+1).toString)
                js.timers.setTimeout(500){ showDemo(id+1) }
            } else {
                localStorage.setItem("leonSeenDemo", 100.toString)
            }
        })

      } else {
        var content = """<div id="demoPane" class="demo">"""
        content += demo.content
        content += """  <div class="demo-nav">"""
        if (id == demos.length-1) {
            // last demo
            content += """    <button class="btn btn-success" demo-action="close">Ok!</button>""";
        } else {
            content += """    <button class="btn" demo-action="close">Got it</button>""";
            content += """    <button class="btn btn-success" demo-action="next">Next <i class="fa fa-forward"></i></button>""";
        }
        content += """  </div>"""
        content += """</div>"""

        var where = demo.where()

        lastDemo = where;

        if (where.length == 0) {
          localStorage.setItem("leonSeenDemo", (id+1).toString)
          hideDemo(id)
          showDemo(id+1)
          return;
        }

        var progress = ""
        for (i <- 0 until (demos.length-1)) {
          if (i < id) {
            progress += """<i class="fa fa-circle"></i>"""
          } else {
            progress += """<i class="fa fa-circle-o"></i>"""
          }
        }

        where.popover(l(
            html = true,
            placement = demo.placement,
            trigger = "manual",
            title = """<span class="demo-progress">"""+progress+"""</span>"""+demo.title,
            content = content,
            container = "body"
        ))

        where.popover("show")

        $("#demoPane button[demo-action=\"close\"]").click(() => {
            localStorage.setItem("leonSeenDemo", 100.toString)
            hideDemo(id)
        })

        $("#demoPane button[demo-action=\"next\"]").click(() => {
            localStorage.setItem("leonSeenDemo", (id+1).toString)
            hideDemo(id)
            showDemo(id+1)
        })
      }
    }

    def hideDemo(id: Int): Unit = {
        var demo = demos(id)

        if (demo.placement == "modal") {
            $("#demoPane").modal("hide")
            $("#demoPane").unbind("hidden").on("hidden", () => { $("demoPane").remove() })
        } else {
            lastDemo.popover("destroy")
        }
    }

    var toShow = (seenDemo != 0) ? seenDemo | 0;
    if (toShow != 0) {
        js.timers.setTimeout(1000){ showDemo(toShow) }
    } else {
        showDemo(toShow)
    }

    storedCode = null
  }

  if (storedCode != null) {
    editor.setValue(storedCode);
    editor.selection.clearSelection();
    editor.gotoLine(0);
  }
  println("Loaded entirely")
  /*
  snowStorm.snowColor = "#ddddff";
  snowStorm.vMaxX = 2;
  snowStorm.vMaxY = 2;
  snowStorm.useTwinkleEffect = false;
  snowStorm.flakesMinActive = 350;
  snowStorm.flakesMaxActive = 350;
  snowStorm.followMouse = false;
  snowStorm.stop();
  */
}