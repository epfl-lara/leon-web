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

import org.scalajs.jquery
import jquery.{jQuery => $, JQueryAjaxSettings, JQueryXHR, JQuery, JQueryEventObject}

import js.Dynamic.{global => g, literal => l, newInstance => jsnew}

import js.JSConverters._

@ScalaJSDefined
class ExplorationFact(val range: Range, val res: String) extends js.Object

object ExplorationFact {
  def apply(range: Range, res: String): ExplorationFact = new ExplorationFact(range, res)
}

@ScalaJSDefined
class Feature(_a: Boolean, _n: String) extends js.Object {
  var active: Boolean = _a
  val name: String = _n
}
object Feature { def apply(active: Boolean, name: String) = new Feature(active, name) }


@JSExport
object MainDelayed extends js.JSApp {
  @JSExport
  def main(): Unit = {
    $(document).ready(Main.main _)
    println("Application starting")
  }
  
  val editor = ace.edit("codebox");
  ace.require("ace/token_tooltip");
  editor.setTheme("ace/theme/chrome");
  editor.getSession().setMode("ace/mode/scala")
  editor.getSession().setUseWrapMode(true)
  editor.setShowPrintMargin(false);
  editor.setAutoScrollEditorIntoView();
  editor.setHighlightActiveLine(false);
  editor.getSession().setTabSize(2)
  $("#codebox").show()
}

@JSExport("Main")
object Main {
  import Bool._
import JQueryExtended._
import js.JSON
import leon.web.shared.Action;
import dom.alert
import dom.console
  def window = g
  val editor = MainDelayed.editor
  val aceRange = ace.require("ace/range").Range;
  
  def main() = {
    println("Just to load this code")
  }

  
  @ScalaJSDefined
  trait LocalStorage extends js.Any {
    def getItem(name: String): String
    def setItem(name: String, value: String): Unit
  }
  
  def localStorage = window.localStorage.asInstanceOf[LocalStorage]
  
  val hash = window.location.hash.asInstanceOf[js.UndefOr[String]]

  @JSExport val WS = !js.isUndefined(g.MozWebSocket) ? g.MozWebSocket | g.WebSocket
  
  @ScalaJSDefined
  trait LeonSocket extends js.Object {
    def send(message: String): Unit
    var onopen: js.Function1[JQueryEventObject, Any]
    var onmessage: js.Function1[JQueryEventObject, Any]
    var onclose: js.Function1[JQueryEventObject, Any]
    var onerror: js.Function1[JQueryEventObject, Any]
  }
  @JSExport("leonSocket") var leonSocket: LeonSocket = null

  val headerHeight = $("#title").height()+20

  var lastRange: Range = null;
  var lastDisplayedRange: Range = null;
  var lastProcessedRange: js.UndefOr[Range] = js.undefined;

  var explorationFacts = new js.Array[ExplorationFact]();

  var displayedMarker = -1;
  
  def clearExplorationFacts() = {
    lastRange = null;
    lastProcessedRange = js.undefined;

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
      val n = newResults(i);

      explorationFacts.push(ExplorationFact(
        range = jsnew(aceRange)(n.fromRow, n.fromColumn, n.toRow, n.toColumn).asInstanceOf[Range],
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
  
  @ScalaJSDefined object _features extends js.Object {
    val verification=   Feature(active= true, name= "Verification")
    val synthesis=      Feature(active= true, name= "Synthesis")
    val termination=    Feature(active= false, name= "Termination <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>")
    val presentation=   Feature(active= false, name= "Presentation Mode")
    val execution=      Feature(active= true, name= "Execution")
    val repair=         Feature(active= true, name= "Repair <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>")
  }
  
  def features = _features.asInstanceOf[js.Dictionary[Feature]]

  def displayExplorationFacts() = {
      if (_features.execution.active && explorationFacts.length > 0) {
          val lastRange = editor.selection.getRange();

          if (!lastProcessedRange.isDefined || !lastRange.isEqual(lastProcessedRange.get)) {
              var maxScore = 0.0
              var maxRes: ExplorationFact = null

              for(r <- explorationFacts) {
                  var score = 0.0;

                  val cmp = lastRange.compareRange(r.range)

                  val found = ((cmp >= -1) && (cmp <= 1));

                  if (cmp == -1) {
                      val match_s = lastRange.start
                      val match_e = r.range.end
                      val before_s = r.range.start
                      val after_e = lastRange.end

                      score = rangeScore(match_s, match_e) -
                              rangeScore(before_s, match_s) -
                              rangeScore(match_e, after_e);

                  } else if (cmp == 0) {
                      if (lastRange.containsRange(r.range)) {
                          val match_s = r.range.start
                          val match_e = r.range.end
                          val before_s = lastRange.start
                          val after_e = lastRange.end

                          score = rangeScore(match_s, match_e) -
                                  rangeScore(before_s, match_s) -
                                  rangeScore(match_e, after_e);
                      } else {
                          val match_s = lastRange.start
                          val match_e = lastRange.end
                          val before_s = r.range.start
                          val after_e = r.range.end

                          score = rangeScore(match_s, match_e) -
                                  rangeScore(before_s, match_s) -
                                  rangeScore(match_e, after_e);
                      }
                  } else if (cmp == 1) {
                      val match_s = r.range.start
                      val match_e = lastRange.end
                      val before_s = lastRange.start
                      val after_e = r.range.end

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
      val target = $(self).attr("ref")
      val sel = "#"+target

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

  val handlers = js.Dictionary.empty[Any]
  var compilationStatus = 0
  val searchFinished = false
  var context = "unknown";

  val maxHistory = 20;
  // Undo/Redo
  val backwardChanges = JSON.parse(localStorage.getItem("backwardChanges")).asInstanceOf[js.UndefOr[js.Array[String]]].getOrElse(new js.Array[String]())
  var forwardChanges  = JSON.parse(localStorage.getItem("forwardChanges")).asInstanceOf[js.UndefOr[js.Array[String]]].getOrElse(new js.Array[String]())

  def doUndo() {
    forwardChanges.push(editor.getValue());
    val code = backwardChanges.pop();
    editor.setValue(code)
    editor.selection.clearSelection();
    editor.gotoLine(0);
    recompile();
    updateUndoRedo()
  }

  def doRedo() {
    backwardChanges.push(editor.getValue());
    val code = forwardChanges.pop();
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
    val ub = $("#button-undo") 
    val rb = $("#button-redo") 

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
          val msg = JSON.stringify(
            l(action= Action.storePermaLink, module= "main", code= editor.getValue())
          )
          leonSocket.send(msg)
      }
      event.preventDefault()
  }): js.ThisFunction);

  @ScalaJSDefined
  trait HPermalink extends js.Object {
    val link: String
  }
  
  handlers("permalink") = (data: HPermalink) => {
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
      val e = $(".compilation-status")
      val codebox = $("div#codebox")
      val boxes = $(".results_table")

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
  @ScalaJSDefined
  trait HCompilationProgress extends js.Object { val total: Float; val current: Float }
  handlers("compilation_progress") = (data: HCompilationProgress ) => {
    updateCompilationProgress(Math.round((data.current*100)/data.total))
  }
  @ScalaJSDefined
  trait HCompilation extends js.Object { val status: String}
  handlers("compilation") = (data: HCompilation) => {
      if(data.status == "success") {
          updateCompilationStatus("success")
      } else {
          updateCompilationStatus("failure")
      }
  }
  
  @ScalaJSDefined 
  trait HMoveCursor extends js.Object { val line: Double }

  handlers("move_cursor") = (data: HMoveCursor) => {
    editor.selection.clearSelection();
    editor.gotoLine(data.line);
  }

  

  val localFeatures = localStorage.getItem("leonFeatures")
  if (localFeatures != null) {
    val locFeatures = JSON.parse(localFeatures).asInstanceOf[js.Dictionary[Feature]] //TODO: Better serialization
    for ((f, locFeature) <- locFeatures) {
      features.get(f) match {
        case Some(feature) =>
          feature.active = locFeature.active
        case None =>
      }
    }
  }

  val fts = $("#params-panel ul")
  for ((f, feature) <- features) {
      fts.append("""<li><label class="checkbox"><input id="feature-""""+f+" class=\"feature\" ref=\""+f+"\" type=\"checkbox\""+(feature.active ? """ checked="checked"""" | "")+">"+feature.name+"</label></li>")
  }

  $(".feature").click(((self: Element) => {
      val f = $(self).attr("ref")
      features(f).active = !features(f).active

      val msg = JSON.stringify(
        l(action= Action.featureSet, module= "main", feature= f, active= features(f).active)
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
  trait Status extends js.Object {
    val status: String
  }
  
  @ScalaJSDefined 
  trait VerificationDetails extends js.Object with Status {
    val vcs: VCS
  }
  
  type Html = String
  
  object overview {
      abstract class Module(name: String) { self =>
        val column: String
        def html(name: String, d: VerificationDetails): Html
        def missing(name: String): Html
        def handlers(): Unit
        modules.list += name -> self
      }
    
      object modules {
          var list = Map[String, Module]() // Defined before all modules.
        
          val verification = new Module("verification") {
              val column= "Verif."
              def html(name: String, d: VerificationDetails): Html = {
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
              }
              def missing(name: String): Html = {
                  "<td class=\"status verif\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
              }
              def handlers(): Unit = {
                  $("td.verif").click(((self: Element) => {
                      val fname = $(self).attr("fname")
                      overview.data.verification.get(fname) match {
                        case Some(d) =>
                          openVerifyDialog()
                          displayVerificationDetails(d.status, d.vcs)
                        case None =>
                          openVerifyDialog()
                          displayVerificationDetails("unknown", new VCS())
                      }
                  }): js.ThisFunction)
              }
          }
          val termination = new Module("termination") {
              val column= "Term."
              def html(name: String, d: VerificationDetails): Html = {
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
              }
              def missing(name: String): Html = {
                  "<td class=\"status termin\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
              }
              def handlers(): Unit = {
                  $("td.termin").click(((self: Element) => {
                      val fname = $(self).attr("fname")
                      openTerminationDialog()
                      overview.data.termination.get(fname) match {
                        case Some(d) =>
                          displayTerminationDetails(d.status, d)
                        case None =>
                          displayTerminationDetails("unknown", null)
                      }
                  }): js.ThisFunction);
              }
          }
      }
      
      var functions= js.Dictionary.empty[OverviewFunction]
      object data {
        var verification = js.Dictionary[VerificationDetails]()

        var termination = js.Dictionary[TerminationDetails]()
        
        def update[A](s: String, v: A) = {
          s match {
            case "verification" => verification = v.asInstanceOf[js.Dictionary[VerificationDetails]]
            case "termination" => termination = v.asInstanceOf[js.Dictionary[TerminationDetails]]
            case _ => println(s"$s data not defined")
          }
        }
        
        def apply[A](s: String): js.Dictionary[A] = {
          s match {
            case "verification" => verification.asInstanceOf[js.Dictionary[A]]
            case "termination" => termination.asInstanceOf[js.Dictionary[A]]
            case _ => throw new Exception(s"$s data not defined")
          }
        }
      }
  }
  @ScalaJSDefined
  trait OverviewFunction extends js.Object {
    val name: String
    val displayName: String
    val line: Int
    val column: Int
  }
  
  type DataOverView = js.Dictionary[OverviewFunction]
  @ScalaJSDefined 
  trait HUpdateOverview extends js.Object {
    val module: String
    val overview: DataOverView
  }

  handlers("update_overview") = (data: HUpdateOverview) => {
    if (data.module == "main") {
      overview.functions = js.Dictionary.empty[OverviewFunction];

      for ((i, fdata)  <- data.overview) {
        val fdata = data.overview(i)
        val fname = fdata.name
        overview.functions(fname) = fdata
      }
    } else {
      overview.data(data.module) = data.overview
    }

    drawOverView()
  }

  @ScalaJSDefined trait SP extends js.Object {val index: Int; val line: Int; val description: String }
  
  @ScalaJSDefined trait SynthesisOverview extends js.Object {
    val functions: js.UndefOr[js.Dictionary[js.Array[SP]]]
  }
  
  var synthesisOverview: SynthesisOverview = new SynthesisOverview {
    val functions = js.undefined
  }

  handlers("update_synthesis_overview") = (data: SynthesisOverview) => {
    if (JSON.stringify(synthesisOverview) != JSON.stringify(data)) {
      synthesisOverview = data;
      drawSynthesisOverview();
    }
  }

  def drawSynthesisOverview(): Unit = {
    val t = $("#synthesis_table")
    var html = "";

    def addMenu(index: Int, fname: String, description: String): Unit = {
        val id = """menu"""+fname+index

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

    val data = synthesisOverview

    val fnames = new js.Array[String]
    if(data.functions.isDefined) {
      val ff = data.functions.get
        for (f <- js.Object.keys(ff.asInstanceOf[js.Object])) {
          fnames.push(f)
        }
    }
    fnames.sort()

    for (fi <- 0 until fnames.length) {
        val  f = fnames(fi);
      if (!js.isUndefined(overview.functions(f))) {
        if (data.functions.get(f).length == 1) {
          val sp = data.functions.get(f)(0)
          html += "<tr><td class=\"fname problem  clicktoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
          addMenu(sp.index, f, overview.functions(f).displayName)
          html += "</td></tr>"
        } else {
          html += "<tr><td class=\"fname clicktoline\" line=\""+overview.functions(f).line+"\">"+overview.functions(f).displayName+"</td></tr>"
          val spArray = data.functions.get(f)
          for (i <- 0 until spArray.length) {
            val sp = spArray(i)
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
                action= Action.getRulesToApply,
                fname= p.attr("fname"),
                cid= p.attr("cid").toInt
            ))

            leonSocket.send(msg)
        }): js.ThisFunction)
    }

    if(data.functions.isDefined && data.functions.get.keys.nonEmpty && features("synthesis").active) {
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
  @ScalaJSDefined trait HUpdateExplorationFacts extends js.Object {val newFacts: js.Array[NewResult]}
  handlers("update_exploration_facts") = (data: HUpdateExplorationFacts) => {
      updateExplorationFacts(data.newFacts);
  }

  def drawOverView() {
    val t = $("#overview_table")
    var html: Html = "";

    html += "<tr>"
    html += "<th>Function</th>"
    for ((name, module) <- overview.modules.list) {
        if (features(name).active) {
            html += "<th>"+module.column+"</th>"
        }
    }
    html += "</tr>"

    for ((fname, fdata) <- overview.functions) {
      val fdata = overview.functions(fname)

      html += "<tr>"
      html += "  <td class=\"fname clicktoline\" line=\""+fdata.line+"\">"+fdata.displayName+"</td>"
      for ((m, mod) <- overview.modules.list) {
        if (features(m).active) {
          val data = overview.data[VerificationDetails](m)
          data.get(fname) match {
            case Some(name) =>
              html += mod.html(fname, name)
            case _ =>
              html += mod.missing(fname)
          }
        }
      }
      html += "</tr>"
    }

    t.html(html);

    for ((name, m) <- overview.modules.list) {
      m.handlers()
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
        val line = $(_this).attr("line").toDouble
        editor.gotoLine(line);
    }): js.ThisFunction)
  }

  def addHoverToLine(within: String): Unit = {
    $("").click(((_this: Element, event: JQueryEventObject) => {
      }): js.ThisFunction)
 
    $(within+" .hovertoline[line]").hover((((_this: Element, event: JQueryEventObject) => {
        val line = $(_this).attr("line").toDouble
        editor.gotoLine(line).asInstanceOf[js.Any]
    }): js.ThisFunction).asInstanceOf[js.Function1[org.scalajs.jquery.JQueryEventObject,scala.scalajs.js.Any]], handlerOut = (event: JQueryEventObject) => ().asInstanceOf[js.Any])
  }
  
  @ScalaJSDefined trait HEditor extends js.Object {
     val annotations: js.UndefOr[js.Array[Annotation]]
  }

  handlers("editor") = (data: HEditor) => {
      if (data.annotations.isDefined) {
          val annotations = data.annotations.get
          val session = editor.getSession();

          context = "unknown";

          $("#annotations").html("");

          for (i <- 0 until annotations.length) {
              val a = annotations(i);
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
                val line = a.row+1
                $("#annotations").append("<li class=\"clicktoline\" line=\""+line+"\"><code><i class=\"fa fa-warning\"></i> "+line+":"+a.text+"</code></li>")
              }
          }


          addClickToLine("#annotations");
          session.setAnnotations(annotations);
          resizeEditor();
      }
  }

  @ScalaJSDefined
  trait HNotification extends js.Object {
    val content: String
    val `type`: String
  }
  
  handlers("notification") = (data: HNotification) => {
      notify(data.content, data.`type`);
  }
  
  @ScalaJSDefined
  trait HLog extends js.Object {
    val message: String
  }

  handlers("log") = (data: HLog) => {
      val txt = $("#console")
      txt.append(data.message+"\n");
      txt.scrollTop((txt(0).scrollHeight - txt.height()).toInt)
  }

  var synthesizing = false;

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
  
  handlers("synthesis_result") = (data: HSynthesisResult) => {
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
          $("#synthesisDialog").unbind("hide.bs.modal").on("hide.bs.modal",  () => {
              if (synthesizing) {
                  val msg = JSON.stringify(l(
                      module= "main",
                      action= Action.doCancel
                  ))

                  leonSocket.send(msg)
              }
          })
      } else if (data.result == "progress") {
          val pc = (data.closed*100)/data.total;
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
            val cid    = $("#synthesisDialog").attr("cid").toInt
            val fname  = $("#synthesisDialog").attr("fname")

            $("#synthesisDialog").modal("hide")

            val msg = JSON.stringify(
              l(action= Action.doExplore, module= "synthesis", fname= fname, cid= cid, `explore-action`= "init", path= js.Array[js.Any](), ws= 0)
            )

            leonSocket.send(msg)
          })
          $("#synthesisDialog .cancelButton").hide()
          $("#synthesisDialog .closeButton").show()
          synthesizing = false;
      }
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

  handlers("synthesis_exploration") = (data: HSynthesisExploration) => {
      val d = $("#synthesisExploreDialog");

      g.prettyPrint();

      if (!d.is(":visible")) {
          d.modal("show")
      }

      var working = false

      d.unbind("hide.bs.modal").on("hide.bs.modal", () => {
        if (working) {
          val msg = JSON.stringify(l(
              module= "main",
              action= Action.doCancel
          ))

          leonSocket.send(msg)
        }
      })

      val node = d.find(".exploreBlock[path=\""+data.from.join("-")+"\"]")
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
          l(action= Action.doExplore, module= "synthesis", fname= data.fname, cid= data.cid, path= pathOf(_this), ws= wsOf(_this),
           `explore-action`= $(_this).attr("data-action"),
           select= $(_this).value().toInt
          )
        )

        leonSocket.send(msg)
      }): js.ThisFunction);

      d.find("span.knob").unbind("click").click(((self: Element) => {
        $(self).removeClass("fa-arrow-right fa-arrow-left").addClass("fa-spin fa-refresh")

        val msg = JSON.stringify(
          l(action= Action.doExplore, module= "synthesis", fname= data.fname, cid= data.cid, path= pathOf(self), ws= wsOf(self),
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

  @ScalaJSDefined trait HRulesApps extends js.Object with Status {
    val id: Int
    val name: String
  }
  
  @ScalaJSDefined trait HSynthesisRulesToApply extends js.Object {
    val fname: String
    val cid: Int
    val rulesApps: js.Array[HRulesApps]
  }
  
  handlers("synthesis_rulesToApply") = (data: HSynthesisRulesToApply) => {
      val fname       = data.fname
      val cid         = data.cid
      val rulesApps   = data.rulesApps

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
              html += """<li role="presentation" class=""""+clazz+""""><a role="menuitem" tabindex="-1" href="#" action="rule" cid=""""+cid+"""" rid=""""+app.id+"""">"""+statusIcon+app.name+"""</a></li>"""
          }
      } else {
          html += """<li role="presentation" class="temp disabled"><a role="menuitem" tabindex="-1" href="#" fname=""""+fname+"""">Not yet compiled...</a></li>"""
      }

      val selector = "#synthesis .problem[fname=\""+fname+"\"][cid=\""+cid+"\"] ul"
      $(selector+" li.temp").remove()
      $(selector).append(html)
      $(selector+" li a[action=\"search\"]").unbind("click").click(() => {
          val msg = JSON.stringify(
            l(action= Action.doSearch, module= "synthesis", fname= fname, cid= cid)
          )

          leonSocket.send(msg)
      })
      $(selector+" li a[action=\"explore\"]").unbind("click").click(() => {
          val msg = JSON.stringify(
            l(action= Action.doExplore, module= "synthesis", fname= fname, cid= cid, `explore-action`= "init", path= js.Array[Any](), ws= 0)
          )

          leonSocket.send(msg)
      })
      $(selector+" li a[action=\"rule\"]").click(((self: Element) => {
          val rid = $(self).attr("rid").toInt

          val msg = JSON.stringify(
            l(action= Action.doApplyRule, module= "synthesis",  fname= fname, cid= cid, rid= rid)
          )

          leonSocket.send(msg)
      }): js.ThisFunction)
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
  handlers("repair_result") = (data : HRepairResult) => {
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
            module= "repair",
            action= Action.doCancel
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
  
  type VCS = js.Array[VC]

  def displayVerificationDetails(status: String, vcs: VCS) {
      val pb = $("#verifyProgress")
      val pbb = pb.children(".progress-bar")

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

      val tbl = $("#verifyResults tbody")
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
          val fname = targetFunction

          val msg = JSON.stringify(
            l(action= Action.doRepair, module= "repair", fname= fname)
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
  
  handlers("verification_result") = (data: VerificationDetails) => {
      displayVerificationDetails(data.status, data.vcs)
  }
  
  @ScalaJSDefined
  trait TerminationDetails extends js.Object {
    val call: String
    val calls: js.Array[String]
    val status: String
  }

  def displayTerminationDetails(
    status: String,
    fdata: TerminationDetails) {
      val pb = $("#terminationProgress")
      val pbb = pb.children(".progress-bar")

      pbb.width("100%")
      pb.removeClass("active")
      pb.addClass("progress-bar-striped")

      pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

      val tbl = $("#terminationResults table")
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
  
  @ScalaJSDefined trait Kind extends js.Object { val kind: String }
  val receiveEvent = (event: JQueryEventObject) => {
    val data = JSON.parse(event.data.asInstanceOf[String]).asInstanceOf[Kind]
    handlers.get(data.kind) match {
      case Some(handler) =>
        handler.asInstanceOf[Function1[Kind, Any]](data);
      case _ =>
        console.log("Unknown event type: "+data.kind)
        console.log(data)
    }
  }

  var connected = false

  var lastReconnectDelay = 0;
  var reconnectIn = 0;

  val closeEvent = (event: JQueryEventObject) => {
      if (connected) {
          setDisconnected()
      }
  }

  val openEvent = (event: JQueryEventObject) => {
      if (lastReconnectDelay != 0) {
        notify("And we are back online!", "success")
        updateCompilationStatus("unknown")
        oldCode = ""
      }

      setConnected()

      for ((featureName, feature) <- features) {
          val msg = JSON.stringify(
            l(action= Action.featureSet, module= "main", feature= featureName, active= feature.active)
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
          val msg = JSON.stringify(
            l(action= Action.accessPermaLink, module= "main", link= hash.substring("#link/".length))
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
      val hash = window.location.hash.asInstanceOf[String];
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

  val errorEvent = (event: JQueryEventObject) => {
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

  @JSExport
  def connectWS() {
      println("Creating socket for "+g._leon_websocket_url)
      leonSocket = jsnew(g.WebSocket/*WS*/)(g._leon_websocket_url).asInstanceOf[LeonSocket]
      leonSocket.onopen = openEvent
      leonSocket.onmessage = receiveEvent
      leonSocket.onclose = closeEvent
      leonSocket.onerror = errorEvent
  }

  var lastChange      = 0.0;
  var lastSavedChange = lastChange;
  val timeWindow      = 2000;

  def updateSaveButton() {
      val e = $("#button-save")
      if (lastChange == lastSavedChange) {
         e.addClass("disabled"); 
      } else {
         e.removeClass("disabled"); 
      }
  }

  def notify(content: String, _type: String, fade: Double = 3000) {
    val `type` = if (_type == "error") "danger" else _type

    val note = $("<div>", l(
        `class`= "alert fade in alert-"+`type`
    )).html("""<button type="button" class="close" data-dismiss="alert">×</button>"""+content)

    $("#notifications").append(note);

    js.timers.setTimeout(fade){
      note.hide();
    }
  }

  var oldCode = ""

  def recompile() = {
      val currentCode = editor.getValue()

      if (oldCode != "" && oldCode != currentCode) {
          if (forwardChanges.length == 0) {
              storeCurrent(oldCode)
          }
      }

      if (connected && oldCode != currentCode) {
          val msg = JSON.stringify(
            l(action= Action.doUpdateCode, module= "main", code= currentCode)
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
    val now = new js.Date().getTime()

    if (lastChange < (now - timeWindow)) {
      if(lastChange > 0) { 
        recompile()
      }
      lastChange = new js.Date().getTime();
    }

    localStorage.setItem("leonEditorCode", editor.getValue());
  }

  def loadSelectedExample(): Unit = {
      val selected = $("""#example-loader""").find(":selected[id]")

      val id = selected.attr("id")
      val group = selected.attr("group")

      loadExample(group, id)
  }
  
  @ScalaJSDefined trait StatusCode extends js.Object {
    val status: String
    val code: String
  }
  
  def loadExample(group: String, id: js.UndefOr[String]) {
    if (id.isDefined) {
      $.ajax(l(
        url= "/ajax/getExample/"+group+"/"+id.get,
        dataType= "json",
        success= (data: StatusCode, textStatus: String, jqXHR: JQueryXHR) => {
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

  val editorSession = editor.getSession();

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

      val h = $(window).height()-$("#title").height()-6
      val ah = $("#annotations").height()
      val w = $("#codecolumn").width()

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

  sealed class Placement(name: String) { override def toString = name }
  object Placement {
    case object Left extends Placement("left")
    case object Right extends Placement("right")
    case object Modal extends Placement("modal")
    case object Bottom extends Placement("bottom")
  }
  
  val seenDemo = localStorage.getItem("leonSeenDemo").toInt
  @ScalaJSDefined abstract class Demo extends js.Object {
    def where: JQuery
    val title: String
    val content: String
    val placement: Placement
  }
  
  val demos = js.Array[Demo](
      new Demo {
          def where = $("")
          val placement = Placement.Modal
          val title = "Welcome to Leon!"
          val content = "Leon is an automated system for <strong>synthesizing</strong> and <strong>verifying</strong> functional Scala programs."
      },
      new Demo {
          def where = $("#example-loader")
          val placement = Placement.Left
          val title = "Select from examples"
          val content = "You can try <em>Leon</em> on a list of selected examples, covering both synthesis and verification problems."
      },
      new Demo {
          def where = $($(".ace_line_group")(13)).find("span").last()
          val placement = Placement.Right
          val title = "Edit at will"
          val content = "Feel free to modify or extend the selected examples with your own code."
      },
      new Demo {
          def where = $("#overview_table")
          val placement = Placement.Left
          val title = "Live results"
          val content = "Leon will verify your code in the background and display live verification results here."
      },
      new Demo {
          def where = $($("#overview_table td.status.verif")(2))
          val placement = Placement.Left
          val title = "Display details"
          val content = "Click on the verification status of each function to get more information!"
      },
      new Demo {
          def where = $("#synthesis_table td.problem").first()
          val placement = Placement.Left
          val title = "Synthesize"
          val content = "Click on a synthesis problem to solve it! You can either ask <em>Leon</em> to <strong>search</strong> for a solution, or perform individual steps yourself."
      },
      new Demo {
          def where = $("#button-permalink")
          val placement = Placement.Bottom
          val title = "Permalinks"
          val content = "You can generate permalinks to the editor session. If you experience any problem with the interface or if you do not understand the result, send us a link!"
      }
  );

  if (seenDemo == 0 || (seenDemo < demos.length-1)) {

    var lastDemo: JQuery = null // Do we have something better?

    def showDemo(id: Int): Unit = {
      val demo = demos(id)

      if (demo.placement == Placement.Modal) {
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

        val where = demo.where

        lastDemo = where;

        if (where.length == 0) {
          localStorage.setItem("leonSeenDemo", (id+1).toString)
          hideDemo(id)
          showDemo(id+1)
          return;
        }

        val progress = (for (i <- 0 until (demos.length-1)) yield {
          if (i < id) {
            """<i class="fa fa-circle"></i>"""
          } else {
            """<i class="fa fa-circle-o"></i>"""
          }
        }).mkString("")

        where.popover(l(
            html = true,
            placement = demo.placement.toString,
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
        val demo = demos(id)

        if (demo.placement == "modal") {
            $("#demoPane").modal("hide")
            $("#demoPane").unbind("hidden").on("hidden", () => { $("demoPane").remove() })
        } else {
            lastDemo.popover("destroy")
        }
    }

    val toShow = (seenDemo != 0) ? seenDemo | 0;
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