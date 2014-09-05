var editor = null;

$(document).ready(function() {
    editor = ace.edit("codebox");
    var aceRange = ace.require("ace/range").Range;
    ace.require("ace/token_tooltip");
    editor.setTheme("ace/theme/chrome");
    editor.getSession().setMode("ace/mode/scala")
    editor.getSession().setUseWrapMode(true)
    editor.setShowPrintMargin(false);
    editor.setAutoScrollEditorIntoView();
    editor.setHighlightActiveLine(false);
    editor.getSession().setTabSize(2)


    var hash = window.location.hash

    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var leonSocket = null

    var headerHeight = $("#title").height()+20

    var lastRange = null;
    var lastProcessedRange = null;
    var lastDisplayedRange = null;

    var explorationFacts = [ ];

    var lastMarker = -1;

    function updateExplorationFacts(newResults) {
        lastRange = null;
        lastProcessedRange = null;

        hideHighlight();

        lastMarker = -1

        //console.log("New Exploration facts: ");
        explorationFacts = [];
        for (i in newResults) {
            var n = newResults[i];

            explorationFacts[i] = {
                range: new aceRange(n.fromRow, n.fromColumn, n.toRow, n.toColumn),
                res: n.result
            };

            //console.log(" - "+explorationFacts[i].range.toString()+"   -->   "+n.result)
        }

        displayExplorationFacts()
    }

    function hideHighlight() {
        if (lastMarker >= 0) {
            editor.getSession().removeMarker(lastMarker);
            var found = false;
            $(".leon-explore-location.ace_start").each(function() {
              found = true;
              $(this).tooltip("destroy");
            });

            $("#codebox .tooltip.in").remove();

            lastMarker = -1;
        }
        lastDisplayedRange = null;
    }

    editor.getSession().on("changeScrollTop", function() {
        hideHighlight();
    });

    function rangeScore(start, end) {
        if (start.row == end.row) {
            return (end.row - start.row)*80 + end.column - start.column;
        } else {
            return (end.row - start.row)*80 + end.column - start.column;
        }
    }

    function displayExplorationFacts() {
        if (features["execution"].active) {
            var lastRange = editor.selection.getRange();

            if (!lastProcessedRange || !lastRange.isEqual(lastProcessedRange)) {
                var maxScore = 0
                var maxRes = null

                for(i in explorationFacts) {
                    var r = explorationFacts[i];

                    var score = 0;

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

                    if (found && (maxRes === null || maxScore < score)) {
                        maxScore = score
                        maxRes = r
                    }
                }

                if (maxRes !== null) {
                    showHighlight(maxRes.range, maxRes.res)
                } else {
                    hideHighlight();
                }
            }

            lastProcessedRange = lastRange
        }
    }

    $("#codecolumn").mouseup(function() {
        displayExplorationFacts();
    })

    $("#codecolumn").keyup(function() {
        displayExplorationFacts();
    })

    function updateExplorationHighlights() {
        if (compilationStatus != 1) {
            explorationFacts = [];
            hideHighlight();
        }
    }

    function showHighlight(range, content) {
        if (range != lastDisplayedRange) {
            if (lastMarker >= 0) {
                editor.getSession().removeMarker(lastMarker);
                $(".leon-explore-location.ace_start").tooltip("destroy");
            }

            lastDisplayedRange = range;

            lastMarker = editor.getSession().addMarker(range, "leon-explore-location", "text", true);

            setTimeout(function() {
                $(".leon-explore-location.ace_start").tooltip({
                    title: content,
                    container: "#codebox",
                    placement: "top",
                    trigger: "manual"
                })
                $(".leon-explore-location.ace_start").tooltip("show");
            }, 50);
        }
    }

    $(".menu-button").click(function(event) {
        var target = $(this).attr("ref")
        var sel = "#"+target

        if ($(sel).is(":visible")) {
            $(sel).hide()
            $(this).addClass("disabled")
        } else {
            $(sel).show()
            $(this).removeClass("disabled")
        }

    });

    $("#button-save").click(function(event) {
        recompile()
        event.preventDefault()
    });

    $("#button-undo").click(function(event) {
        if (!$(this).hasClass("disabled")) {
            doUndo()
        }
        event.preventDefault()
    });

    $("#button-redo").click(function(event) {
        if (!$(this).hasClass("disabled")) {
            doRedo()
        }
        event.preventDefault()
    });

    function hasLocalStorage() {
      try {
        return 'localStorage' in window && window['localStorage'] !== null;
      } catch (e) {
        return false;
      }
    }

    var handlers = [];
    var compilationStatus = 0
    var searchFinished = false
    var context = "unknown";

    // Undo/Redo
    var backwardChanges = []
    var forwardChanges  = []

    function doUndo() {
      forwardChanges.push(editor.getValue());
      var code = backwardChanges.pop();
      editor.setValue(code)
      editor.selection.clearSelection();
      editor.gotoLine(0);
      recompile();
      updateUndoRedo()
    }

    function doRedo() {
      backwardChanges.push(editor.getValue());
      var code = forwardChanges.pop();
      editor.setValue(code)
      editor.selection.clearSelection();
      editor.gotoLine(0);
      recompile();
      updateUndoRedo()
    }

    function storeCurrent(code) {
      forwardChanges = []
      if (backwardChanges.length >= 1) {
        if (code != backwardChanges[backwardChanges.length-1]) {
          backwardChanges.push(code)
        }
      } else {
          backwardChanges.push(code)
      }
      updateUndoRedo()
    }

    function updateUndoRedo() {
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
    }

    updateUndoRedo()

    $("#button-permalink").click(function(event) {
        if (!$(this).hasClass("disabled")) {
            var msg = JSON.stringify(
              {action: "storePermaLink", module: "main", code: editor.getValue()}
            )
            leonSocket.send(msg)
        }
        event.preventDefault()
    });

    handlers["permalink"] = function (data) {
        $("#permalink-value input").val(_leon_url+"#link/"+data.link)
        $("#permalink-value").show()
    }

    $("#button-permalink-close").click(function(event) {
        $("#permalink-value").hide()
    })

    /**
     * Compilation
     */

    function updateCompilationProgress(percents) {
      $("#overview .progress-bar").css("width", percents+"%");
    }

    function updateCompilationStatus(status) {
        var e = $(".compilation-status")
        var codebox = $("div#codebox")
        var boxes = $(".results_table")

        e.attr("class", "compilation-status");
        $(".results_table > .overlay").remove();

        if (status == "success") {
          compilationStatus = 1

          e.addClass("success")
          e.html('Compiled <i class="fa fa-check" title="Compilation succeeded"></i>')


          codebox.removeClass("compilation-error")
        } else if (status == "failure") {
          compilationStatus = -1

          e.addClass("failure")
          e.html('Compilation Failed <i class="fa fa-warning" title="Compilation failed"></i>')

          codebox.addClass("compilation-error")

          boxes.append('<div class="overlay" />')
        } else if (status == "disconnected") {
          compilationStatus = 0

          e.addClass("failure")
          e.html('Disconnected <i class="fa fa-unlink"></i>')

          boxes.append('<div class="overlay" />')

        } else if (status == "unknown") {
          compilationStatus = 0

          e.html('Compiling <i class="fa fa-refresh fa-spin" title="Compiling..."></i>')
        } else {
          alert("Unknown status: "+status)
        }

        if (status == "unknown") {
          updateCompilationProgress(0);
        } else {
          updateCompilationProgress(100);
        }

        updateExplorationHighlights();
        drawSynthesisOverview()
    }

    handlers["compilation_progress"] = function (data) {
      updateCompilationProgress(Math.round((data.current*100)/data.total))
    }

    handlers["compilation"] = function (data) {
        if(data.status == "success") {
            updateCompilationStatus("success")
        } else {
            updateCompilationStatus("failure")
        }
    }

    handlers["move_cursor"] = function (data) {
      editor.selection.clearSelection();
      editor.gotoLine(data.line, data.column, true);
    }

    var features = {
        verification:   {active: true, name: "Verification"},
        synthesis:      {active: true, name: "Synthesis"},
        termination:    {active: false, name: "Termination <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>"},
        presentation:   {active: false, name: "Presentation Mode"},
        execution:      {active: true, name: "Execution <i class=\"fa fa-lightbulb-o\" title=\"Beta version\"></i>"},
    }

    var localFeatures = localStorage.getItem("leonFeatures")
    if (localFeatures != null) {
        var locFeatures = JSON.parse(localFeatures)
        for (var f in locFeatures) {
            if (f in features) {
                features[f].active = locFeatures[f].active
            }
        }
    }

    var fts = $("#params-panel ul")
    for (var f in features) {
        fts.append('<li><label class="checkbox"><input id="feature-'+f+'" class=\"feature\" ref=\"'+f+'\" type="checkbox"'+(features[f].active ? ' checked="checked"' : "")+'>'+features[f].name+'</label></li>')
    }

    $(".feature").click(function () {
        var f = $(this).attr("ref")
        features[f].active = !features[f].active

        var msg = JSON.stringify(
          {action: "featureSet", module: "main", feature: f, active: features[f].active}
        )
        leonSocket.send(msg)


        localStorage.setItem("leonFeatures", JSON.stringify(features));

        recompile()

        drawOverView()
        drawSynthesisOverview()
        setPresentationMode()
    })

    setPresentationMode()

    var overview = {
        modules: {
            verification: {
                column: "Verif.",
                html: function(name, d) {
                    var vstatus = '<i class="fa fa-refresh fa-spin" title="Verifying..."></i>';

                    switch(d.status) {
                      case "crashed":
                        vstatus = '<i class="fa fa-bolt text-danger" title="Unnexpected error during verification"></i>';
                        break;
                      case "undefined":
                        vstatus = '<i class="fa fa-refresh fa-spin" title="Verifying..."></i>';
                        break;
                      case "cond-valid":
                        vstatus = '<span class="text-success" title="Conditionally valid">(<i class="fa fa-check"></i>)</span>';
                        break;
                      case "valid":
                        vstatus = '<i class="fa fa-check text-success" title="Valid"></i>';
                        break;
                      case "invalid":
                        vstatus = '<i class="fa fa-exclamation-circle text-danger" title="Invalid"></i>';
                        break;
                      case "timeout":
                        vstatus = '<i class="fa fa-clock-o text-warning" title="Timeout"></i>';
                        break;
                    }

                    return "<td class=\"status verif\" fname=\""+name+"\">"+vstatus+"</td>"
                },
                missing: function(name) {
                    return "<td class=\"status verif\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
                },
                handlers: function() {
                    $("td.verif").click(function () {
                        var fname = $(this).attr("fname")
                        if (fname in overview.data["verification"]) {
                            var d = overview.data["verification"][fname]

                            openVerifyDialog()

                            displayVerificationDetails(d.status, d.vcs)
                        } else {
                            openVerifyDialog()

                            displayVerificationDetails("unknown", [])
                        }
                    });
                }
            },
            termination: {
                column: "Term.",
                html: function(name, d) {
                    var tstatus = '<i class="fa fa-refresh fa-spin" title="Checking termination..."></i>';

                    switch(d.status) {
                        case "unknown":
                            tstatus = "<i class=\"fa fa-question\" title=\"Unknown\" />";
                            break;
                        case "terminates":
                            tstatus = "<i class=\"fa fa-check text-success\" title=\"Termination guaranteed\"></i>";
                            break;
                        case "noguarantee":
                            tstatus = "<i class=\"fa fa-exclamation-circle text-danger\" title=\"No termination guarantee\"></i>";
                            break;
                    }

                    return "<td class=\"status termin\" fname=\""+name+"\">"+tstatus+"</td>"
                },
                missing: function(name) {
                    return "<td class=\"status termin\" fname=\""+name+"\"><i class=\"fa fa-question\" title=\"unknown\"></i></td>"
                },
                handlers: function() { }
            },
        },
        functions: {},
        data: {
            verification: {},
            termination: {},
        },
    }

    handlers["update_overview"] = function(data) {
        if (data.module == "main") {
            overview.functions = {};

            for (var i = 0; i < data.overview.length; i++) {
                var fdata = data.overview[i]
                var fname = fdata.name
                overview.functions[fname] = fdata;
            }
        } else {
            overview.data[data.module] = data.overview
        }

        drawOverView()
    }

    var synthesisOverview = {}

    handlers["update_synthesis_overview"] = function(data) {
        if (JSON.stringify(synthesisOverview) != JSON.stringify(data)) {
            synthesisOverview = data;
            drawSynthesisOverview();
        }
    }


    function drawSynthesisOverview() {
        var t = $("#synthesis_table")
        var html = "";

        function addMenu(index, fname, description) {
            var id = 'menu'+fname+index

            html += ' <div class="dropdown">'
            html += '  <a id="'+id+'" href="#" role="button" class="dropdown-toggle" data-toggle="dropdown"> <i class="fa fa-magic"></i> '+description+'</a>'
            html += '  <ul class="dropdown-menu" role="menu" aria-labelledby="'+id+'">'
            if (compilationStatus == 1) {
                html += '    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="search" cid="'+index+'">Search</a></li>'
                html += '    <li role="presentation" class="divider"></li>'
                html += '    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><img src="/assets/images/loader.gif" /></a></li>'
            } else {
                html += '    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><i class="fa fa-ban"></i> Not compiled</a></li>'
            }

            html += '  </ul>'
            html += ' </div>'
        }

        var data = synthesisOverview

        for (var f in data.functions) {
            if (data.functions[f].length == 1) {
                var sp = data.functions[f][0]
                html += "<tr><td class=\"fname problem  clicktoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
                addMenu(sp.index, f, overview.functions[f].displayName)
                html += "</td></tr>"
            } else {
                html += "<tr><td class=\"fname clicktoline\" line=\""+overview.functions[f].line+"\">"+overview.functions[f].displayName+"</td></tr>"
                for (var i = 0; i < data.functions[f].length; i++) {
                    var sp = data.functions[f][i]
                    html += "<tr>"
                    html += "<td class=\"problem subproblem clicktoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
                    addMenu(sp.index, f, sp.description)
                    html += "</td></tr>"
                }
            }
        }

        t.html(html);

        if (compilationStatus == 1) {
            $("#synthesis .dropdown-toggle").click(function(e) {
                var p = $(this).parents(".problem")

                var msg = JSON.stringify({
                    module: "synthesis",
                    action: "getRulesToApply",
                    fname: p.attr("fname"),
                    cid: 1*p.attr("cid"),
                })

                leonSocket.send(msg)
            })
        }

        if (data.functions && Object.keys(data.functions).length > 0 && features["synthesis"].active) {
            $("#synthesis").show()
        } else {
            $("#synthesis").hide()
        }
    }

    function setPresentationMode() {
        if(features["presentation"].active) {
            $("body").addClass("presentation")
        } else {
            $("body").removeClass("presentation")
        }
        resizeEditor()
    }

    handlers["update_exploration_facts"] = function(data) {
        updateExplorationFacts(data.newFacts);
    }

    function drawOverView() {
        var t = $("#overview_table")
        var html = "";

        html += "<tr>"
        html += "<th>Function</th>"
        for (var m in overview.modules) {
            if (features[m].active) {
                html += "<th>"+overview.modules[m].column+"</th>"
            }
        }
        html += "</tr>"

        for (var fname in overview.functions) {
            var fdata = overview.functions[fname]

            html += "<tr>"
            html += "  <td class=\"fname clicktoline\" line=\""+fdata.line+"\">"+fdata.displayName+"</td>"
            for (var m in overview.modules) {
                if (features[m].active) {
                    var mod = overview.modules[m]
                    var data = overview.data[m]
                    if (fname in data) {
                        html += mod.html(fname, data[fname])
                    } else {
                        html += mod.missing(fname)
                    }
                }
            }
            html += "</tr>"
        }

        t.html(html);

        for (var m in overview.modules) {
            if ("handlers" in overview.modules[m]) {
                overview.modules[m].handlers()
            }
        }


        addClickToLine("#overview_table");
        addHoverToLine("#overview_table");

        if (Object.keys(overview.functions).length == 0) {
            t.hide()
        } else {
            t.show()
        }
    }

    function addClickToLine(within) {
      $(within+" .clicktoline[line]").click(function() {
          var line = $(this).attr("line")
          editor.gotoLine(line);
      })
    }

    function addHoverToLine(within) {
      $(within+" .hovertoline[line]").hover(function() {
          var line = $(this).attr("line")
          editor.gotoLine(line);
      }, function() {})
    }

    handlers["editor"] = function (data) {
        if ("annotations" in data) {
            var session = editor.getSession();

            context = "unknown";

            $("#annotations").html("");

            for (var i = 0; i < data.annotations.length; i++) {
                var a = data.annotations[i];
                if (a.type == "verification") {
                    context = "verification";
                } else if (a.type == "synthesis") {
                    context = "synthesis";
                }

                if (a.type != "info" && a.type != "error") {
                    session.addGutterDecoration(a.row, "leon_gutter_"+a.type)
                    a.type = "info";
                }

                if (a.type == "error") {
                  var line = a.row+1
                  $("#annotations").append("<li class=\"clicktoline\" line=\""+line+"\"><code><i class=\"fa fa-warning\"></i> "+line+":"+a.text+"</code></li>")
                }
            }


            addClickToLine("#annotations");
            session.setAnnotations(data.annotations);
            resizeEditor();
        }
    }

    handlers["notification"] = function (data) {
        notify(data.content, data.type);
    }

    handlers["log"] = function (data) {
        var txt = $("#console")
        txt.append(data.message+"\n");
        txt.scrollTop(txt[0].scrollHeight - txt.height())
    }

    var synthesizing = false;

    handlers["synthesis_result"] = function(data) {
        var pb = $("#synthesisProgress")
        var pbb = $("#synthesisProgress .progress-bar")

        // setup and open pane
        if (data.result == "init") {
            $("#synthesisResults").hide()
            $("#synthesisDialog .importButton").hide()
            $("#synthesisDialog .closeButton").hide()
            $("#synthesisDialog .cancelButton").show()
            $("#synthesisDialog .code.problem").removeClass("prettyprinted")
            $("#synthesisDialog .code.problem").html(data.problem)
            prettyPrint();
            $("#synthesisDialog").modal("show")

            pbb.addClass("active progress-bar-striped")
            pbb.removeClass("progress-bar-success progress-bar-danger")
            pbb.width("100%")
            pbb.html("Synthesizing...");

            $("#synthesisProgressBox").show()
            synthesizing = true;
            $('#synthesisDialog').on('hide.bs.modal', function () {
                if (synthesizing) {
                    var msg = JSON.stringify({
                        module: "main",
                        action: "doCancel"
                    })

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

            $("#synthesisDialog .cancelButton").hide()
            $("#synthesisDialog .closeButton").show()
            synthesizing = false;

        } else if (data.result == "success") {
            pbb.removeClass("active progress-bar-striped")

            pbb.width("100%")
            pbb.html(data.closed+"/"+data.total);
            pbb.addClass("progress-bar-success")

            if (data.total == 1) {
                $("#synthesisProgressBox").hide()
            }

            $("#synthesisResults .code.solution").removeClass("prettyprinted")
            $("#synthesisResults .code.solution").html(data.solCode)
            $("#synthesisResults").show()
            prettyPrint();
            $("#synthesisDialog .importButton").show()
            $("#synthesisDialog .importButton").unbind('click').click(function () {
                handlers["replace_code"]({ newCode: data.allCode })
                if ("cursor" in data) {
                  handlers["move_cursor"](data.cursor)
                }
            })
            $("#synthesisDialog .cancelButton").hide()
            $("#synthesisDialog .closeButton").show()
            synthesizing = false;
        }
    }

    handlers["synthesis_rulesToApply"] = function(data) {
        var fname       = data.fname
        var cid         = data.cid
        var rulesApps   = data.rulesApps

        var html = "";

        // Start by removing temp content
        if (compilationStatus == 1) {
            for (var i = 0; i < rulesApps.length; i++) {
                var app = rulesApps[i];
                var statusIcon = ""
                var clazz = "temp"

                if (app.status == "closed") {
                    statusIcon = '<i class="fa fa-exclamation-circle"></i> '
                    clazz += ' disabled'
                }
                html += '<li role="presentation" class="'+clazz+'"><a role="menuitem" tabindex="-1" href="#" action="rule" cid="'+cid+'" rid="'+app.id+'">'+statusIcon+app.name+'</a></li>'
            }
        } else {
            html += '<li role="presentation" class="temp disabled"><a role="menuitem" tabindex="-1" href="#" fname="'+fname+'">Not yet compiled...</a></li>'
        }

        var selector = "#synthesis .problem[fname=\""+fname+"\"][cid=\""+cid+"\"] ul"
        $(selector+" li.temp").remove()
        $(selector).append(html)
        $(selector+" li a[action=\"search\"]").unbind('click').click(function() {
            var msg = JSON.stringify(
              {action: "doSearch", module: "synthesis", fname: fname, cid: cid}
            )

            leonSocket.send(msg)
        })
        $(selector+" li a[action=\"rule\"]").click(function() {
            var rid = 1*$(this).attr("rid")

            var msg = JSON.stringify(
              {action: "doApplyRule", module: "synthesis",  fname: fname, cid: cid, rid: rid}
            )

            leonSocket.send(msg)
        })
    }

    function displayVerificationDetails(status, vcs) {
        var pb = $("#verifyProgress")
        var pbb = pb.children(".progress-bar")

        pbb.width("100%")
        pb.removeClass("active")
        pb.addClass("progress-bar-striped")

        pbb.removeClass("progress-bar-warning progress-bar-success progress-bar-danger")

        switch (status) {
            case "cond-valid":
                pbb.html("Conditionally Valid!")
                pbb.addClass("progress-bar-warning")
                break;

            case "valid":
                pbb.html("Valid!")
                pbb.addClass("progress-bar-success")
                break;

            case "invalid":
                pbb.html("Invalid!")
                pbb.addClass("progress-bar-danger")
                break;

            case "unknown":
                pbb.html("Unknown ?!")
                pbb.addClass("progress-bar-warning")
                break;

            case "timeout":
                pbb.html("Timeout!")
                pbb.addClass("progress-bar-warning")
                break;
        }

        var tbl = $("#verifyResults tbody")
        tbl.html("");

        for (var i = 0; i < vcs.length; i++) {
            var vc = vcs[i];
            var icon = "check"
            if (vc.status == "invalid") {
                icon = "warning"
            } else if (vc.status == "unknown") {
                icon = "question"
            }
            var clas = "success"
            if (vc.status == "invalid") {
              clas = "danger"
            } else if (vc.status == "unknown") {
              clas = "warning"
            }


            tbl.append("<tr class=\""+clas+"\"> <td>"+vc.fun+"</td> <td>"+vc.kind+"</td> <td><i class=\"fa fa-"+icon+"\"></i> "+vc.status+"</td> <td>"+vc.time+"</td> </tr>")

            if ("counterExample" in vc) {
                var html = "<tr class=\""+clas+" counter-example\"><td colspan=\"4\">"
                html += "<div>"
                html += "  <p>The following inputs violate the VC:</p>";
                html += "  <table class=\"input\">";
                for (var v in vc.counterExample) {
                    html += "<tr><td>"+v+"</td><td>&nbsp;:=&nbsp;</td><td>"+vc.counterExample[v]+"</td></tr>";
                }
                html += "  </table>"

                if ("execution" in vc && vc.execution.result == "success" && features["execution"].active) {
                    html += "  <p>It produced the following output:</p>";
                    html += "  <div class=\"output\">"+vc.execution.output+"</div>"
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
        $("#verifyResults").show("fade");

    }

    handlers["verification_result"] = function (data) {
        displayVerificationDetails(data.status, data.vcs)
    }

    function error(msg) {
        alert(msg);
    }

    var receiveEvent = function(event) {
        var data = JSON.parse(event.data)
        if (data.kind in handlers) {
            handlers[data.kind](data);
        } else {
            console.log("Unknown event type: "+data.kind)
            console.log(data)
        }
    }

    var connected = false

    var lastReconnectDelay = 0;
    var reconnectIn = 0;

    var closeEvent = function(event) {
        if (connected) {
            setDisconnected()
        }
    }

    var openEvent = function(event) {
        if (lastReconnectDelay != 0) {
          notify("And we are back online!", "success")
          updateCompilationStatus("unknown")
        }

        setConnected()

        for (var f in features) {
            var msg = JSON.stringify(
              {action: "featureSet", module: "main", feature: f, active: features[f].active}
            )

            leonSocket.send(msg)
        }

        if(hash) {
            loadStaticLink(hash)
        } else {
            recompile()
        }
    }

    function loadStaticLink(hash) {
        if (hash.indexOf("#link/") == 0) {
            var msg = JSON.stringify(
              {action: "accessPermaLink", module: "main", link: hash.substr("#link/".length)}
            )

            leonSocket.send(msg);
            window.location.hash = "";
        }
        if (hash.indexOf("#demo/") == 0) {
            loadExample("demo", hash.substr("#demo/".length))
            window.location.hash = "";
        }
    }

    $(window).on('hashchange', function() {
        var hash = window.location.hash;
        loadStaticLink(hash);
    });

    function setDisconnected() {
        connected = false
        updateCompilationStatus("disconnected")
        lastReconnectDelay = 5;
        reconnectIn = lastReconnectDelay;

        checkDisconnectStatus()
    }

    function setConnected() {
        connected = true

        $("#connectError").hide();
        $("#disconnectError").hide();

        lastReconnectDelay = 0;
        reconnectIn = -1;
    }

    function checkDisconnectStatus() {
        if (reconnectIn == 0) {
            reconnectIn = -1;
            $("#disconnectError #disconnectMsg").html("Attempting reconnection...");

            connectWS()

            // If still not connected after 2 seconds, consider failed
            setTimeout(function() {
                if (!connected) {
                    if (lastReconnectDelay == 0) {
                        lastReconnectDelay = 5;
                    } else {
                        lastReconnectDelay *= 2;
                    }

                    reconnectIn = lastReconnectDelay;
                }
            }, 2000);
        } else if (reconnectIn > 0) {
            $("#disconnectError #disconnectMsg").html('Retrying in '+reconnectIn+' seconds... <button id="tryReconnect" class="btn btn-danger btn-xs">Try now</button>');

            $("#tryReconnect").click(function() {
                reconnectIn = 0;
                checkDisconnectStatus();
            })

            $("#disconnectError").show().alert();

            reconnectIn -= 1;
        }
    }

    setInterval(function () { checkDisconnectStatus() }, 2000);

    var errorEvent = function(event) {
        console.log("ERROR")
        console.log(event)
    }

    connectWS()
    setTimeout(function() {
        if (!connected) {
            $("#disconnectError").hide();
            $("#connectError").show().alert();
        }
    }, 3000);

    function connectWS() {
        console.log("Connecting..")
        leonSocket = new WS(_leon_websocket_url)
        leonSocket.onopen = openEvent
        leonSocket.onmessage = receiveEvent;
        leonSocket.onclose = closeEvent
        leonSocket.onerror = errorEvent
    }

    var lastChange      = 0;
    var lastSavedChange = lastChange;
    var timeWindow      = 2000;

    function updateSaveButton() {
        var e = $("#button-save")
        if (lastChange == lastSavedChange) {
           e.addClass("disabled"); 
        } else {
           e.removeClass("disabled"); 
        }
    }

    function notify(content, type, fade) {
        if (!fade) {
            fade = 3000
        }

        var note = $("<div>", {
            "class": "alert fade in alert-"+type
        }).html('<button type="button" class="close" data-dismiss="alert">×</button>'+content)

        $("#notifications").append(note);

        setTimeout(function() {
            note.hide();
        }, fade)
    }

    var oldCode = ""

    function recompile() {
        var currentCode = editor.getValue()

        if (oldCode != "" && oldCode != currentCode) {
            if (forwardChanges.length == 0) {
                storeCurrent(oldCode)
            }
        }

        if (connected) {
            var msg = JSON.stringify(
              {action: "doUpdateCode", module: "main", code: currentCode}
            )
            oldCode = currentCode;
            lastSavedChange = lastChange;
            updateSaveButton();
            leonSocket.send(msg)
            updateCompilationProgress(0)
        }
    }

    function onCodeUpdate() {
        var now = new Date().getTime()

        if (lastChange < (now - timeWindow)) {
            lastChange = new Date().getTime();
            if (lastChange > 0) {
                recompile()
            }
        }

        localStorage.setItem("leonEditorCode", editor.getValue());
    }

    function loadSelectedExample() {
        var selected = $('#example-loader').find(":selected[id]")

        var id = selected.attr("id")
        var group = selected.attr("group")

        loadExample(group, id)
    }
    function loadExample(group, id) {
        if (id) {
            $.ajax({
              url: '/ajax/getExample/'+group+'/'+id,
              dataType: "json",
              success: function(data, textStatus, jqXHR) {
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
              error: function(jqXHR, textStatus, errorThrown) {
                notify("Loading example failed :(", "error")
              }
            });
        }
    }

    $("#example-loader").change(loadSelectedExample);

    var editorSession = editor.getSession();

    editor.commands.addCommand({
        name: 'save',
        bindKey: {win: 'Ctrl-S',  mac: 'Command-S'},
        exec: function(editor) {
            recompile()
        },
        readOnly: true
    });

    editor.commands.removeCommand('replace');
    editor.commands.removeCommand('transposeletters');

    editorSession.on('change', function(e) {
        lastChange = new Date().getTime();
        updateSaveButton();
        var currentCode = editor.getValue()
        if (currentCode != oldCode) {
            updateCompilationStatus("unknown")
        }
        setTimeout(onCodeUpdate, timeWindow+50)
    });

    function resizeEditor() {

        var h = $(window).height()-$("#title").height()-6
        var ah = $("#annotations").height()
        var w = $("#codecolumn").width()

        $('#codecolumn').height(h);
        $('#panelscolumn').height(h);
        $('#leoninput').height(h-ah).width(w);
        $('#codebox').height(h-ah).width(w);

        editor.resize();
    };

    $(window).resize(resizeEditor);

    resizeEditor();


    handlers["replace_code"] = function(data) {
        storeCurrent(editorSession.getValue())
        editorSession.setValue(data.newCode)
        recompile()
    }

    var currentMousePos = { x: -1, y: -1 };

    $(document).mousemove(function(event) {
        currentMousePos = { x: event.pageX, y: event.pageY };
    });

    function openVerifyDialog() {
        $("#verifyDialog").modal("show")
    }

    var storedCode = localStorage.getItem("leonEditorCode")

    var seenDemo = 1*localStorage.getItem("leonSeenDemo")
    var demos = [
        {
            placement: "modal",
            title: "Welcome to Leon!",
            content: "Leon is an automated system for <strong>synthesizing</strong> and <strong>verifying</strong> functional Scala programs."
        },
        {
            where: function() { return $("#example-loader") },
            placement: "left",
            title: "Select from examples",
            content: "You can try <em>Leon</em> on a list of selected examples, covering both synthesis and verification problems."
        },
        {
            where: function() { return $($(".ace_line_group")[13]).find("span").last() },
            placement: "right",
            title: "Edit at will",
            content: "Feel free to modify or extend the selected examples with your own code."
        },
        {
            where: function() { return $("#overview_table") },
            placement: "left",
            title: "Live results",
            content: "Leon will verify your code in the background and display live verification results here."
        },
        {
            where: function() { return $($("#overview_table td.status.verif")[2]) },
            placement: "left",
            title: "Display details",
            content: "Click on the verification status of each function to get more information!"
        },
        {
            where: function() { return $("#synthesis_table td.problem").first() },
            placement: "left",
            title: "Synthesize",
            content: "Click on a synthesis problem to solve it! You can either ask <em>Leon</em> to <strong>search</strong> for a solution, or perform individual steps yourself."
        },
        {
            where: function() { return $("#button-permalink") },
            placement: "bottom",
            title: "Permalinks",
            content: "You can generate permalinks to the editor session. If you experience any problem with the interface or if you do not understand the result, send us a link!"
        }
    ];

    if (!seenDemo || (seenDemo < demos.length-1)) {

        var lastDemo = null

        function showDemo(id) {
            var demo = demos[id]

            if (demo.placement == "modal") {
                // Assume only the first demo is modal
                var html  = '<div id="demoPane" class="modal fade" tabindex="-1" role="dialog" aria-labelledby="demoModal" aria-hidden="true" data-backdrop="static">'
                html     += '  <div class="modal-dialog">'
                html     += '    <div class="modal-content">'
                html     += '      <div class="modal-header">'
                html     += '        <button type="button" class="close" demo-action="close" data-dismiss="modal" aria-hidden="true">×</button>'
                html     += '        <h3 id="demoModal">'+demo.title+'</h3>'
                html     += '      </div>'
                html     += '      <div class="modal-body">'
                html     += '        '+demo.content
                html     += '      </div>'
                html     += '      <div class="modal-footer">'
                html     += '        <button class="btn btn-success" data-dismiss="modal" aria-hidden="true" demo-action="next">Take the tour <i class="fa fa-play"></i></button>'
                html     += '        <button class="btn" data-dismiss="modal" aria-hidden="true" demo-action="close">No thanks</button>'
                html     += '      </div>'
                html     += '    </div>'
                html     += '  </div>'
                html     += '</div>'

                $("body").append(html);

                $("#demoPane").modal("show")

                var action = "close"

                $("#demoPane button").click(function() {
                    action = $(this).attr("demo-action")
                    hideDemo(id)
                })

                $('#demoPane').on('hide.bs.modal', function () {
                    if (action == "next") {
                        localStorage.setItem("leonSeenDemo", id+1)
                        setTimeout(function() { showDemo(id+1) }, 500)
                    } else {
                        localStorage.setItem("leonSeenDemo", 100)
                    }
                })

            } else {
                var content = '<div id="demoPane" class="demo">'
                content += demo.content
                content += '  <div class="demo-nav">'
                if (id == demos.length-1) {
                    // last demo
                    content += '    <button class="btn btn-success" demo-action="close">Ok!</button>';
                } else {
                    content += '    <button class="btn" demo-action="close">Got it</button>';
                    content += '    <button class="btn btn-success" demo-action="next">Next <i class="fa fa-forward"></i></button>';
                }
                content += '  </div>'
                content += '</div>'

                var where = demo.where()

                lastDemo = where;

                if (where.length == 0) {
                  localStorage.setItem("leonSeenDemo", id+1)
                  hideDemo(id)
                  showDemo(id+1)
                  return;
                }

                var progress = ""
                for (var i = 0; i < demos.length-1; i++) {
                    if (i < id) {
                        progress += '<i class="fa fa-circle"></i>'
                    } else {
                        progress += '<i class="fa fa-circle-o"></i>'
                    }
                }

                where.popover({
                    html: true,
                    placement: demo.placement,
                    trigger: "manual",
                    title: '<span class="demo-progress">'+progress+'</span>'+demo.title,
                    content: content,
                    container: "body"
                })

                where.popover("show")

                $("#demoPane button[demo-action=\"close\"]").click(function() {
                    localStorage.setItem("leonSeenDemo", 100)
                    hideDemo(id)
                })

                $("#demoPane button[demo-action=\"next\"]").click(function() {
                    localStorage.setItem("leonSeenDemo", id+1)
                    hideDemo(id)
                    showDemo(id+1)
                })
            }


        }

        function hideDemo(id) {
            var demo = demos[id]

            if (demo.placement == "modal") {
                $("#demoPane").modal("hide")
                $("#demoPane").unbind("hidden").on("hidden", function() { $("demoPane").remove() })
            } else {
                lastDemo.popover("destroy")
            }
        }

        var toShow = seenDemo? seenDemo : 0;
        if (toShow != 0) {
            setTimeout(function() { showDemo(toShow) }, 1000)
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

    /*
    snowStorm.snowColor = '#ddddff';
    snowStorm.vMaxX = 2;
    snowStorm.vMaxY = 2;
    snowStorm.useTwinkleEffect = false;
    snowStorm.flakesMinActive = 350;
    snowStorm.flakesMaxActive = 350;
    snowStorm.followMouse = false;
    snowStorm.stop();
    */
});
