var editor = null;

$(document).ready(function() {
    editor = ace.edit("codebox");
    editor.getSession().setMode("ace/mode/scala")
    editor.getSession().setUseWrapMode(true)
    editor.getSession().setTabSize(2)

    var hash = window.location.hash

    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var leonSocket = null

    var headerHeight = $("#title").height()+20

    var mode = "normal"
    function togglePresentationMode() {
        if (mode == "normal") {
            $("#actionscolumn").hide()
            $("#codecolumn").removeClass("span8").addClass("span12")

            mode = "presentation"
            //$("#button-menu span.label").html("Show menu")
        } else {
            $("#codecolumn").removeClass("span12").addClass("span8")
            $("#actionscolumn").show()

            mode = "normal"
            //$("#button-menu span.label").html("Hide menu")
        }
        resizeEditor()
    }

    $(".menu-button").click(function(event) {
        var target = $(this).attr("ref")
        var sel = "#"+target
        console.log(sel)

        if ($(sel).is(":visible")) {
            $(sel).hide()
            $(this).addClass("disabled")
        } else {
            $(sel).show()
            $(this).removeClass("disabled")
        }

    });

    $("#button-menu").click(function(event) {
        togglePresentationMode()
        if (mode == "presentation") {
            $(this).addClass("disabled")
            $("#button-console").hide()
        } else {
            $(this).removeClass("disabled")
            $("#button-console").show()
        }
        event.preventDefault()
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

    function updateCompilationStatus(status) {
        var e = $(".compilation-status")

        if (status == "success") {
          e.attr("class", "compilation-status success")
          compilationStatus = 1
          e.html("<img src=\""+_leon_prefix+"/assets/images/accept.png\" />")
        } else if (status == "failure") {
          e.attr("class", "compilation-status failure")
          compilationStatus = -1
          e.html("<img src=\""+_leon_prefix+"/assets/images/exclamation.png\" />")
        } else if (status == "disconnected") {
          e.attr("class", "compilation-status failure")
          compilationStatus = 0
          e.html("<img src=\""+_leon_prefix+"/assets/images/exclamation.png\" />")
        } else if (status == "unknown") {
          e.attr("class", "compilation-status")
          compilationStatus = 0
          e.html("<img src=\""+_leon_prefix+"/assets/images/loader.gif\" />")
        } else {
            alert("Unknown status: "+status)
        }
    }

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


    handlers["compilation"] = function (data) {
        if(data.status == "success") {
            updateCompilationStatus("success")
        } else {
            updateCompilationStatus("failure")
        }
    }

    var features = {
        verification:   {active: true, name: "Verification"},
        synthesis:      {active: true, name: "Synthesis"},
        termination:    {active: false, name: "Termination <i class=\"icon-beaker\" title=\"Beta version\"></i>"},
    }

    var localFeatures = localStorage.getItem("leonFeatures")
    if (localFeatures != null) {
        features = JSON.parse(localFeatures)
    }

    var fts = $("#params-action ul")
    for (var f in features) {
        fts.append('<li><label class="checkbox"><input id="feature-'+f+'" class=\"feature\" ref=\"'+f+'\" type="checkbox"'+(features[f].active ? ' checked="checked"' : "")+'>'+features[f].name+'</label></li>')
    }

    $(".feature").click(function () {
        var f = $(this).attr("ref")
        features[f].active = !features[f].active
        localStorage.setItem("leonFeatures", JSON.stringify(features));

        drawOverView()
        drawSynthesisOverview()
    })

    var overview = {
        modules: {
            verification: {
                column: "Verif.",
                html: function(name, d) {
                    var vstatus = "<img src=\""+_leon_prefix+"/assets/images/loader.gif\" title=\"Verifying...\" />"

                    switch(d.status) {
                      case "undefined":
                        vstatus = "<img src=\""+_leon_prefix+"/assets/images/loader.gif\" title=\"Verifying...\" />";
                        break;
                      case "cond-valid":
                        vstatus = "<i class=\"icon-exclamation-sign\" title=\"Cond-Valid\"></i>";
                        break;
                      case "valid":
                        vstatus = "<i class=\"icon-ok-sign\" title=\"Valid\"></i>";
                        break;
                      case "invalid":
                        vstatus = "<i class=\"icon-remove-sign\" title=\"Invalid\"></i>";
                        break;
                      case "timeout":
                        vstatus = "<i class=\"icon-time\" title=\"Timeout\"></i>";
                        break;
                    }

                    return "<td class=\"status verif\" fname=\""+name+"\">"+vstatus+"</td>"
                },
                missing: function(name) {
                    return "<td class=\"status verif\" fname=\""+name+"\"><i class=\"icon-question-sign\" title=\"unknown\"></i></td>"
                },
                handlers: function() {
                    $("td.verif").click(function () {
                        var fname = $(this).attr("fname")
                        var d = overview.data["verification"][fname]
                        displayVerificationDetails(d.status, d.vcs, true)
                    });
                }
            },
            termination: {
                column: "Term.",
                html: function(name, d) {
                    var tstatus = "<img src=\""+_leon_prefix+"/assets/images/loader.gif\" title=\"Verifying\"/>"

                    switch(d.status) {
                        case "unknown":
                            tstatus = "<img src=\""+_leon_prefix+"/assets/images/loader.gif\" title=\"Unknown\" />";
                            break;
                        case "terminates":
                            tstatus = "<i class=\"icon-ok-sign\" title=\"Termination guaranteed\"></i>";
                            break;
                        case "noguarantee":
                            tstatus = "<i class=\"icon-question-sign\" title=\"No termination guarantee\"></i>";
                            break;
                    }

                    return "<td class=\"status termin\" fname=\""+name+"\">"+tstatus+"</td>"
                },
                missing: function(name) {
                    return "<td class=\"status termin\" fname=\""+name+"\"><i class=\"icon-question-sign\" title=\"Unknown\"></i></td>"
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
        synthesisOverview = data;
        drawSynthesisOverview();
    }

    function drawSynthesisOverview() {
        var t = $("#synthesis_table")
        var html = "";

        function addMenu(index, fname, description) {
            var id = 'menu'+fname+index

            html += ' <div class="dropdown">'
            html += '  <a id="'+id+'" href="#" role="button" class="dropdown-toggle" data-toggle="dropdown">'+description+'</a>'
            html += '  <ul class="dropdown-menu" role="menu" aria-labelledby="'+id+'">'
            html += '    <li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="search">Search</a></li>'
            html += '    <li role="presentation" class="divider"></li>'
            html += '    <li role="presentation" class="disabled loader temp"><a role="menuitem" tabindex="-1"><img src="'+_leon_prefix+'/assets/images/loader.gif" /></a></li>'
            html += '  </ul>'
            html += ' </div>'
        }

        var data = synthesisOverview

        for (var f in data.functions) {
            if (data.functions[f].length == 1) {
                var sp = data.functions[f][0]
                html += "<tr><td class=\"fname problem  hovertoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
                addMenu(sp.index, f, overview.functions[f].displayName)
                html += "</td></tr>"
            } else {
                html += "<tr><td class=\"fname hovertoline\" line=\""+overview.functions[f].line+"\">"+overview.functions[f].displayName+"</td></tr>"
                for (var i = 0; i < data.functions[f].length; i++) {
                    var sp = data.functions[f][i]
                    html += "<tr>"
                    html += "<td class=\"problem subproblem hovertoline\" line=\""+sp.line+"\" fname=\""+f+"\" cid=\""+sp.index+"\">"
                    addMenu(sp.index, f, sp.description)
                    html += "</td></tr>"
                }
            }
        }

        t.html(html);

        $("#synthesis .hovertoline[line]").hover(function() {
            var line = $(this).attr("line")
            editor.gotoLine(line);
        }, function() {})


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

        if (Object.keys(data.functions).length > 0 && features["synthesis"].active) {
            $("#synthesis").show()
        } else {
            $("#synthesis").hide()
        }
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
            html += "  <td class=\"fname hovertoline\" line=\""+fdata.line+"\">"+fdata.displayName+"</td>"
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

        $("#overview .hovertoline[line]").hover(function() {
            var line = $(this).attr("line")
            editor.gotoLine(line);
        }, function() {})

        if (Object.keys(overview.functions).length == 0) {
            $("#overview").hide()
        } else {
            $("#overview").show()
        }
    }

    handlers["editor"] = function (data) {
        if ("annotations" in data) {
            var session = editor.getSession();

            context = "unknown";

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
            }

            session.setAnnotations(data.annotations);
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

    handlers["synthesis_result"] = function(data) {
        var pb = $("#synthesisProgress")
        var pbb = $("#synthesisProgress .bar")

        // setup and open pane
        if (data.result == "init") {
            $("#synthesisResults").hide()
            $("#synthesisDialog .importButton").hide()
            $("#synthesisDialog .cancelButton").show()
            $("#synthesisDialog .code.problem").html(data.problem)
            $("#synthesisDialog").modal("show")
            pbb.removeClass("bar-success bar-danger")
            pbb.width("0%")
            pbb.html("");
            $("#synthesisProgressBox").show()
        } else if (data.result == "progress") {

            var pc = (data.closed*100)/data.total;
            pbb.width(pc+"%")
            pbb.html(data.closed+"/"+data.total);

        } else if (data.result == "failure") {
            pb.removeClass("active progress-striped")

            pbb.width("100%")
            pbb.html("Failed to apply");
            pbb.addClass("bar-danger")

            $("#synthesisDialog .cancelButton").hide()

        } else if (data.result == "success") {
            pb.removeClass("active progress-striped")

            pbb.width("100%")
            pbb.addClass("bar-success")

            if (data.total == 1) {
                $("#synthesisProgressBox").hide()
            }

            $("#synthesisResults .code.solution").html(data.solCode)
            $("#synthesisResults").show()
            $("#synthesisDialog .importButton").show()
            $("#synthesisDialog .importButton").unbind('click').click(function () {
                handlers["replace_code"]({ newCode: data.allCode })
            })
            $("#synthesisDialog .cancelButton").hide()
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
                    statusIcon = '<i class="icon-exclamation-sign"></i> '
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

    function displayVerificationDetails(status, vcs, doOpen) {
        var pb = $("#verifyProgress")
        var pbb = pb.children(".bar")

        if (doOpen) {
            openVerifyDialog()
        }

        pbb.width("100%")
        pb.removeClass("active")
        pb.removeClass("progress-striped")


        pbb.removeClass("bar-warning bar-success bar-danger")

        switch (status) {
            case "cond-valid":
                pbb.html("Conditionally Valid!")
                pbb.addClass("bar-warning")
                break;

            case "valid":
                pbb.html("Valid!")
                pbb.addClass("bar-success")
                break;

            case "invalid":
                pbb.html("Invalid!")
                pbb.addClass("bar-danger")
                break;

            case "timeout":
                pbb.html("Timeout!")
                pbb.addClass("bar-warning")
                break;
        }

        var tbl = $("#verifyResults tbody")
        tbl.html("");

        for (var i = 0; i < vcs.length; i++) {
            var vc = vcs[i];
            var icon = "ok"
            if (vc.status == "invalid") {
                icon = "remove"
            } else if (vc.status == "unknown") {
                icon = "time"
            }


            tbl.append("<tr class=\""+((i%2 == 0) ? "odd " : "")+vc.status+"\"> <td>"+vc.fun+"</td> <td>"+vc.kind+"</td> <td><i class=\"icon-"+icon+"\"></i> "+vc.status+"</td> <td>"+vc.time+"</td> </tr>")

            if ("counterExample" in vc) {
                var html = "<tr class=\""+((i%2 == 0) ? "odd " : "")+"counter-example\"><td colspan=\"4\"><div><div>The following valuation violates the VC:</div><table>";

                for (var v in vc.counterExample) {
                    html += "<tr><td>"+v+"</td><td>&nbsp;:=&nbsp;</td><td>"+vc.counterExample[v]+"</td></tr>";
                }
                html += "</div></td></tr></table>"

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
        displayVerificationDetails(data.status, data.vcs, false)
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

    var closeEvent = function(event) {
        if (connected) {
            connected = false
            updateCompilationStatus("disconnected")
            reconnect(0)
        }
    }

    var openEvent = function(event) {
        connected = true;

        if(hash) {
            if (hash.indexOf("#link/") == 0) {
                var msg = JSON.stringify(
                  {action: "accessPermaLink", module: "main", link: hash.substr("#link/".length)}
                )

                leonSocket.send(msg);

                window.location.hash = "";
            }
        } else {
            recompile()
        }
    }

    var reconnectEvent = function(event) {
        connected = true

        $("#notifications").children(".error").each(function() {
            $(this).hide();
        });
        notify("Aaaaaand we are back!", "success")
        recompile()
    }

    function reconnect(after) {
        if (!connected) {
            var newTimeout = after + 5000;
            notify("Server disconnected. Attempting reconnection in "+(newTimeout/1000)+"s...", "error")

            connectWS()
            leonSocket.onopen = reconnectEvent

            setTimeout(function() { reconnect(newTimeout) }, newTimeout);
        }
    }

    var errorEvent = function(event) {
        console.log("ERROR")
        console.log(event)
    }

    connectWS()
    setTimeout(function() {
        if (!connected) {
            $("#connectError").show().alert();
        }
    }, 3000);

    function connectWS() {
        leonSocket = new WS(_leon_websocket_url)
        leonSocket.onopen = openEvent
        leonSocket.onmessage = receiveEvent
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

        if (connected && oldCode != currentCode) {
            updateCompilationStatus("unknown")

            var msg = JSON.stringify(
              {action: "doUpdateCode", module: "main", code: currentCode}
            )

            lastSavedChange = lastChange;
            updateSaveButton();
            leonSocket.send(msg)
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

    function loadExample() {
        var selected = $('#example-loader').find(":selected")

        var group = selected.attr("group")
        var value = selected.attr("id")

        if (value) {
            $.ajax({
              url: _leon_prefix+'/ajax/getExample/'+group+'/'+value,
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

    $("#example-loader").change(loadExample);

    var editorSession = editor.getSession();

    editor.commands.addCommand({
        name: 'save',
        bindKey: {win: 'Ctrl-S',  mac: 'Command-S'},
        exec: function(editor) {
            recompile()
        },
        readOnly: true
    });

    editor.commands.addCommand({
        name: 'verify',
        bindKey: {win: 'Alt-V',  mac: 'Alt-V'},
        exec: function(editor) {
            verifyCurrentFun()
        },
        readOnly: true
    });

    editorSession.on('change', function(e) {
        lastChange = new Date().getTime();
        updateSaveButton();
        setTimeout(onCodeUpdate, timeWindow+50)
    });

    function resizeEditor() {

        var h = $(window).height()-$("#title").height()-6
        //var w = $(window).width()
        var w = $("#codecolumn").width()

        $('#codecolumn').height(h);
        $('#actionscolumn').height(h);
        $('#leoninput').height(h).width(w);
        $('#codebox').height(h).width(w);

        editor.resize();
    };

    $(window).resize(resizeEditor);

    resizeEditor();


    handlers["replace_code"] = function(data) {
        storeCurrent(editorSession.getValue())
        editorSession.setValue(data.newCode)
        $("#overview").hide()
        recompile()
    }

    var currentMousePos = { x: -1, y: -1 };

    $(document).mousemove(function(event) {
        currentMousePos = { x: event.pageX, y: event.pageY };
    });

    //editorSession.selection.on('changeCursor', function(e) {
    //    var cursor = editorSession.selection.getCursor()

    //    var token     = editorSession.getTokenAt(cursor.row, cursor.column);

    //    if (token != null) {
    //        var screenX = currentMousePos.x
    //        var screenY = currentMousePos.y

    //        var prevToken = editorSession.getTokenAt(cursor.row, token.start-1);

    //        if (token.type == "identifier.choose" && token.value == "choose" && context == "synthesis") {

    //            if (compilationStatus == 1) {
    //                var msg = JSON.stringify({
    //                    action: "synthesis_getRulesToApply",
    //                    module: "synthesis",
    //                    chooseLine: cursor.row+1,
    //                    chooseColumn: token.start+1,
    //                })

    //                chooseRulesDisplayer = function(cid, rulesApp) {
    //                    return synthesisDisplayMenu(screenX, screenY, cid, rulesApp);
    //                }

    //                leonSocket.send(msg)
    //            } else {
    //                synthesisDisplayMenu(screenX, screenY, 0, []);
    //            }
    //        } else if (token.type == "identifier" && prevToken != null && prevToken.type == "keyword" && prevToken.value == "def" && context == "verification") {
    //            var found = false;
    //            var annots = editorSession.getAnnotations();
    //            for (var i = 0; i < annots.length && !found; i++) {
    //                if (annots[i].row == cursor.row) {
    //                    found = true;
    //                }
    //            }
    //            if (found) {
    //                verificationDisplayMenu(screenX, screenY, token.value);
    //            }
    //            
    //        }
    //    }
    //});

    //function verificationDisplayMenu(screenX, screenY, fname) {
    //    var m = $("#editorMenu")
    //    m.html("");

    //    if (compilationStatus == 1) {
    //        m.append('<li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="verify" fname="'+fname+'">Verify</a></li>');
    //    } else if (compilationStatus == 0) {
    //        m.append('<li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="none" fname="'+fname+'">Not yet compiled...</a></li>');
    //    } else {
    //        m.append('<li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="none" fname="'+fname+'">Not yet compiled...</a></li>');
    //    }

    //    $("#menuToggle").dropdown('toggle')
    //    $("#editorMenu").css({
    //        "position": "absolute",
    //        "top": (screenY-20)+"px",
    //        "left": (screenX-20)+"px",
    //        "z-index": 1000
    //    });

    //    $('ul.dropdown-menu li a').click(function (e) {
    //        $("#menuToggle").dropdown('toggle')
    //        if ($(this).attr("action") == "verify") {
    //            verifyFun($(this).attr("fname"))
    //        }
    //        e.preventDefault();
    //        return false;
    //    });
    //}

    function verifyCurrentFun() {
        var cursor = editorSession.selection.getCursor()

        var res = editor.find('def', {
            backwards: true,
            wholeWord: true
        });

        editorSession.selection.moveCursorToPosition(cursor)
        editorSession.selection.clearSelection();

        var found = false;
        var annots = editorSession.getAnnotations();
        for (var i = 0; i < annots.length && !found; i++) {
            if (annots[i].row == res.end.row) {
                found = true;
            }
        }

        if (found) {
            var tokens    = editorSession.getTokens(res.end.row)
            var pastDef   = false
            var fname     = null;
            for (var i = 0; i < tokens.length && fname == null; i++) {
                if (tokens[i].type == "keyword" && tokens[i].value == "def") {
                    pastDef = true
                } else if(pastDef && tokens[i].type == "identifier") {
                    fname = tokens[i].value
                }
            }

            if (fname != null) {
                verifyFun(fname)
            }
        }
    }

    function openVerifyDialog(cancelOnClose) {
        $("#verifyDialog").modal("show")
    }

    function verifyFun(fname) {
        var msg = JSON.stringify(
          {action: "verification_doVerify", module: "verification", fname: fname}
        )

        leonSocket.send(msg)

        var pb = $("#verifyProgress")
        var pbb = $("#verifyProgress .bar")

        pbb.html("Verifying...");
        pb.addClass("active progress-striped")

        $("#verifyResults").hide();

        openVerifyDialog(fname)
    }

    function synthesisDisplayMenu(screenX, screenY, cid, rulesApps) {
        var m = $("#editorMenu")
        m.html("");
        if (compilationStatus == 1) {
            m.append('<li role="presentation"'+clazz+'><a role="menuitem" tabindex="-1" href="#" action="search" cid="'+cid+'">Search</a></li>');
            m.append('<li role="presentation" class="divider"></li>');

            for (var i = 0; i < rulesApps.length; i++) {
                var app = rulesApps[i];
                var statusIcon = ""
                var clazz = ""

                if (app.status == "closed") {
                    statusIcon = '<i class="icon-exclamation-sign"></i> '
                    clazz = ' class="disabled"'
                }
                m.append('<li role="presentation"'+clazz+'><a role="menuitem" tabindex="-1" href="#" action="rule" cid="'+cid+'" rid="'+app.id+'">'+statusIcon+app.name+'</a></li>');
            }
        } else if (compilationStatus == 0) {
            m.append('<li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="none" fname="'+fname+'">Not yet compiled...</a></li>');
        } else {
            m.append('<li role="presentation"><a role="menuitem" tabindex="-1" href="#" action="none" fname="'+fname+'">Not yet compiled...</a></li>');
        }


        $("#menuToggle").dropdown('toggle')
        $("#editorMenu").css({
            "position": "absolute",
            "top": (screenY-20)+"px",
            "left": (screenX-20)+"px",
            "z-index": 1000
        });

        $('ul.dropdown-menu li a').click(function (e) {
            $("#menuToggle").dropdown('toggle')
            var r = $(this)

            if (r.attr("action") == "rule") {
                var msg = JSON.stringify(
                  {action: "synthesis_doApplyRule", module: "synthesis", cid: 1*r.attr("cid"), rid: 1*r.attr("rid")}
                )

                leonSocket.send(msg)
            } else if (r.attr("action") == "search") {
                var cid = 1*r.attr("cid")

                searchFinished = false

                var msg = JSON.stringify(
                  {action: "synthesis_doSearch", module: "synthesis", cid: cid}
                )

                leonSocket.send(msg)

                var pb = $("#searchProgress")
                var pbb = $("#searchProgress .bar")

                $("#searchDialog").modal("show")
                //$("#searchDialog").dialog({
                //    modal: true,
                //    width: 500,
                //    buttons: {
                //        Cancel: function() {
                //            $(this).dialog("close");
                //        }
                //    },
                //    close: function() {
                //        if (!searchFinished) {
                //            var msg = JSON.stringify(
                //              {action: "synthesis_doCancelSearch", cid: cid}
                //            )

                //            leonSocket.send(msg)
                //            searchFinished = true;
                //        }
                //    }
                //});
            }
            e.preventDefault();
            return false;
        });
    }

    var storedCode = localStorage.getItem("leonEditorCode")
    if (storedCode != null) {
        editor.setValue(storedCode);
        editor.selection.clearSelection();
        editor.gotoLine(0);
    }
});
