var editor = null;

$(document).ready(function() {
    editor = ace.edit("codebox");
    editor.getSession().setMode("ace/mode/scala");
    editor.getSession().setUseWrapMode(true);
    editor.getSession().setTabSize(2);

    var hash = window.location.hash

    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var leonSocket = null

    var headerHeight = 100
    var menuWidth    = 450

    var mode = "normal"
    function togglePresentationMode() {
        if (mode == "normal") {
            $("#actionscolumn").hide()
            menuWidth = 20
            resizeEditor()

            mode = "presentation"
            //$("#button-menu span.label").html("Show menu")
        } else {
            menuWidth = 450
            resizeEditor()
            $("#actionscolumn").show()

            mode = "normal"
            //$("#button-menu span.label").html("Hide menu")
        }
    }

    $("#button-menu").click(function(event) {
        togglePresentationMode()
        if (mode == "presentation") {
            $(this).addClass("ui-state-disabled")
        } else {
            $(this).removeClass("ui-state-disabled")
        }
        event.preventDefault()
    });

    $("#button-save").click(function(event) {
        recompile()
        event.preventDefault()
    });

    $("#button-undo").click(function(event) {
        if (!$(this).hasClass("ui-state-disabled")) {
            doUndo()
        }
        event.preventDefault()
    });

    $("#button-redo").click(function(event) {
        if (!$(this).hasClass("ui-state-disabled")) {
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
        ub.removeClass("ui-state-disabled") 
      } else {
        ub.addClass("ui-state-disabled") 
      }

      if (forwardChanges.length > 0) {
        rb.removeClass("ui-state-disabled") 
      } else {
        rb.addClass("ui-state-disabled") 
      }
    }

    updateUndoRedo()

    function updateCompilationStatus(status) {
        var e = $(".compilation-status")
        var b = $("#button-compilation")
        var bs = $("#button-compilation-status")

        if (status == "success") {
          b.removeClass("ui-state-disabled")
          bs.attr("class", "ui-icon ui-icon-check")

          e.attr("class", "compilation-status success")
          compilationStatus = 1
          e.html("<img src=\""+_leon_prefix+"/assets/images/accept.png\" />")
        } else if (status == "failure") {
          b.removeClass("ui-state-disabled")
          bs.attr("class", "ui-icon ui-icon-alert")

          e.attr("class", "compilation-status failure")
          compilationStatus = -1
          e.html("<img src=\""+_leon_prefix+"/assets/images/exclamation.png\" />")
        } else if (status == "disconnected") {
          b.addClass("ui-state-disabled")
          bs.attr("class", "ui-icon ui-icon-alert")

          e.attr("class", "compilation-status failure")
          compilationStatus = 0
          e.html("<img src=\""+_leon_prefix+"/assets/images/exclamation.png\" />")
        } else if (status == "unknown") {
          b.removeClass("ui-state-disabled")
          bs.attr("class", "ui-icon ui-icon-refresh")

          e.attr("class", "compilation-status")
          compilationStatus = 0
          e.html("<img src=\""+_leon_prefix+"/assets/images/loader.gif\" />")
        } else {
            alert("Unknown status: "+status)
        }
    }

    $("#button-permalink").click(function(event) {
        if (!$(this).hasClass("ui-state-disabled")) {
            var msg = JSON.stringify(
              {action: "storePermaLink", code: editor.getValue()}
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

    handlers["verification_overview"] = function(data) {
        console.log(data)
        var t = $("#overview_table")
        var html = "";
        var o = data.overview

        for (var i = 0; i < o.length; i++) {
           var f = o[i] 
           var status = ""
           var time = f.time.toFixed(2)
           switch(f.status) {
            case "undefined":
                status = "<img src=\""+_leon_prefix+"/assets/images/loader.gif\" />";
                time   = ""
                break;
            case "cond-valid":
                status = "<img src=\""+_leon_prefix+"/assets/images/error.png\" />";
                break;
            case "valid":
                status = "<img src=\""+_leon_prefix+"/assets/images/accept.png\" />";
                break;
            case "invalid":
                status = "<img src=\""+_leon_prefix+"/assets/images/exclamation.png\" />";
                break;
            case "timeout":
                status = "<img src=\""+_leon_prefix+"/assets/images/hourglass.png\" />";
                break;
           }

           var vcDetails = ""
           if (time != "") {
            vcDetails = " vc=\""+i+"\""
           }
           html += "<tr><td>"+f.name+"</td><td class=\"status\""+vcDetails+">"+status+"</td><td class=\"time\""+vcDetails+">"+time+"</td></tr>"
        }

        t.html(html);

        $("td[vc]").click(function () {
            var vcId = $(this).attr("vc")
            displayVerificationDetails(o[vcId].status, o[vcId].vcs, true)
        });
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

    handlers["synthesis_proof"] = function (data) {
        if (data.status == "started") {
            $("#searchProgress").progressbar("value", false)
            $("#searchProgress .progress-label").text("Verifying solution...")
        } else {
            $("#searchProgress").progressbar("value", 100)
        }
    }

    handlers["synthesis_search"] = function (data) {
        if (data.action == "progress") {
            var pc = (data.closed*100)/data.total;
            $("#searchProgress").progressbar("value", pc)
            $("#searchProgress .progress-label").text(data.closed + " / "+data.total)
        } else if (data.action == "result") {
            searchFinished = true
            $("#searchDialog").dialog("close");
        }
    }

    function displayVerificationDetails(status, vcs, doOpen) {
        var pb = $("#verifyProgress")
        if (doOpen) {
            pb.progressbar({
                value: 100,
            });

            openVerifyDialog()
        } else {
            pb.progressbar("value", 100);
        }

        var pbValue = $("#verifyProgress").find(".ui-progressbar-value");
        pbValue.removeClass("cond-valid valid invalid timeout")

        switch (status) {
            case "cond-valid":
                $("#verifyProgress .progress-label").text("Conditionally Valid!")
                pbValue.addClass("cond-valid")
                break;

            case "valid":
                $("#verifyProgress .progress-label").text("Valid!")
                pbValue.addClass("valid")
                break;

            case "invalid":
                $("#verifyProgress .progress-label").text("Invalid!")
                pbValue.addClass("invalid")
                break;

            case "timeout":
                $("#verifyProgress .progress-label").text("Timeout!")
                pbValue.addClass("timeout")
                break;
        }

        var tbl = $("#verifyResults tbody")
        tbl.html("");

        for (var i = 0; i < vcs.length; i++) {
            var vc = vcs[i];
            var icon = "check"
            if (vc.status == "invalid") {
                icon = "alert"
            } else if (vc.status == "unknown") {
                icon = "help"
            }


            tbl.append("<tr class=\""+((i%2 == 0) ? "odd " : "")+vc.status+"\"> <td>"+vc.fun+"</td> <td>"+vc.kind+"</td> <td><span class=\"ui-icon ui-icon-"+icon+"\"></span>"+vc.status+"</td> <td>"+vc.time+"</td> </tr>")

            if ("counterExample" in vc) {
                var html = "<tr class=\""+((i%2 == 0) ? "odd " : "")+"counter-example\"><td colspan=\"4\"><div><div>The following example violates the VC:</div><table>";

                for (var v in vc.counterExample) {
                    html += "<tr><td>"+v+"</td><td><span class=\"ui-icon ui-icon-arrowthick-1-e\"></span></td><td>"+vc.counterExample[v]+"</td></tr>";
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
                  {action: "accessPermaLink", link: hash.substr("#link/".length)}
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

    function connectWS() {
        leonSocket = new WS(_leon_websocket_url)
        leonSocket.onopen = openEvent
        leonSocket.onmessage = receiveEvent
        leonSocket.onclose = closeEvent
        leonSocket.onerror = errorEvent
    }

    var lastChange      = new Date().getTime();
    var lastSavedChange = lastChange;
    var timeWindow      = 2000;

    function updateSaveButton() {
        var e = $("#button-save")
        if (lastChange == lastSavedChange) {
           e.addClass("ui-state-disabled"); 
        } else {
           e.removeClass("ui-state-disabled"); 
        }
    }

    function notify(content, type, fade) {
        if (!fade) {
            fade = 3000
        }

        var note = $("<div>", {
            "class": type
        }).html(content)

        note.hide();

        $("#notifications").append(note);
        note.show("fade");
        setTimeout(function() {
            note.hide("fade");
        }, fade)
    }

    var oldCode = ""

    function recompile() {
        if (oldCode != "" && oldCode != editor.getValue()) {
            if (forwardChanges.length == 0) { 
                storeCurrent(oldCode)
            }
        }
        oldCode = editor.getValue();

        if (connected) {
            updateCompilationStatus("unknown")

            var msg = JSON.stringify(
              {action: "doUpdateCode", code: editor.getValue()}
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
            recompile()
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

    setTimeout(onCodeUpdate, timeWindow+50)

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

        $('#codecolumn')
            .height($(window).height()-headerHeight)
            .width($(window).width()-menuWidth);


        $('#leoninput')
            .height($(window).height()-headerHeight)
            .width($(window).width()-menuWidth);

        $('#codebox')
            .height($(window).height()-headerHeight)
            .width($(window).width()-menuWidth);


        editor.resize();
    };

    $(window).resize(resizeEditor);

    resizeEditor();

    var chooseRulesDisplayer = null

    handlers["synthesis_choose_rules"] = function(data) {
        if (chooseRulesDisplayer != null) {
            chooseRulesDisplayer(data.cid, data.rulesApps)
        } else {
            error("I don't know how to display this..");
        }
    }

    handlers["replace_code"] = function(data) {
        storeCurrent(editorSession.getValue())
        editorSession.setValue(data.newCode)
        recompile()
    }

    var currentMousePos = { x: -1, y: -1 };

    $(document).mousemove(function(event) {
        currentMousePos = { x: event.pageX, y: event.pageY };
    });

    editorSession.selection.on('changeCursor', function(e) {
        var cursor = editorSession.selection.getCursor()

        var token     = editorSession.getTokenAt(cursor.row, cursor.column);

        $("#editorMenu").hide();

        if (token != null) {
            var screenX = currentMousePos.x
            var screenY = currentMousePos.y

            var prevToken = editorSession.getTokenAt(cursor.row, token.start-1);

            if (token.type == "identifier.choose" && token.value == "choose" && context == "synthesis") {

                if (compilationStatus == 1) {
                    var msg = JSON.stringify({
                        action: "synthesis_getRulesToApply",
                        chooseLine: cursor.row+1,
                        chooseColumn: token.start+1,
                    })

                    chooseRulesDisplayer = function(cid, rulesApp) {
                        return synthesisDisplayMenu(screenX, screenY, cid, rulesApp);
                    }

                    leonSocket.send(msg)
                } else {
                    synthesisDisplayMenu(screenX, screenY, 0, []);
                }
            } else if (token.type == "identifier" && prevToken != null && prevToken.type == "keyword" && prevToken.value == "def" && context == "verification") {
                var found = false;
                var annots = editorSession.getAnnotations();
                for (var i = 0; i < annots.length && !found; i++) {
                    if (annots[i].row == cursor.row) {
                        found = true;
                    }
                }
                if (found) {
                    verificationDisplayMenu(screenX, screenY, token.value);
                }
                
            }
        }
    });

    function verificationDisplayMenu(screenX, screenY, fname) {
        $("#editorMenu").html("");

        if (compilationStatus == 1) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-circle-triangle-s"></span>Automated Verification:</a></li>');

            $("#editorMenu").append('<li action="verify" fname="'+fname+'"><a href="#">Verify</a></li>');
        } else if (compilationStatus == 0) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Not yet compiled...</a></li>');
        } else {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Compilation failed!</a></li>');
        }

        $("#editorMenu").menu({
            select: function( event, ui) {
                var r = ui.item;

                if (r.attr("action") == "verify") {
                    verifyFun(r.attr("fname"))
                }

                $("#editorMenu").hide();
                event.preventDefault();
            }
        });

        $("#editorMenu").show();
        $("#editorMenu").menu("refresh");
        $("#editorMenu").css({
            "position": "absolute",
            "top": screenY+"px",
            "left": screenX+"px",
            "z-index": 100
        });
    }

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
        $("#verifyDialog").dialog({
            modal: true,
            width: 500,
            buttons: {
                Cancel: function() {
                    $(this).dialog("close");
                }
            },
            close: function() {
                if (cancelOnClose) {
                    var msg = JSON.stringify(
                      {action: "verification_doCancel", fname: cancelOnClose}
                    )

                    leonSocket.send(msg)
                }
            }
        });
    }

    function verifyFun(fname) {
        var msg = JSON.stringify(
          {action: "verification_doVerify", fname: fname}
        )

        leonSocket.send(msg)

        var pb = $("#verifyProgress")
        var pbl = $("#verifyProgress .progress-label")
        var pbv = pb.find(".ui-progressbar-value")

        pbl.text("Verifying...");

        pb.progressbar({
            value: false,
            complete: function() {
                pbl.text("Complete!");
            }
        });

        pbv.removeClass("failure").removeClass("success")
        $("#verifyResults").hide();

        openVerifyDialog(fname)
    }

    function synthesisDisplayMenu(screenX, screenY, cid, rulesApps) {
        $("#editorMenu").html("");

        if (compilationStatus == 1) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-circle-triangle-s"></span>Automated Search:</a></li>');

            $("#editorMenu").append('<li'+clazz+' action="search" cid="'+cid+'"><a href="#">Search</a></li>');

            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-circle-triangle-s"></span>Apply Rule:</a></li>');

            for (var i = 0; i < rulesApps.length; i++) {
                var app = rulesApps[i];
                var statusIcon = ""
                var clazz = ""

                if (app.status == "closed") {
                    statusIcon = '<span class="ui-icon ui-icon-alert"></span>'
                    clazz = ' class="ui-state-disabled"'
                }
                $("#editorMenu").append('<li'+clazz+' action="rule" cid="'+cid+'" rid="'+app.id+'"><a href="#">'+statusIcon+app.name+'</a></li>');
            }
        } else if (compilationStatus == 0) {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Not yet compiled...</a></li>');
        } else {
            $("#editorMenu").append('<li class="ui-state-disabled"><a href="#"><span class="ui-icon ui-icon-alert"></span>Compilation failed!</a></li>');
        }

        $("#editorMenu").menu({
            select: function( event, ui) {
                var r = ui.item;

                if (r.attr("action") == "rule") {
                    var msg = JSON.stringify(
                      {action: "synthesis_doApplyRule", cid: 1*r.attr("cid"), rid: 1*r.attr("rid")}
                    )

                    leonSocket.send(msg)
                } else if (r.attr("action") == "search") {
                    var cid = 1*r.attr("cid")

                    searchFinished = false

                    var msg = JSON.stringify(
                      {action: "synthesis_doSearch", cid: cid}
                    )

                    leonSocket.send(msg)

                    var pb = $("#searchProgress")
                    var pbl = $("#searchProgress .progress-label")

                    pb.progressbar({
                        value: false,
                        complete: function() {
                            pbl.text("Complete!");
                        }
                    });

                    $("#searchDialog").dialog({
                        modal: true,
                        width: 500,
                        buttons: {
                            Cancel: function() {
                                $(this).dialog("close");
                            }
                        },
                        close: function() {
                            if (!searchFinished) {
                                var msg = JSON.stringify(
                                  {action: "synthesis_doCancelSearch", cid: cid}
                                )

                                leonSocket.send(msg)
                                searchFinished = true;
                            }
                        }
                    });
                }

                $("#editorMenu").hide();
                event.preventDefault();
            }
        });

        $("#editorMenu").show();
        $("#editorMenu").menu("refresh");
        $("#editorMenu").css({
            "position": "absolute",
            "top": screenY+"px",
            "left": screenX+"px",
            "z-index": 100

        });
    }

    var storedCode = localStorage.getItem("leonEditorCode")
    if (storedCode != null) {
        editor.setValue(storedCode);
        editor.selection.clearSelection();
        editor.gotoLine(0);
    }
});
