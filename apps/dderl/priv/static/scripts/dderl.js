var OpsBufEnum = { APPEND  : 1
                 , PREPEND : 2
                 , REPLACE : 3
                 };

var OpsFetchEnum = { NEXT     :1
                   , PREVIOUS :2
                   , JUMPNEXT :3
                   , JUMPPREV :4
                   , TOEND    :5
                   , TOBEGIN  :6
                   , RELOAD   :7
                   };

if(Object.hasOwnProperty('freeze')) {
    Object.freeze(OpsBufEnum);
    Object.freeze(OpsFetchEnum);
}

String.prototype.visualLength = function()
{
    var ruler = $('#txtlen');
    ruler.html(''+this);
    return ruler.width();
}

function getUniqueTime() {
  var time = new Date().getTime();
  while (time == new Date().getTime());
  return new Date().getTime();
}

var dderlState = {
    session: null,
    adapter: null,
    connection: null,
    ws: null,
    pingTimer: null,
    currentErrorAlert: null,
    dashboards: null,
    currentDashboard: null,
    currentViews: null,
    currentWindows: new Array(),
    saveDashboardCounter: 0
}

// generic dderlserver call interface
// TODO: currently the widget and non-widget
//       is determined by the presence of the
//       context variable for widget there is
//       no this['context']
function ajaxCall(_ref,_url,_data,_resphead,_successevt) {
    resetPingTimer();
    var self = _ref;

    // if data is JSON object format to string
    if(_data == null) _data = JSON.stringify({});
    else
        try {
            _data = JSON.stringify(_data);
        } catch (ex) {
            console.error(_data + ' is not JSON');
            throw(ex);
        }

    console.log('[AJAX] TX '+_url);

    var headers = new Object();
    if (dderlState.adapter != null) headers['adapter'] = dderlState.adapter;
    headers['dderl_sess'] = (dderlState.session != null ? '' + dderlState.session : '');
    if (null != self) {
        if(self.hasOwnProperty('_session')) headers['dderl_sess'] = self._session;
        if(self.hasOwnProperty('_adapter')) headers['adapter'] = self._adapter;
    }

    $.ajax({
        type: 'POST',
        url: _url,
        data: _data,
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: self,

        success: function(_data, textStatus, request)
        {
            console.log('Request '+_url+' Result '+textStatus);

            // Save the session if the request was to log in.
            if(_url == '/app/login') {
                var s = request.getResponseHeader('dderl_sess');
                console.log("The session response header dderl_sess");
                console.log(s);
                dderlState.session = s;
            }

            if(request.status === 204) {
                console.error('204 received for the request ' + _url);
            } else if(!_data)
                throw('null data received for the request '+_url);
            else if(_data.hasOwnProperty(_resphead)) {
                console.log('[AJAX] RX '+_resphead);
                if(this.hasOwnProperty('context') && null == this.context) {
                    if(null === _successevt)
                        console.log('no success callback for '+_url);
                    else if($.isFunction(_successevt))
                        _successevt(_data[_resphead]);
                    else
                        throw('unsupported success event '+_successevt+' for '+_url);
                } else {
                    if(this._handlers.hasOwnProperty(_successevt))
                        this.element.trigger(_successevt, _data[_resphead]);
                    else
                        throw('unsupported success event '+_successevt+' for '+_url);
                }
            }
            else if(_data.hasOwnProperty('error')) {
                if(!dderlState.currentErrorAlert || !dderlState.currentErrorAlert.hasClass('ui-dialog-content')) {
                    dderlState.currentErrorAlert = alert_jq('Error : '+_data.error);
                }
            }
            else {
                console.log(_data);
                throw('resp doesn\'t match the request '+_url);
            }
        },

        error: function (request, textStatus, errorThrown) {
            if(_url == '/app/ping') {
                _successevt("error");
            } else {
                if(!dderlState.currentErrorAlert || !dderlState.currentErrorAlert.hasClass('ui-dialog-content')) {
                    dderlState.currentErrorAlert = alert_jq('HTTP Error'+
                        (textStatus.length > 0 ? ' '+textStatus:'') +
                        (errorThrown.length > 0 ? ' details '+errorThrown:''));
                }
            }
        }
    });
}

/*** TODO: Move this to dashboard container class dderl.dashboard ***/
function loadDashboard(dashboard) {
    $(".ui-dialog-content").dialog('close');
    dashboard.openViews();
}

function requestDashboards() {
    ajaxCall(null, '/app/dashboards', null, 'dashboards', function(dashboards) {
        var dashboard, view, viewLayout;
        for(var i = 0; i < dashboards.length; ++i) {
            dashboard = new DDerl.Dashboard(dashboards[i].id, dashboards[i].name, []);
            for(var j = 0; j < dashboards[i].views.length; ++j) {
                view = dashboards[i].views[j];
                viewLayout = view.layout;
                dashboard.addView(
                    new DDerl.DashView(view.id,
                                       viewLayout.x,
                                       viewLayout.y,
                                       viewLayout.width,
                                       viewLayout.height)
                );
            }
            addDashboard(dashboard);
        }
    });
}

//TODO: Check if this can be merged with the windows handler...
function addToCurrentViews(tableView) {
    // Bind to the close event to remove it from the list.
    tableView._dlg.bind("dialogclose", function(event, ui) {
        var viewPos = dderlState.currentViews.indexOf(tableView);
        if(viewPos != -1) {
            dderlState.currentViews.splice(viewPos, 1);
        }
    });
    dderlState.currentViews.push(tableView);
}

function getCurrentViews() {
    var resultViews, id, x, y, w, h;
    resultViews = new Array();
    for(var i = 0; i < dderlState.currentViews.length; ++i) {
        id = dderlState.currentViews[i]._viewId;
        x = dderlState.currentViews[i]._dlg.dialog('widget').position().left;
        y = dderlState.currentViews[i]._dlg.dialog('widget').position().top;
        w = dderlState.currentViews[i]._dlg.dialog('widget').width();
        h = dderlState.currentViews[i]._dlg.dialog('widget').height();
        resultViews.push(new DDerl.DashView(id, x, y, w, h));
    }
    return resultViews;
}

function addDashView(id, x, y, width, height) {
    dderlState.currentDashboard.addView(new DDerl.DashView(id, x, y, width, height));
}

function removeDashView(viewId) {
    dderlState.currentDashboard.removeView(viewId);
}

function findDashboardById(dashboardId) {
    for(var i = 0; i < dderlState.dashboards.length; ++i) {
        if(dderlState.dashboards[i].getId() === dashboardId) {
            return dderlState.dashboards[i];
        }
    }
    return null;
}

function findDashboard(name) {
    for(var i = 0; i < dderlState.dashboards.length; ++i) {
        if(dderlState.dashboards[i].getName() === name) {
            return dderlState.dashboards[i];
        }
    }
    return null;
}

function addDashboard(dashboard) {
    var addedOption, dashboardList;

    dderlState.dashboards.push(dashboard);

    addedOption = document.createElement("option");
    addedOption.value = dashboard.getId();
    addedOption.textContent = dashboard.getName();

    dashboardList = document.getElementById("dashboard-list");
    dashboardList.appendChild(addedOption);
}

function checkTablesNotSaved() {
    var tablesNotSaved, notSavedTitles, message;
    tablesNotSaved = new Array();
    notSavedTitles = "";
    message = "";
    if(dderlState.currentWindows.length === dderlState.currentViews.length) {
        saveDashboard();
    } else {
        notSavedTitles += "<ul>"
        for(var i = 0; i < dderlState.currentWindows.length; ++i) {
            if(!dderlState.currentWindows[i]._viewId) {
                tablesNotSaved.push(dderlState.currentWindows[i]);
                notSavedTitles += "<li>" + dderlState.currentWindows[i].options.title + "</li>";
            }
        }
        notSavedTitles += "</ul>"
        message = "The following tables are not saved as views: <br>" + notSavedTitles;
        $('<div><p><span class="ui-icon ui-icon-alert" style="float: left; margin: 0 7px 20px 0;"></span>'+ message +'</p></div>').appendTo(document.body).dialog({
            resizable: false,
            width: 450,
            height:220,
            modal: true,
            buttons: {
                "Create views": function() {
                    $( this ).dialog( "close" );
                    dderlState.saveDashboardCounter += tablesNotSaved.length;
                    for(var i = 0; i < tablesNotSaved.length; ++i) {
                        tablesNotSaved[i].saveView();
                    }
                },
                "Ignore tables": function() {
                    $( this ).dialog( "close" );
                    saveDashboard();
                },
                Cancel: function() {
                    $( this ).dialog( "close" );
                }
            },
            close : function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        });
    }
}

function saveDashboardWithCounter() {
    if(dderlState.saveDashboardCounter === 1) {
        saveDashboard();
    } else if(dderlState.saveDashboardCounter > 0) {
        dderlState.saveDashboardCounter -= 1;
    }
}

function saveDashboard() {
    var name, dashboard, dashViews;

    name = document.getElementById("dashboard-list-input").value;
    if(name === "default") {
        alert_jq("Please select a name for the dashboard");
        return;
    }

    dashboard = findDashboard(name);
    dashViews = getCurrentViews();

    if(dashboard === null) {
        dashboard = new DDerl.Dashboard(-1, name, dashViews);
        dashboard.save(function() {
            addDashboard(dashboard);
        });
    } else {
        dashboard.updateViews(dashViews);
        dashboard.save();
    }
}

function createDashboardMenu(container) {
    var mainMenuBar, saveButton, dashboardList, dashboardListObj, defaultOption;

    // Check to only create the elements once.
    if(document.getElementById("dashboard-list")) {
        return;
    }

    // Button creation
    saveButton = document.createElement("input");
    saveButton.type = "button";
    saveButton.id = "dashboard-save";
    saveButton.value = "Save this dash";
    saveButton.onclick = function() {
        checkTablesNotSaved();
    }

    // Default option creation
    defaultOption = document.createElement("option");
    defaultOption.value = "default";
    defaultOption.textContent = "default";

    // Dashboard list creation
    dashboardList = document.createElement("select");
    dashboardList.id = "dashboard-list";
    dashboardList.appendChild(defaultOption);

    // Add elements to the dom
    container.appendChild(dashboardList);
    container.appendChild(saveButton);

    // Convert the select to combobox
    dashboardListObj = $('#dashboard-list').combobox();

    // Add the handler for selection
    dashboardListObj.change(function() {
        var dashId, dashboard;
        dashId = dashboardListObj.val();
        if($('#dashboard-list-input').is(":focus")) {
            $('#dashboard-list-input').blur();
        }
        if(dashId === "default") {
            return;
        } else if(!isNaN(parseInt(dashId)) && isFinite(dashId)) {
            dashboard = findDashboardById(parseInt(dashId));
            if(dashboard) {
                loadDashboard(dashboard);
            }
        }
    });
}

function initDashboards() {
    dderlState.dashboards = new Array();
    dderlState.currentDashboard = new DDerl.Dashboard(-1, "default", []);
    dderlState.currentViews = new Array();
    createDashboardMenu(document.getElementById("dashboard-menu"));
    var userDashboards = requestDashboards();
}
/********** End dashboard functions *********************/


function resetPingTimer() {
    if(dderlState.pingTimer) {
        clearTimeout(dderlState.pingTimer);
    }

    //Stop ping if there is no session.
    if(!dderlState.session) {
        console.log("ping canceled");
        return;
    }

    dderlState.pingTimer = setTimeout(
        function() {
            ajaxCall(null, '/app/ping', null, 'ping', function(response) {
                console.log("ping " + response);
                if(response != "pong") {
                    alert_jq("Failed to reach the server, the connection might be lost.");
                    clearTimeout(dderlState.pingTimer);
                }
            });
        },
    30000); // Ping time 30 secs.
}

function login_first()
{
    alert("Please log in first!");
}

function show_qry_files(useSystem)
{
    var loggedInUser = $('#change-pswd-button').data("logged_in_user");
    if(loggedInUser == undefined || loggedInUser.length == 0) {
        login_first();
        return;
    }
    $('<div>')
    .appendTo(document.body)
    .table({
        autoOpen    : false,
        dderlConn   : dderlState.connection,
        dderlAdapter: dderlState.adapter,
        title       : "All Views"
    })
    .table('loadViews', useSystem);
}

function import_query()
{
    $('<form id="fileuploader" enctype="multipart/form-data" method="post" action="/app/upload"></form>')
        .append($('<input type="file" id="fileToUpload" style="position:absolute; top:-100px;">')
                    .change(function() {
                        uploadFile(this.files[0]);
                    }))
        .appendTo(document.body);
    $("#fileToUpload").click();
}

function uploadFile(file) {
    console.log(file);
    var xhr = new XMLHttpRequest();
    var fd = new FormData();
    fd.append("fileToUpload", file);
  
    /* event listners */
    xhr.upload.addEventListener("progress", function(e) {console.log("uploading...");}, false);
    xhr.addEventListener("load", function(e) {
            var fileObj = JSON.parse(e.target.responseText).upload;
            console.log(fileObj);
            StartSqlEditorWithTitle(fileObj.name, fileObj.content);
        }, false);
    xhr.addEventListener("error", function(e) {console.log("upload error!");}, false);
    xhr.addEventListener("abort", function(e) {console.log("upload cancled!");}, false);
    /* Be sure to change the url below to the url of your upload server side script */
    xhr.open("POST", "/app/upload");
    xhr.setRequestHeader('dderl_sess', (dderlState.session != null ? '' + dderlState.session : ''));
    xhr.send(fd);
}

function show_more_apps() {
    if($(".extra-app").css('display') === 'none') {
        $(".extra-app").css('display', '');
        $("#more-apps-link").html("-").css('text-decoration', 'none');
    } else {
        $(".extra-app").css('display', 'none');
        $("#more-apps-link").html("+").css('text-decoration', 'none');
    }
}


/* Escape new lines and tabs */
function escapeNewLines(str)
{
    var result = "";
    for(var i = 0; i < str.length; ++i) {
        if(str.charCodeAt(i) === 9) {
            result += "\\t";
        } else if(str.charCodeAt(i) === 10) {
            result += "\\n";
        } else if(str.charCodeAt(i) !== 13) {
            result += str[i];
        }
    }
    return result;
}

function unescapeNewLines(str) {
    str = str.replace(/\\t/gi, "\t");
    str = str.replace(/\\n/gi, "\n");
    return unescape(str);
}

function get_local_apps(table) {
    ajaxCall(null, '/app/about', null, 'about', function(applications) {
        var apps = '';
        for(app in applications) {
            var version = applications[app].version;
            if(app === "dderl") {
            } else if(applications[app].dependency) {
                apps += '<tr>';
                apps += '<td class="about-dep-name">' + app + '</td>';
                apps += '<td class="about-dep-vsn">' + version + '</td>';
                apps += '</tr>';
            } else {
                apps += '<tr class="extra-app">';
                apps += '<td class="about-dep-name">' + app + '</td>';
                apps += '<td class="about-dep-vsn">' + version + '</td>';
                apps += '</tr>';
            }
        }
        table.html(apps);
        $("#more-apps-link").css('display', '');
        show_more_apps();
    });
}

function get_remote_apps(table) {
    ajaxCall(null, '/app/remote_apps', {remote_apps : {connection: dderlState.connection}}, 'remote_apps', function(applications) {
        var extra_apps = '';
        for(app in applications) {
            var version = applications[app].version;
            if(app !== "dderl") {
                extra_apps += '<tr class="extra-app">';
                extra_apps += '<td class="about-dep-name">' + app + '</td>';
                extra_apps += '<td class="about-dep-vsn">' + version + '</td>';
                extra_apps += '</tr>';
            }
        }
        table.html(extra_apps);
        $("#more-apps-link").css('display', 'none');
    });
}

function show_about_dlg()
{
    ajaxCall(null, '/app/about', null, 'about', function(applications) {
        var aboutDlg =
            $('<div id="about-dderl-dlg" title ="About"></div>')
            .appendTo(document.body);

        if(dderlState.connection) {
            aboutDlg.append('<div class="remote-apps"><a id="remote-apps-link" title="Show all remote apps" href="#">show remote</a></div>');
        }

        var table = '<table class="about-deps-table" cellspacing="5" border="0">';
        var extra_apps = '';
        for(app in applications) {
            var version = applications[app].version;
            if(app === "dderl") {
                var description = applications[app].description;
                var p = '<p class="about-title">DDerl</p>';
                p += '<p class="about-vsn">Version ' + version + '</p>';
                p += '<p class="about-vsn">Gui Version 1.0.6</p>';
                p += '<p class="about-desc">' + description + '</p>';
                p += '<hr>'
                aboutDlg.prepend(p);
            } else if(applications[app].dependency) {
                table += '<tr>';
                table += '<td class="about-dep-name">' + app + '</td>';
                table += '<td class="about-dep-vsn">' + version + '</td>';
                table += '</tr>';
            } else {
                extra_apps += '<tr class="extra-app">';
                extra_apps += '<td class="about-dep-name">' + app + '</td>';
                extra_apps += '<td class="about-dep-vsn">' + version + '</td>';
                extra_apps += '</tr>';
            }
        }
        table += '</table>';
        table = $(table).append(extra_apps);
        aboutDlg.append(table);

        var isLocal = true;
        aboutDlg.find('#remote-apps-link').click(
            function(evt) {
                evt.preventDefault();
                var apps;
                if(isLocal) {
                    isLocal = false;
                    $(this).html("show local");
                    apps = get_remote_apps(table);
                } else {
                    isLocal = true;
                    $(this).html("show remote");
                    apps = get_local_apps(table);
                }
            }
        );

        var divMore = '<div class="about-more"><a id="more-apps-link" title="Show all running apps" href="#" onclick="show_more_apps()">+</a></div>';
        aboutDlg.append(divMore);

        aboutDlg.dialog({
            modal:false,
            width: 230,
            resizable:false,
            open: function() {
                $(this).dialog("widget").appendTo("#main-body");
            },
            close: function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        }).dialog("widget").draggable("option","containment","#main-body");
        show_more_apps();
    });
}

function alert_jq(string)
{
    var dlgDiv =
        $('<div>')
        .appendTo(document.body)
        .append('<p><span class="ui-icon ui-icon-info" style="float: left; margin: 0 7px 50px 0;"></span>'+string+'</p>')
        .dialog({
            modal:false,
            width: 300,
            height: 300,
            title: "DDerl message",
            open: function() {
                $(this).dialog("widget").appendTo("#main-body");
            },
            close: function() {
                //We have to remove the added child p
                dlgDiv.dialog('destroy');
                dlgDiv.remove();
                dlgDiv.empty();
            }
        });
    dlgDiv.dialog("widget").draggable("option","containment","#main-body");
    return dlgDiv;
}

function create_ws(url)
{
    if(!dderlState.ws) {
        dderlState.ws = $.bullet(url, {});

        dderlState.ws.onopen = function(){
            console.log('WebSocket: opened');
            dderlState.ws.send("time");
        };
        dderlState.ws.onclose = function(){
            console.log('WebSocket: closed');
            $('#server-time').text("");
        };
        dderlState.ws.onmessage = function(e) {
            if(e.data != 'pong') {
                $('#server-time').text(e.data);
            }
        };
        dderlState.ws.onheartbeat = function() {
			dderlState.ws.send('ping');
        };
        dderlState.ws.ondisconnect = function() {
            console.log('WebSocket: disconnected');
        }
    }
}

function beep()
{
    var beepStorage = sessionStorage.getItem("beep-sound");
    var beep = $("#beep-sound")[0];

    if (beepStorage) {
        // Reuse existing Data URL from sessionStorage
        beep.setAttribute("src", beepStorage);
    } else if (typeof(FileReader) === "function") { //I.E. 9 doesn't support FileReader
        // Create XHR and FileReader objects
        var xhr = new XMLHttpRequest();
        var fileReader = new FileReader();

        xhr.open("GET", beep.currentSrc, true);
        // Set the responseType to blob
        xhr.responseType = "blob";

        xhr.addEventListener("load", function () {
            if (xhr.status === 200) {
                // onload needed since Google Chrome doesn't support addEventListener for FileReader
                fileReader.onload = function (evt) {
                    // Read out file contents as a Data URL
                    var result = evt.target.result;
                    beep.setAttribute("src", result);
                    // Store Data URL in sessionStorage
                    try {
                        sessionStorage.setItem("beep-sound", result);
                    }
                    catch (e) {
                        console.log("Storage failed: " + e);
                    }
                };
                // Load blob as Data URL
                fileReader.readAsDataURL(xhr.response);
            }
        }, false);
        // Send XHR
        xhr.send();
    }
    beep.load();
    beep.play();
}

$(".grid-header .g-ui-icon").addClass("ui-state-default ui-corner-all");

// In some environment, console is defined but console.log or console.error is missing.
if (window.console && window.console.log && window.console.error) {
    console.log('console log is defined');
} else {
    window['console'] = {log: function(){ }, error: function(){ }};
    console.log('dummy console is created');
}

function smartDialogPosition(container, owner, self, checks)
{
    if(!checks || checks.length === 0) {
        checks = ['right'];
    }
    var dlg = self.dialog("widget");
    var ownerDlg = owner.dialog("widget");
    for(var i = 0; i < checks.length; ++i) {
        var haveSpace = false;
        var newPos = {at: 'left bottom', my : 'left top', of: ownerDlg};
        switch(checks[i]) {
        case 'left':
            haveSpace = ownerDlg.position().left > dlg.width();
            newPos = {at: 'left top', my : 'right top', of: ownerDlg};
            break;
        case 'right':
            haveSpace = container.width() - ownerDlg.position().left - ownerDlg.width() > dlg.width();
            newPos = {at: 'right top', my : 'left top', of: ownerDlg};
            break;
        case 'top':
            haveSpace = ownerDlg.position().top > dlg.height();
            newPos = {at: 'left top', my : 'left bottom', of: ownerDlg};
            break;
        case 'bottom':
            haveSpace = container.height() - ownerDlg.position().top - ownerDlg.height() > dlg.height();
            newPos = {at: 'left bottom', my : 'left top', of: ownerDlg};
            break;
        }

        //The last check is the default pos.
        if((i === checks.length - 1) || haveSpace) {
            self.dialog("option", "position", newPos);
            break;
        }
    }
}

function findFreeSpace(self) {
    var currentDlgs = $(".ui-dialog-content");
    var dialogPositions = [];
    for(var i = 0; i < currentDlgs.length; ++i) {
        if($(currentDlgs[i]).dialog('isOpen')) {
            var dlg = $(currentDlgs[i]).dialog("widget");
            var box = {top   : dlg.position().top,
                       left  : dlg.position().left,
                       bottom: dlg.position().top + dlg.height(),
                       right : dlg.position().left + dlg.width()};
            dialogPositions.push(box);
        }
    }
    dialogPositions.sort(function(b1, b2) {return b1.left - b2.left});
    //TODO: Naive implementation, we improve it if it works...
    for(var i = 0; i < $("#main-body").width(); i += 10) {
        for(var j = 0; j < $("#main-body").height(); j += 10) {
        }
    }
    console.log(self.dialog("widget").width() + ", " + self.dialog("widget").height());
    //console.log(dialogPositions);
}

function patch_jquery_ui() {
    // Added this to fix the bug: http://bugs.jqueryui.com/ticket/5559
    // it is currently fixed in jquery-ui 1.10, however we can't upgrade
    // until this bug is also fixed http://bugs.jqueryui.com/ticket/9166
    // it will be probably be fixed on versio 1.11.
    $.ui.plugin.add("resizable", "alsoResize", {
        start: function () {
            var that = $(this).data("ui-resizable"),
            o = that.options,
            _store = function (exp) {
                $(exp).each(function() {
                    var el = $(this);
                    el.data("ui-resizable-alsoresize", {
                        width: parseInt(el.width(), 10), height: parseInt(el.height(), 10),
                        left: parseInt(el.css('left'), 10), top: parseInt(el.css('top'), 10)
                    });
                });
            };

            if (typeof(o.alsoResize) === 'object' && !o.alsoResize.parentNode) {
                if (o.alsoResize.length) { o.alsoResize = o.alsoResize[0]; _store(o.alsoResize); }
                else { $.each(o.alsoResize, function (exp) { _store(exp); }); }
            }else{
                _store(o.alsoResize);
            }
        },

        resize: function (event, ui) {
            var that = $(this).data("ui-resizable"),
            o = that.options,
            os = that.originalSize,
            op = that.originalPosition,
            delta = {
                height: (that.size.height - os.height) || 0, width: (that.size.width - os.width) || 0,
                top: (that.position.top - op.top) || 0, left: (that.position.left - op.left) || 0
            },

            _alsoResize = function (exp, c) {
                $(exp).each(function() {
                    var el = $(this), start = $(this).data("ui-resizable-alsoresize"), style = {},
                    css = c && c.length ? c : el.parents(ui.originalElement[0]).length ? ['width', 'height'] : ['width', 'height', 'top', 'left'];

                    $.each(css, function (i, prop) {
                        var sum = (start[prop]||0) + (delta[prop]||0);
                        if (sum && sum >= 0) {
                            style[prop] = sum || null;
                        }
                    });

                    el.css(style);
                });
            };

            if (typeof(o.alsoResize) === 'object' && !o.alsoResize.nodeType) {
                $.each(o.alsoResize, function (exp, c) { _alsoResize(exp, c); });
            }else{
                _alsoResize(o.alsoResize);
            }
        },

        stop: function () {
            $(this).removeData("resizable-alsoresize");
        }
    });
}

function addWindowFinder(table, title) {
    // Create the elements.
    var windowsList = document.getElementById("window-finder");
    var link = document.createElement("a");
    var li = document.createElement("li");

    // Set the title and the click event.
    if(title.length < 20) {
        link.textContent = title;
    } else {
        link.textContent = title.substring(0, 17) + "...";
    }
    link.href = '#';
    link.onclick = function() {
        if(table && table._dlg && table._dlg.hasClass('ui-dialog-content')) {
            table.moveAllToTop();
        } else {
            // In case we have a invalid entry it is removed.
            windowsList.removeChild(li);
        }
    };

    // Append to the page.
    li.appendChild(link);
    windowsList.appendChild(li);

    // Bind to the close event to remove it from the list.
    table._dlg.bind("dialogclose", function(event, ui) {
        windowsList.removeChild(li);
    });

    table._dlg.bind("dialogclose", function(event, ui) {
        var tablePos = dderlState.currentWindows.indexOf(table);
        if(tablePos != -1) {
            dderlState.currentWindows.splice(tablePos, 1);
        }
    });
    // Add it to the global windows array
    dderlState.currentWindows.push(table);
    return link;
}
