import $ from 'jquery';
import {alert_jq} from '../dialogs/dialogs';
import './dderl.table';
import {change_login_password, showScreeSaver} from './login';
import {change_connect_password} from './connect';

import '../dashboard/dderl.dashView';
import '../dashboard/dderl.dashboard';
import * as DashboardMenu from '../dashboard/DashboardMenu';

export var dderlState = {
    isLoggedIn : false,
    adapter: null,
    connection: null,
    connected_user: null,
    service: null,
    ws: null,
    pingTimer: null,
    currentErrorAlert: null,
    dashboards: null,
    currentViews: null,
    currentWindows: [],
    saveDashboardCounter: 0,
    connections: [],  // List of available connections for the current adapter
    connectionSelected: null,
    copyMode: "normal",             // normal, header, json
    operationLogs: "",
    screensaver: false,
    xsrfToken: "",
    username: "",
    app: "",
    vsn: "",
    node: "",
    host: "",
    rowNumLimit: 10000
};

// generic dderlserver call interface
// TODO: currently the widget and non-widget
//       is determined by the presence of the
//       context variable for widget there is
//       no this['context']
export function ajaxCall(_ref,_url,_data,_resphead,_successevt, _errorevt) {
    resetPingTimer();
    var self = _ref;

    // if data is JSON object format to string
    if(!_data) {
        _data = JSON.stringify({});
    } else {
        try {
            _data = JSON.stringify(_data);
        } catch (ex) {
            console.error(_data + ' is not JSON');
            throw(ex);
        }
    }

    // request url converted to server AJAX map
    _url = 'app/'+_url;

    console.log('[AJAX] TX '+_url);

    var headers = {};
    if (dderlState.adapter !== null) headers['DDERL-Adapter'] = dderlState.adapter;
    if (self) {
        if(self.hasOwnProperty('_adapter')) headers['DDERL-Adapter'] = self._adapter;
    }

    headers["X-XSRF-TOKEN"] = dderlState.xsrfToken;

    $.ajax({
        type: 'POST',
        url: _url,
        data: _data,
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: self,

        success: function(_data, textStatus, request) {
            console.log('Request '+_url+' Result '+textStatus);

            if(this && this.hasOwnProperty('_spinCounter') &&
               this._dlg && this._dlg.hasClass('ui-dialog-content')) {
                this.removeWheel();
            }

            if(request.status === 204) {
                console.error('204 received for the request ' + _url);
            } else if(!_data)
                throw('null data received for the request '+_url);
            else if(_data.hasOwnProperty(_resphead)) {
                console.log('[AJAX] RX '+_resphead);
                if(this.hasOwnProperty('context') &&
                    (this.context === null || this.context === undefined)) {
                    if(null === _successevt) {
                        console.log('no success callback for '+_url);
                    } else if($.isFunction(_successevt)) {
                        _successevt(_data[_resphead]);
                    } else {
                        throw('unsupported success event '+_successevt+' for '+_url);
                    }
                } else {
                    if($.isFunction(_successevt)) {
                        _successevt(_data[_resphead]);
                    } else if(this._handlers.hasOwnProperty(_successevt)) {
                        var result = _data[_resphead];
                        // When the object contains length property it has to be wrapped
                        // in an array, see notes at http://api.jquery.com/trigger/
                        if (result && result.length) {
                            result = [result];
                        }
                        this.element.trigger(_successevt, result);
                    } else {
                        throw('unsupported success event '+_successevt+' for '+_url);
                    }
                }
            } else if(_data.hasOwnProperty('error')) {
                if(_url == 'app/ping') {
                    dderlState.isLoggedIn = false;
                    dderlState.connection = null;
                    dderlState.adapter = null;
                    resetPingTimer();
                }

                if(_data.error == 'screensaver') {
                    dderlState.screensaver = true;
                    showScreeSaver();
                } else if(_url == 'app/logout') {
                    if($.isFunction(_successevt)) {
                        _successevt({error: _data.error});
                    }
                } else if(_url == 'app/check_session') {
                    if($.isFunction(_errorevt)) {
                        _errorevt(_data.error);
                    }
                } else {
                    if(!dderlState.currentErrorAlert || !dderlState.currentErrorAlert.hasClass('ui-dialog-content')) {
                        dderlState.currentErrorAlert = alert_jq('Error : '+_data.error);
                    }
                    if($.isFunction(_errorevt)) {
                        _errorevt(_data.error);
                    }
                }
            } else {
                console.log(_data);
                throw('resp doesn\'t match the request '+_url);
            }
        },

        error: function (request, textStatus, errorThrown) {
            if(this.hasOwnProperty('_spinCounter')) {
                this.removeWheel();
            }

            if($.isFunction(_errorevt)) {
                _errorevt(errorThrown, textStatus);
            } else if(!dderlState.currentErrorAlert || !dderlState.currentErrorAlert.hasClass('ui-dialog-content')) {
                dderlState.currentErrorAlert = alert_jq('HTTP Error'+
                    (textStatus.length > 0 ? ' '+textStatus:'') +
                    (errorThrown.length > 0 ? ' details '+errorThrown:''));
                
            }
        }
    });
}

/*** TODO: Move this functions to apropiate dashboard modules ***/
function requestDashboards() {
    ajaxCall(null, 'dashboards', null, 'dashboards', function(dashboards) {
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
export function addToCurrentViews(tableView) {
    // Bind to the close event to remove it from the list.
    tableView._dlg.bind("dialogclose", function() {
        var viewPos = dderlState.currentViews.indexOf(tableView);
        if(viewPos != -1) {
            dderlState.currentViews.splice(viewPos, 1);
        }
    });
    dderlState.currentViews.push(tableView);
}

export function addDashboard(dashboard) {
    var index = dderlState.dashboards.push(dashboard) - 1;
    DashboardMenu.add(index, dashboard);
}

export function saveDashboardWithCounter() {
    if(dderlState.saveDashboardCounter === 1) {
        DashboardMenu.save();
    } else if(dderlState.saveDashboardCounter > 0) {
        dderlState.saveDashboardCounter -= 1;
    }
}

export function initDashboards() {
    dderlState.dashboards = [];
    dderlState.currentViews = [];
    DashboardMenu.create(document.getElementById("dashboard-menu"));
    requestDashboards();
}
/********** End dashboard functions *********************/


export function resetPingTimer() {
    if(dderlState.pingTimer) {
        clearTimeout(dderlState.pingTimer);
    }

    //Stop ping if there is no session.
    if(!dderlState.isLoggedIn) {
        console.log("ping canceled");
        return;
    }

    dderlState.pingTimer = setTimeout(
        function() {
            ajaxCall(null, 'ping', null, 'ping', 
                function(response) {
                    if(response.error == "show_screen_saver" && !dderlState.screensaver) {
                        console.log("showing screen saver");
                        dderlState.screensaver = true;
                        showScreeSaver();
                    }
                },
                function(error) {
                    console.log("Error on ping : ", error);
                    alert_jq("Failed to reach the server, the connection might be lost.");
                    clearTimeout(dderlState.pingTimer);
                }
            );
        },
    30000); // Ping time 30 secs.
}

function login_first()
{
    alert_jq("Please log in first!");
}

export function show_qry_files(useSystem) {
    var loggedInUser = $('#btn-change-password').data("logged_in_user");
    if(!loggedInUser || loggedInUser.length === 0) {
        login_first();
        return;
    }
    $('<div>')
    .appendTo(document.body)
    .table({
        autoOpen     : false,
        dderlConn    : dderlState.connection,
        dderlAdapter : dderlState.adapter,
        title        : "All ddViews"
    })
    .table('loadViews', useSystem);
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

function get_local_apps(table) {
    ajaxCall(null, 'about', null, 'about', function(applications) {
        applications.jQuery = {version : $.fn.jquery, dependency : true};
        applications.jQueryUI = {version : $.ui.version, dependency : true};
        applications.SlickGrid = {version : (new Slick.Grid($('<div>'), [], [], [])).slickGridVersion, dependency : true};
        var apps = '';
        for(let app in applications) {
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
    ajaxCall(null, 'remote_apps', {remote_apps : {connection: dderlState.connection}}, 'remote_apps', function(applications) {
        var extra_apps = '';
        for(let app in applications) {
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

export function show_about_dlg() {
    ajaxCall(null, 'about', null, 'about', function(applications) {
        applications.jQuery = {version : $.fn.jquery, dependency : true};
        applications.jQueryUI = {version : $.ui.version, dependency : true};
        applications.SlickGrid = {version : (new Slick.Grid($('<div>'), [], [], [])).slickGridVersion, dependency : true};

        var aboutDlg = $('<div id="about-dderl-dlg"></div>');

        if(dderlState.connection) {
            aboutDlg.append('<div class="remote-apps"><a id="remote-apps-link" title="Show all remote apps" href="#">show remote</a></div>');
        }

        var table = '<table class="about-deps-table" cellspacing="5" border="0">';
        var extra_apps = '';
        for(let app in applications) {
            var version = applications[app].version;
            if(app === "dderl") {
                var description = applications[app].description;
                var p = '<p class="about-title">DDerl</p>';
                p += '<p class="about-vsn">' + version + ' GUI 2.3.1</p>';
                p += '<p class="about-desc">' + description + '</p>';
                p += '<hr>';
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

        let moreAppsLink = $('<a id="more-apps-link" title="Show all running apps" href="#">+</a>');
        moreAppsLink.click(() => show_more_apps());
        var divMore = $('<div class="about-more"></div>');
        aboutDlg.append(divMore.append(moreAppsLink));

        aboutDlg.dialog({
            modal: false,
            width: 230,
            resizable: false,
            title: "About",
            appendTo: "#main-body",
            position: {my: "center top", at: "center top+75", of: "#main-body", collision : 'none'},
            close: function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        }).dialog("widget").draggable("option","containment","#main-body");
        show_more_apps();
    });
}

let beepTimeout;
export function beep() {
    let notificationEl = document.getElementById('beep-notification');
    notificationEl.style.display = 'inline-block';
    if(beepTimeout) { clearTimeout(beepTimeout); }
    beepTimeout = setTimeout(() => {
        notificationEl.style.display = 'none';
    }, 1000);
}

$(".grid-header .g-ui-icon").addClass("ui-state-default ui-corner-all");

// In some environment, console is defined but console.log or console.error is missing.
if (window.console && window.console.log && window.console.error) {
    console.log('console log is defined');
} else {
    window.console = {log: function(){ }, error: function(){ }};
    console.log('dummy console is created');
}

export function change_password(shouldConnect) {
    var loggedInUser;
    if(dderlState.connected_user && dderlState.connection) {
        loggedInUser = dderlState.connected_user;
        change_connect_password(loggedInUser);
    } else {
        loggedInUser = $('#btn-change-password').data("logged_in_user");
        if(!loggedInUser || loggedInUser.length === 0) {
            login_first();
            return;
        }
        change_login_password(loggedInUser, shouldConnect);
    }
}

export function smartDialogPosition(container, owner, self, checks) {
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
        case 'center':
            haveSpace = false; // Only used as default.
            newPos = {at: 'center top+60', my: 'center top', of: ownerDlg};
        }

        //The last check is the default pos.
        if((i === checks.length - 1) || haveSpace) {
            self.dialog("option", "position", newPos);
            break;
        }
    }
}

// TODO: Fix the automatic positioning of windows
/*
function findFreeSpace(self) {
    var currentDlgs = $(".ui-dialog-content");
    var dialogPositions = [];
    for(let i = 0; i < currentDlgs.length; ++i) {
        if($(currentDlgs[i]).dialog('isOpen')) {
            var dlg = $(currentDlgs[i]).dialog("widget");
            var box = {top   : dlg.position().top,
                       left  : dlg.position().left,
                       bottom: dlg.position().top + dlg.height(),
                       right : dlg.position().left + dlg.width()};
            dialogPositions.push(box);
        }
    }
    dialogPositions.sort(function(b1, b2) { return b1.left - b2.left; });
    //TODO: Naive implementation, we improve it if it works...
    for(let i = 0; i < $("#main-body").width(); i += 10) {
        for(let j = 0; j < $("#main-body").height(); j += 10) {
        }
    }
    console.log(self.dialog("widget").width() + ", " + self.dialog("widget").height());
    //console.log(dialogPositions);
}
*/

// TODO: move this function to a separate tablelist module
export function addWindowFinder(table, title) {
    // Create the elements.
    var windowsList = document.getElementById("window-finder");
    var link = document.createElement("a");
    var li = document.createElement("li");

    // Set the title and the click event.
    link.textContent = title;
    link.onclick = function() {
        if(table && table._dlg && table._dlg.hasClass('ui-dialog-content') && table._dlg.dialog("isOpen") === true) {
            table.moveAllToTop();
        } else {
            // In case we have a invalid entry it is removed.
            windowsList.removeChild(li);
        }
    };

    // Set the size of the dropdown if the size is bigger than the current one
    // 9 px for each character, with a minimun of 100px
    var titleSize = title.length * 9;
    if(titleSize < 100) {
        titleSize = 100;
    }

    if(windowsList.style.width) {
        var currentRowSize = parseInt(windowsList.style.width, 10);
        if(currentRowSize < titleSize) {
            windowsList.style.width = titleSize + "px";
        }
    } else {
        windowsList.style.width = titleSize + "px";
    }

    // Append to the page.
    li.appendChild(link);
    windowsList.appendChild(li);

    // Bind to the close event to remove it from the list.
    table._dlg.bind("dialogclose", function() {
        windowsList.removeChild(li);
        // Set the size to the biggest element
        let biggestChild = 100;
        for(let i = 0; i < windowsList.children.length; ++i) {
            let sizeChild = windowsList.children[i].textContent.length * 9;
            if(sizeChild > biggestChild) {
                biggestChild = sizeChild;
            }
        }
        windowsList.style.width = biggestChild + "px";
    });

    table._dlg.bind("dialogclose", function() {
        var tablePos = dderlState.currentWindows.indexOf(table);
        if(tablePos != -1) {
            dderlState.currentWindows.splice(tablePos, 1);
        }
    });
    // Add it to the global windows array
    dderlState.currentWindows.push(table);
    return link;
}

export function updateWindowTitle(link, title) {
    var windowsList = document.getElementById("window-finder");
    link.textContent = title;

    // Set the size of the dropdown if the size is bigger than the current one
    // 9 px for each character, with a minimun of 100px
    var titleSize = title.length * 9;
    if(titleSize < 100) {
        titleSize = 100;
    }
    if(windowsList.style.width) {
        var currentRowSize = parseInt(windowsList.style.width, 10);
        if(currentRowSize < titleSize) {
            windowsList.style.width = titleSize + "px";
        }
    } else {
        windowsList.style.width = titleSize + "px";
    }
}

export function password_change_dlg(title, loggedInUser, change_pass_fn) {
    var change_password_dlg = 
      '<div id="dialog-change-password" title="'+title+'">' +
      '  <table border=0 width=100% height=85% cellpadding=0 cellspacing=0>';
    if (loggedInUser) {
        change_password_dlg += '<tr><td align=right valign=center>User&nbsp;</td>'+
                                    '<td valign=center><b>'+loggedInUser+'</b></td></tr>';
    }
      change_password_dlg +=
      '      <tr><td align=right valign=center>Old Password&nbsp;</td>'+
      '         <td valign=bottom>' +
      '             <input type="password" id="old_password_login" class="text ui-widget-content ui-corner-all"/>' +
      '         </td></tr>' +
      '      <tr><td align=right valign=center>New Password&nbsp;</td>'+
      '         <td valign=bottom>' +
      '             <input type="password" id="password_change_login" class="text ui-widget-content ui-corner-all"/>' +
      '         </td></tr>' +
      '      <tr><td></td>'+
      '          <td><span id="passstrength"></span></td></tr>' +
      '      <tr><td align=right valign=center>Confirm Password&nbsp;</td>' +
      '         <td valign=bottom>' +
      '             <input type="password" id="conf_password_login" class="text ui-widget-content ui-corner-all"/>' +
      '         </td></tr>' +
      '  </table>' +
      '</div>';
    $(change_password_dlg).appendTo(document.body);

    $('#password_change_login').keyup(function() {
        $('#passstrength').removeClass().html('');
        clearTimeout($('#password_change_login').data("checkto"));
        $('#password_change_login').data("checkto",
            setTimeout(function() {
                ajaxCall(null, 'password_strength',
                    {password:$('#password_change_login').val()}, 'password_strength',
                    function(result) {
                        if (result == "short") {
                            $('#passstrength').removeClass().addClass('password_strength_more')
                                .html('More Characters');
                            $("#dialog-change-password").parent()
                                .find("button:contains('Change Password')").button("disable");
                        } else if (result == "strong") {
                            $('#passstrength').removeClass().addClass('password_strength_ok')
                              .html('Strong');
                            $("#dialog-change-password").parent()
                                .find("button:contains('Change Password')").button("enable");
                        } else if (result == "medium") {
                            $('#passstrength').removeClass().addClass('password_strength_alert')
                                .html('Medium');
                            $("#dialog-change-password").parent()
                                .find("button:contains('Change Password')").button("disable");
                        } else {
                            $('#passstrength').removeClass().addClass('password_strength_error')
                                .html('Weak');
                            $("#dialog-change-password").parent()
                                .find("button:contains('Change Password')").button("disable");
                        }
                    });
            }, 500));
        return true;
    });
    $('#dialog-change-password').dialog({
        autoOpen: false,
        height: 200,
        width: 300,
        resizable: false,
        modal: false,
        appendTo: "#main-body",
        close: function() {
            $("#dialog-change-password").dialog('destroy');
            $("#dialog-change-password").remove();
        },
        buttons: {
            "Change Password": change_pass_fn,
            Cancel: function() {
                $(this).dialog("close");
            }
        }
    })
    .dialog("open")
    .dialog("widget")
    .draggable("option","containment","#main-body");

    $("#dialog-change-password").parent()
        .find("button:contains('Change Password')").button("disable");
}

export function unescapeNewLines(str) {
    try {
        JSON.parse(str);
    } catch (e) {
        if(/[\n\t\r]/.test(str) === true) {
            str = str.replace(/^"|"$/g, '').replace(/""/g, '"');
        }
    }
    return unescape(str);
}

export function escapeNewLines(str) {
    var result = str;
    if(typeof str == 'string' || str instanceof String) {
        if(/[\n\t\r]/.test(str) === true) {
            result = "\""+str.replace(/"/g,"\"\"")+"\"";
        }
    }
    return result;
}
