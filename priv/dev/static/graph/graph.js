import * as d3 from 'd3/build/d3.node';
import $ from 'jquery';
import {alert_js_error, alert_jq, dlg_fit_to_window} from '../dialogs/dialogs';
import {dderlState, ajaxCall} from '../scripts/dderl';
import {renderNewTable} from '../scripts/dderl.table';
import {newStmt} from './hiddenStmt';

export function evalD3Script(script, statement, tableStmtReload, tableLoopBlock) {
    /* jshint evil:true */
    // Here we can inject libraries we would like to make available to d3 scripts.
    var f = new Function('script', 'd3', 'helper', "return eval('(' + script + ')')");
    var result = null;
    var closeFuns = [];
    var helper = {
        browse: openGraphView,
        runView: function(name, callback, binds = {}, mode = "normal") {
            openHiddenStmt(name, callback, binds, mode, closeFuns);
        },
        req: buildReq(statement, tableStmtReload, tableLoopBlock),
        contextMenu: openContextMenu,
        openDialog: openDialog,
        parseInt: ddParseInt,
        parseFloat: ddParseFloat,
        tParseEu: ddParseTimeEu(),
        tParseEuL: ddParseTimeEuL(),
        tParseInt: ddParseTimeInt(),
        tParseIntL: ddParseTimeIntL(),
        parseTime: ddParseTime
    };
    try {
        var init = f(script, d3, helper);
        result = function(container, width, height) {
            var d3obj = init(container, width, height);
            var onCloseCb = d3obj.on_close;
            d3obj.on_close = function() {
                console.log("on_close called... funs count", closeFuns.length);
                closeFuns.forEach(function(closeStmt) {
                    closeStmt();
                });
                if ($.isFunction(onCloseCb)) {
                    onCloseCb();
                }
            };
            return d3obj;
        };
    } catch(e) {
        alert_js_error(e);
    }
    return result;
}

function openGraphView(name, binds = {}, position = {top: 0, left: 0}, force = false, cb = null) {
    var openViewData = {
        open_graph_view: {
            connection: dderlState.connection,
            view_name: name,
            conn_id: dderlState.connectionSelected.connection,
            binds: binds
        }
    };
    ajaxCall(null, 'open_graph_view', openViewData, 'open_graph_view', function(viewResult) {
        if(viewResult.bind_types) {
            viewResult.qparams = {
                types: viewResult.bind_types,
                pars: binds
            };
        }
        var viewRef = renderNewTable(viewResult, position, force);
        if(cb) {
            cb(viewRef);
        }
    });
}

function openHiddenStmt(name, callback, binds, mode, closeFuns) {
    var openViewData = {
        open_graph_view: {
            connection: dderlState.connection,
            view_name: name,
            conn_id: dderlState.connectionSelected.connection,
            binds: binds
        }
    };
    ajaxCall(null, 'open_graph_view', openViewData, 'open_graph_view', function(viewResult) {
        // This maybe is not required, but pass it on just in case.
        if(viewResult.bind_types) {
            viewResult.qparams = {
                types: viewResult.bind_types,
                pars: binds
            };
        }
        newStmt(viewResult, callback, mode, closeFuns);
    });
}

function buildReq(statement, tableStmtReload, tableLoopBlock) {
    function req(viewName, suffix, topic, key, binds, graphFocusCb) {
        var update_stmt_data = {
            view_name: viewName,
            suffix: suffix,
            key: key,
            statement: statement,
            binds: binds,
            connection: dderlState.connection,
            conn_id: dderlState.connectionSelected.connection
        };
        console.log("Blocking requests before replacing the fsm");
        tableLoopBlock();
        ajaxCall(null, 'update_focus_stmt', update_stmt_data, 'update_focus_stmt', function(result) {
            console.log("Result subscription", result);
            if (!result) {
                alert_jq("Error response on subscription");
            } else if(result.hasOwnProperty('error')) {
                alert_jq(result.error);
            } else {
                statement = result.statement;
                graphFocusCb();
                if(!key) {
                    // We should not register with empty keys.
                    tableStmtReload(result);
                    return;
                }

                var subscribe_data = {
                    statement: statement,
                    key: key,
                    topic: topic
                };
                //TODO: Should graph_subscribe trigger a reply_stack on fsm ??...
                setTimeout(function() {
                    ajaxCall(null, 'graph_subscribe', subscribe_data, 'graph_subscribe', function (subsResult) {
                        if (!subsResult) {
                            alert_jq("Error response on subscription");
                        } else if(subsResult.message == "error") {
                            alert_jq("Error on subscription");
                        } else if(subsResult.message != "ok") {
                            alert_jq("Error on subscription: " + subsResult.message);
                        } else {
                            tableStmtReload(result);
                        }
                    });
                }, 100);
            }
        });
    }

    return req;
}

function ddParseInt(string, radix = 10) {
    return parseInt(string.split("'").join(""), radix);
}

function ddParseFloat(string) {
    return parseFloat(string.split("'").join(""));
}

function ddParseTimeEu() {
    // timeParse without msec
    return d3.utcParse("%d.%m.%Y %H:%M:%S");
}

function ddParseTimeEuL() {
    // timeParse with msec
    return d3.utcParse("%d.%m.%Y %H:%M:%S.%L");
}

function ddParseTimeInt() {
    // timeParse international format without msec
    return d3.utcParse("%Y-%m-%d %H:%M:%S");
}

function ddParseTimeIntL() {
    // timeParse international format
    return d3.utcParse("%Y-%m-%d %H:%M:%S.%L");
}

var tParseEu = ddParseTimeEu();
var tParseEuL = ddParseTimeEuL();
var tParseInt = ddParseTimeInt();
var tParseIntL = ddParseTimeIntL();

function ddParseTime(tStr) {
    var s = tStr.substr(0,23);
    if (!s.includes("-")) {
        if (s.length === 23) {
            return tParseEuL(s);
        } else {
            return tParseEu(s);
        }
    } else {
        if (s.length === 23) {
            return tParseIntL(s);
        } else {
            return tParseInt(s);
        }
    }
}

/**
 * Creates a context menu based on the position and array of entries provided:
 * 
 * openContextMenu({x: 10, y: 20}, [
 *     {label: "firefox", icon: "firefox", cb: function() { alert("browser!"); }},
 *     {label: "chrome", icon: "chrome", cb: function() { alert("browser!"); }},
 *     {label: "bitcoin", icon: "btc", cb: function() { alert("crypto!"); }}
 * ]);
 * 
 */

function openContextMenu(entriesList, {x, y}) {
    var body = document.body;
    var menu = document.createElement('ul');
    menu.className = 'context_menu';
    entriesList.forEach(function({label, icon, cb: callback}) {
        var li = document.createElement('li');
        if(icon) {
            var i = document.createElement("i");
            i.className = "menu-icon fa-fw fa-lg fa fa-" + icon;
            li.appendChild(i);
        }
        li.appendChild(document.createTextNode(label));
        li.onclick = function(evt) {
            body.removeChild(menu);
            callback(evt);
        };
        menu.appendChild(li);
    });

    menu.onmouseleave = function() {
        body.removeChild(menu);
    };

    menu.style.left = x + 'px';
    menu.style.top = y + 'px';

    body.appendChild(menu);
}

function openDialog(title, content, {x, y}) {
    var dlg = $('<div class="selectable-alert-text graph-details-text">');

    dlg.html(content);

    dlg.dialog({
        title: title,
        autoOpen: false,
        minHeight: 200,
        height: 'auto',
        width: 'auto',
        position: {my: 'left top', at: 'left top', of: '#main-body', collision: 'none'},
        appendTo: '#main-body',
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        }
    });

    dlg.dialog("open")
        .dialog("widget")
        .draggable("option", "containment", "#main-body");

    // Move the dialog after it has been open, as position doesn't work
    // properly otherwise since requires to calculate the offset.
    var at = 'left+' + x + ' top+' + y;
    dlg.dialog("option", "position", {
        my: 'left top',
        at: at,
        of: '#main-body',
        collision: 'none'
    });

    dlg_fit_to_window(dlg);
}
