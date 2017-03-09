import * as d3 from 'd3/build/d3.node';
import $ from 'jquery';
import {alert_js_error, alert_jq, dlg_fit_to_window} from '../dialogs/dialogs';
import {dderlState, ajaxCall} from '../scripts/dderl';
import {renderNewTable} from '../scripts/dderl.table';

export function evalD3Script(script, statement, tableStmtReload) {
    /* jshint evil:true */
    // Here we can inject libraries we would like to make available to d3 scripts.
    var f = new Function('script', 'd3', 'helper', "return eval('(' + script + ')')");
    var result = null;
    var helper = {
        browse: openGraphView,
        req: buildReq(statement, tableStmtReload),
        contextMenu: openContextMenu,
        openDialog: openDialog,
        parseInt: ddParseInt,
        parseFloat: ddParseFloat
    };
    try {
        result = f(script, d3, helper);
    } catch(e) {
        alert_js_error(e);
    }
    return result;
}

function openGraphView(name, binds = {}, position = {top: 0, left: 0}, force = false) {
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
        renderNewTable(viewResult, position, force);
    });
}

function buildReq(statement, tableStmtReload) {
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
    var dlg = $('<div class="selectable-alert-text">');

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
