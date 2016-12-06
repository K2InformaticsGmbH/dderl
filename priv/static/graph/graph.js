import * as d3 from 'd3/build/d3.node';
import {alert_js_error, alert_jq} from '../dialogs/dialogs';
import {dderlState, ajaxCall} from '../scripts/dderl';
import {renderNewTable} from '../scripts/dderl.table';

export function evalD3Script(script, statement) {
    /* jshint evil:true */
    // Here we can inject libraries we would like to make available to d3 scripts.
    var f = new Function('script', 'd3', 'helper', "return eval('(' + script + ')')");
    var result = null;
    var helper = {
        browse: openGraphView,
        req: buildReq(statement)
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
        renderNewTable(viewResult, position, force);
    });
}

function buildReq(statement) {
    function req(viewName, suffix, topic, key, binds) {
        var data = {
            view_name: viewName,
            suffix: suffix,
            topic: topic,
            key: key,
            statement: statement,
            binds: binds,
            connection: dderlState.connection,
            conn_id: dderlState.connectionSelected.connection
        };
        ajaxCall(null, 'graph_subscribe', data, 'graph_subscribe', function(result){
            console.log("Result subscription", result);
            if (!result) {
                alert_jq("Error response on subscription");
            } else if(result.message == "error") {
                alert_jq("Error on subscription");
            } else if(result.message != "ok") {
                alert_jq("Error on subscription: " + result.message);
            }
        });
    }

    return req;
}
