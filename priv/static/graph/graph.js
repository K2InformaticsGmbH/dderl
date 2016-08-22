import * as d3 from 'd3/build/d3.node';
import {alert_js_error} from '../dialogs/dialogs';
import {dderlState, ajaxCall} from '../scripts/dderl';
import {renderNewTable} from '../scripts/dderl.table';

export function evalD3Script(script) {
    /* jshint evil:true */
    // Here we can inject libraries we would like to make available to d3 scripts.
    var f = new Function('script', 'd3', 'helper', "return eval('(' + script + ')')");
    var result = null;
    var helper = {
        browse: openGraphView
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
