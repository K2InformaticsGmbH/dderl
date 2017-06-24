import {alert_jq} from '../dialogs/dialogs';
import {ajaxCall, dderlState} from '../scripts/dderl';

export function newStmt(table, callback, mode, closeFuns) {
    var viewId = null;
    if(table.hasOwnProperty('view_id')) {
        viewId = table.view_id;
    }

    if(table.hasOwnProperty('error')) {
        alert_jq(table.error);
        return;
    } else if(table.hasOwnProperty('binds')) {
        alert_jq("Not all the variables bound to open background statement");
        return;
    }

    // TODO: Should we allow the graph definition to change start ?
    var startBtn = modeToBtn(mode);

    var binds = null;
    if(table.qparams && table.qparams.hasOwnProperty('pars')) {
        binds = table.qparams.pars;
    }
    var stmt = table.statement;

    // Helper functions to control the statement
    function closeStmt() {
        console.log("close requested");
        if(closeFuns.indexOf(this) !== -1) {
            console.log("Found this....");
        }
        if(closeFuns.indexOf(closeStmt) !== -1) {
            console.log("Found the name ...");
        }
        cmd('close', stmt, binds, cmdResult);
    }

    function nextRows() {
        console.log("more rows requested");
        cmd('>', stmt, binds, cmdResult);
    }

    // process cmd result
    function cmdResult(result) {
        if(result.hasOwnProperty('op') && result.op === "close") {
            console.log("Hidden stmt closed");
            stmt = null;
        } else if(result.hasOwnProperty('rows')) {
            if(result.message.length > 0) {
                alert_jq(result.message);
            }
            if(result.loop.length > 0 && isLoopMode(mode)) {
                callback(result.rows, closeStmt);
                cmd(result.loop, stmt, binds, cmdResult);
            } else {
                if(result.state === "completed") {
                    callback(result.rows, closeStmt, null, true);
                } else {
                    callback(result.rows, closeStmt, nextRows, false);
                }
            }
        } else if(result.hasOwnProperty('error')) {
            alert_jq(result.error);
        }
    }

    // Get the rows
    cmd(startBtn, stmt, binds, cmdResult);

    closeFuns.push(closeStmt);
}

// loading rows
function cmd(cmd, stmt, binds, rowsCb) {
    if(!stmt) { return; }

    ajaxCall(null, 'button', {
        button: {
            connection: dderlState.connection,
            statement: stmt,
            binds: binds,
            btn: cmd
        }
    }, 'button', rowsCb);
}

function modeToBtn(mode) {
    if(mode === 'pt' || mode === 'passthrough') {
        return 'pt';
    } else if (mode === 'tail' || mode === '...') {
        return '...';
    } else if (mode === '>|...') {
        return mode;
    } else {
        return '>';
    }
}

function isLoopMode(mode) {
    return (mode === 'pt' || mode === 'passthrough' ||
        mode === 'tail' || mode === '...' ||
        mode === '>|...');
}
