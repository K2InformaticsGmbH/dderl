import $ from "jquery";

import 'imports-loader?$=jquery,$.uiBackCompat=>false!jquery-ui/ui/widgets/dialog';
import 'imports-loader?$=jquery,$.uiBackCompat=>false!jquery-ui/ui/widgets/progressbar';
import 'imports-loader?$=jquery,$.uiBackCompat=>false!jquery-ui/ui/widgets/sortable';

import {loginAjax} from "./scripts/login";
import {alert_jq} from './dialogs/dialogs';
import {dderlState, show_qry_files,
        change_password, show_about_dlg} from "./scripts/dderl";
import {new_connection_tab, logout, restart} from "./scripts/login";
import {disconnect_tab, close_tab} from "./scripts/connect";
import {newSqlEditor} from "./scripts/dderl.sql";
import {patch_jquery_ui} from "./jquery-ui-helper/helper.js";
import * as tableSelection from './scripts/table-selection';

import 'font-awesome/css/font-awesome.css';
// Add our jquery-ui theme (smoothness) from http://jqueryui.com/download/
import './styles/jquery-ui-smoothness/jquery-ui.css';

import './styles/slick.grid.css';
import './styles/slick.columnpicker.css';
import './styles/dropdown.css';
import './styles/dderl.sql.css';
import './styles/dderl.connect.css';
import './styles/dderl.css';

var req = require.context("./slickgrid", true, /\.js$/);
req.keys().forEach(function(key){
    req(key);
});

window.MonacoEnvironment = {
    getWorkerUrl: function(moduleId, label) {
        if(label === 'json') {
            return 'public/dist/json.worker.js';
        }
        if(label === 'css') {
            return 'public/dist/css.worker.js';
        }
        if(label === 'html') {
            return 'public/dist/html.worker.js';
        }
        if(label === 'typescript' || label === 'javascript') {
            return 'public/dist/ts.worker.js';
        }
        return 'public/dist/editor.worker.js';
    }
};

$(document).ready(function () {
    let mainBody = $('#main-body');
    let menuBar = $('#main-menu-bar');

    mainBody.css('top', menuBar.height());
    if (Object.hasOwnProperty('freeze')) {
        // Add support for html titles on dialogs.
        patch_jquery_ui();
        loginAjax();
    } else {
        menuBar.hide();
        alert_jq("We are really sorry, but we don't support your current browser version.");
    }

    $(window).on('beforeunload', function () {
        if (dderlState.connection) {
            return "You will lose all unsaved data, are you sure you want to continue?";
        }
    });

    $(window).on('unload', function() {
        close_tab();
    });

    mainBody.click(function(e) {
        if(e.target === this) {
            tableSelection.select(null);
        }
    });

});

// Set up main-menu handlers from index.html
function addClick(id, handler) {
    document.getElementById(id).onclick = handler;
}

addClick('btn-tables',          () => { show_qry_files(true);   });
addClick('btn-change-password', () => { change_password(false); });
addClick('btn-about',           () => { show_about_dlg();       });
addClick('btn-disconnect',      () => { disconnect_tab();       });
addClick('btn-restart',         () => { restart();              });
addClick('connect-button',      () => { new_connection_tab();   });
addClick('newsql-button',       () => { newSqlEditor();         });
addClick('btn-logout',          () => { logout();               });
