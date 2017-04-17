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
import {StartSqlEditor} from "./scripts/dderl.sql";

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


function patch_jquery_ui() {
    // Since version 1.10 of jquery do not support html on title's dialog
    // http://stackoverflow.com/questions/14488774/using-html-in-a-dialogs-title-in-jquery-ui-1-10
    // https://github.com/jquery/jquery-ui/commit/7e9060c109b928769a664dbcc2c17bd21231b6f3
    $.widget("ui.dialog", $.extend({}, $.ui.dialog.prototype, {
        _title: function (title) {
            title.html(this.options.title || "&#160;");
        }
    }));
}

$(document).ready(function () {
    $('#main-body').css('top', $('#main-menu-bar').height());
    if (Object.hasOwnProperty('freeze')) {
        patch_jquery_ui(); // Add support for html titles on dialogs.
        loginAjax();
    } else {
        $('#main-menu-bar').hide();
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
addClick('newsql-button',       () => { StartSqlEditor();       });
addClick('btn-logout',          () => { logout();               });
