import $ from "jquery";
import "jquery-ui";

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
        patch_jquery_ui(); // Add support for html titles to dialogs.
        check_already_connected();
    } else {
        $('#main-menu-bar').hide();
        alert_jq("We are really sorry, but we don't support your current browser version.");
    }

    $(window).on('beforeunload', function () {
        if (dderlState.connection) {
            return "You will lose all unsaved data, are you sure you want to continue?";
        }
    });
    //beep(); // beep test
});