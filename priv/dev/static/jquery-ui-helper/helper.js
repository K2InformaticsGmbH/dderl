import $ from 'jquery';

export function patch_jquery_ui() {
    // Since version 1.10 of jquery do not support html on title's dialog
    // http://stackoverflow.com/questions/14488774/using-html-in-a-dialogs-title-in-jquery-ui-1-10
    // https://github.com/jquery/jquery-ui/commit/7e9060c109b928769a664dbcc2c17bd21231b6f3
    $.widget("ui.dialog", $.extend({}, $.ui.dialog.prototype, {
        _title: function (title) {
            title.html(this.options.title || "&#160;");
        }
    }));
}

export function controlgroup_options() {
    return {
        items: {
            "button": "input[type='text'], input[type='button'], input[type='submit'], input[type='reset'], button, a",
            "controlgroupLabel": ".ui-controlgroup-label",
            "checkboxradio": "input[type='checkbox'], input[type='radio']",
            "selectmenu": "select",
            "spinner": ".ui-spinner-input"
        }
    };
}
