import $ from 'jquery';

// TODO: Revisit to reduce the duplication
export function alert_jq(string) {
    var dlgDiv =
        $('<div>')
        .appendTo(document.body)
        .append('<p class="selectable-alert-text"><span class="ui-icon ui-icon-info" style="float: left; margin: 0 7px 50px 0;"></span>'+string+'</p>')
        .dialog({
            modal:false,
            width: 300,
            height: 300,
            title: "DDerl message",
            appendTo: "#main-body",
            close: function() {
                //We have to remove the added child
                dlgDiv.dialog('destroy');
                dlgDiv.remove();
                dlgDiv.empty();
            }
        });
    dlgDiv.dialog("widget").draggable("option","containment","#main-body");
    return dlgDiv;
}

export function confirm_jq(dom, callback) {
    var content = dom.content;

    if ($.isArray(content)) {
        content = content.join('<br>');
    }
    content = '<h1 style="color:red">CAUTION : IRREVERSIBLE ACTION</h1>'+
              '<p style="background-color:black;color:yellow;font-weight:bold;text-align:center;">'+
              'If confirmed can NOT be undone</p>'+
              (content.length > 0 ?
                '<div style="position:absolute;top:65px;bottom:5px;overflow-y:scroll;left:5px;right:5px;">'+
                 content+'</div>'
               : '');
    
    var dlgDiv =
        $('<div>')
        .append(content)
        .dialog({
            modal:false,
            width: 300,
            height: 300,
            title: dom.title,
            appendTo: "#main-body",
            close: function() {
                //We have to remove the added child
                dlgDiv.dialog('destroy');
                dlgDiv.remove();
                dlgDiv.empty();
            },
            buttons: {
                'Ok': function() {
                    $(this).dialog("close");
                    callback();
                },
                'Cancel': function() {
                    $(this).dialog("close");
                }
            }
        });
    dlgDiv.dialog("widget").draggable("option","containment","#main-body");
    return dlgDiv;
}

export function prompt_jq(dom, callback) {
    var content = dom.content;
    var value = '';
    if (dom.value) {
        value = dom.value;
    } 
    if ($.isArray(content))
        content = content.join('<br>');
        content = '<form id="prompt_form"><fieldset>' +
                  '<label for="prompt_jq_input">' + dom.label + ':</label>' +
                  '<input type="text" id="prompt_jq_input" name="prompt_jq_input" class="text ui-widget-content ui-corner-all" value="'+ value +'" autofocus/>' +
                  (content.length > 0 ?
                    '<div style="position:absolute;top:65px;bottom:5px;overflow-y:scroll;left:5px;right:5px;">' +
                     content + '</div>'
                   : '') +
                   '</fieldset></form>';

    var execute_callback = function(dlg) {
        var inputValue = $("#prompt_jq_input").val();
        if (inputValue) {
            dlg.dialog("close");
            callback(inputValue);
        }
    };

    var dlgDiv =
        $('<div>')
        .append(content)
        .dialog({
            modal:false,
            width: 300,
            height: 300,
            title: "DDerl parameter input",
            appendTo: "#main-body",
            close: function() {
                //We have to remove the added child
                dlgDiv.dialog('destroy');
                dlgDiv.remove();
                dlgDiv.empty();
            },
            buttons: {
                'Ok': function() {
                    execute_callback($(this));
                },
                'Cancel': function() {
                    $(this).dialog("close");
                }
            }
        });

    $('#prompt_jq_input').keypress(function(event) {
        if(event.which == 13) {
            event.preventDefault();
            event.stopPropagation();
            execute_callback(dlgDiv);
        }
    });
    dlgDiv.dialog("widget").draggable("option","containment","#main-body");
    return dlgDiv;
}

export function alert_js_error(e) {
    var message = e.message;
    if(e.stack) {
        message += "\n" + e.stack;
    }
    alert_jq(message);
}

export function dlg_fit_to_window(dlg) {
    var widget = dlg.dialog('widget');
    var dWidth = widget.width();
    let dHeight = widget.height();

    // available width for the window
    var rWindowWidth = $(window).width() - widget.position().left - 25;
    console.log("window space to the right", rWindowWidth);

    // dialog is bigger than the remaining window
    if (dWidth > rWindowWidth) {
        var orig_top = widget.position().top;
        var new_left = widget.position().left - dWidth + rWindowWidth;
        if(new_left > 0) {
            console.log("Dialog outside right edge, moving it to the left, target:", new_left);
            dlg.dialog("option", "position", {
                my: "left top",
                at: "left+" + new_left + " top+" + orig_top,
                of: "#main-body",
                collision : 'none'
            });
        } else {
            console.log("Dialog doesn't fit in the window reducing its width");
            dlg.dialog("option", "position", {
                my: "left top",
                at: "left+3 top+" + orig_top,
                of: "#main-body",
                collision : 'none'
            });
            dlg.dialog("option", "width", $(window).width() - 25);
            // We have to account for the newly created scrollbar...
            dlg.dialog("option", "height", dHeight + 15);
        }
    }

    // available height for the window
    let rWindowHeight = $(window).height() - widget.position().top - 60;
    console.log("the window height available space", rWindowHeight);
    if (dHeight > rWindowHeight) {
        var orig_left = widget.position().left;
        var new_top = widget.position().top - dHeight + rWindowHeight;
        if(new_top > 0) {
            console.log("Dialog outside bottom, moving it up, target:", new_top);
            dlg.dialog("option", "position", {
                my: "left top",
                at: "left+" + orig_left + " top+" + new_top,
                of: "#main-body",
                collision: 'none'
            });
        } else {
            console.log("Dialog doesn't fit in the window reducing its height");
            dlg.dialog("option", "position", {
                my: "left top",
                at: "left+" + orig_left + " top+5",
                of: "#main-body",
                collision: 'none'
            });
            dlg.dialog("option", "height", $(window).height() - 60);
            // We have to account for the newly created scrollbar...
            dlg.dialog("option", "width", dWidth + 15);
        }
    }
}
