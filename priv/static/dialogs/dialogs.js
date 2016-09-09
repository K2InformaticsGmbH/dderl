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
