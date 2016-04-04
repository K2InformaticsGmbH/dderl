function update_user_information(user) {
    $('#btn-change-password').data("logged_in_user", user);
    $('#login-button').html('Log out ' + user);
}

function check_already_connected() {
    if(!window.opener || !window.opener.dderlState || !window.opener.dderlState.session ||
       !window.opener.$('#btn-change-password').data("logged_in_user")) {
        loginAjax(null);
    } else {
        dderlState.session = window.opener.dderlState.session;
        dderlState.connectionSelected = window.opener.dderlState.connectionSelected;
        var user = window.opener.$('#btn-change-password').data("logged_in_user");
        update_user_information(user);
        connect_dlg();
    }
}

function loginAjax(data) {
    ajaxCall(null, 'login', (data == null ? {} : data),
            'login', loginCb);
}

var app = "";
function loginCb(resp) {
    $('#btn-disconnect').removeClass('disabled');

    if (resp.hasOwnProperty('vsn')) {
        $('#version').text(' | '+resp.vsn);
    }
    if (resp.hasOwnProperty('node')) {
        $('#node').text(resp.node);
    }
    if (resp.hasOwnProperty('app')) {
        app = resp.app;
        document.title = 'DDErl - '+app;
    }

    if (resp.hasOwnProperty('error')) {
        var accountName = "";
        if(resp.hasOwnProperty('pwdmd5')) {
            accountName = resp.pwdmd5.accountName;
        }
        display({title  : "Login",
                  fields :[{type       : "text",
                            placeholder: "User",
                            val        : accountName},
                           {type       : "password",
                            placeholder: "Password",
                            val        : ""},
                           {type       : "label",
                            val        : resp.error,
                            color      : "#DD1122"}] //Swisscom red color
        });
    } else if(resp.hasOwnProperty('pwdmd5')) {
        display({title  : "Login",
                 fields : [{type        : "text",
                            placeholder : "User",
                            val         : resp.pwdmd5.accountName},
                           {type        : "password",
                            placeholder : "Password",
                            val         : ""}]
        });
    } else if(resp.hasOwnProperty('smsott')) {
        display({title  : "Enter Token",
                 fields : [{type        : "label",
                            val         : "A token is send through SMS to "+resp.smsott.to+
                                          " for user "+resp.smsott.accountName+
                                          ". Please enter the token below"},
                           {type        : "text",
                            placeholder : "SMS Token",
                            val         : ""}]
        });
    } else if (resp.hasOwnProperty('accountName')) {
        update_user_information(resp.accountName);
        resetPingTimer();
        connect_dlg();
    } else if (resp.hasOwnProperty('changePass')) {
        change_login_password(resp.changePass, true);
    } else {
        alert_jq("Unexpected "+JSON.stringify(resp));
    }
}

function display(layout) {
    var dlg = $('<div title="'+layout.title+'" style="display:none">')
        .appendTo($('#login-bg').css('display', 'block'));
    var tab = $('<table border=0 width=100% cellspacing=0>')
        .appendTo(dlg);

    dlg.dialog({
        autoOpen: false,
        minHeight: 100,
        height: 'auto',
        width: 'auto',
        resizable: false,
        modal: false,
        position: { my: "left top", at: "left+50 top+20", of: "#login-bg" },
        closeOnEscape: false,
        dialogClass: 'no-close',
        open: function(event, ui) {
            $(this).dialog("widget").appendTo("#login-bg");
            $(this).dialog("widget").css('z-index', 99999);
        },
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
            $('#login-bg').css('display', 'none');
        }
    });

    var focused = false;
    for(var fldIdx = 0; fldIdx < layout.fields.length; fldIdx++) {
        var tr = $('<tr>').appendTo(tab);
        var td = $('<td valign=bottom>').appendTo(tr);
        if(layout.fields[fldIdx].type == "label") {
            var fieldLabel = $('<span>');
            if(layout.fields[fldIdx].color) {
                fieldLabel.css('color', layout.fields[fldIdx].color);
            }
            fieldLabel.css('word-wrap', 'break-word')
                .css('width', '300px')
                .text(layout.fields[fldIdx].val)
                .appendTo(td.attr('colspan',2))
                .appendTo(td.attr('style', 'padding-top: 8px'));
        } else if(layout.fields[fldIdx].type == "text") {
            td.append(layout.fields[fldIdx].placeholder);
            td = $('<td valign=bottom>').appendTo(tr);
            var txt = $('<input type="text" class="text ui-widget-content ui-corner-all"/>')
                            .val(layout.fields[fldIdx].val)
                            .keypress(function(e) {
                                if(e.which == 13) {
                                    inputEnter(layout);
                                    dlg.dialog("close");
                                }
                            })
                            .appendTo(td);
            layout.fields[fldIdx].elm = txt;
            if(!focused && layout.fields[fldIdx].val.length == 0) {
                focused = true;
                setTimeout(function() { txt.focus(); }, 100);
            }
       } else if(layout.fields[fldIdx].type == "password") {
            td.append(layout.fields[fldIdx].placeholder);
            td = $('<td valign=bottom>').appendTo(tr);
            var pass = $('<input type="password" class="text ui-widget-content ui-corner-all"/>')
                            .val(layout.fields[fldIdx].val)
                            .keypress(function(e) {
                                if(e.which == 13) {
                                    inputEnter(layout);
                                    dlg.dialog("close");
                                }
                            })
                            .appendTo(td);
            layout.fields[fldIdx].elm = pass;
            if(!focused) {
                focused = true;
                setTimeout(function() { pass.focus(); }, 100);
            }
        }
    }

    dlg.dialog("open")
       .dialog("widget")
       .draggable("option","containment","#main-body");
}

function inputEnter(layout) {
    var data = {};
    for(var fldIdx = 0; fldIdx < layout.fields.length; ++fldIdx) {
        if (layout.fields[fldIdx].hasOwnProperty('elm')) {
            layout.fields[fldIdx].val = layout.fields[fldIdx].elm.val();
        }
        if (layout.fields[fldIdx].type != "label") {
            if(layout.fields[fldIdx].type == "password") {
                data[layout.fields[fldIdx].placeholder] = md5Arr(layout.fields[fldIdx].val);
            } else {
                data[layout.fields[fldIdx].placeholder] = layout.fields[fldIdx].val;
            }
        }
    }
    loginAjax(data);
}

function logout() {

    if (!dderlState.session) {
        return;
    }

    var headers = new Object();

    if (dderlState.adapter != null) {
        headers['DDERL-Adapter'] = dderlState.adapter;
    }
    headers['DDERL-Session'] = (dderlState.session != null ? '' + dderlState.session : '');

    $.ajax({
        type: 'POST',
        url: 'app/logout',
        data: JSON.stringify({}),
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: null,

        success: function(_data, textStatus, request) {
            console.log('Request logout Result ' + textStatus);
        },

        error: function (request, textStatus, errorThrown) {
            console.log('Request logout Error, status: ' + textStatus);
        }
    });
    process_logout();
}

function restart() {
    if (!dderlState.session) {
        return;
    }
    var headers = new Object();
    if (dderlState.adapter != null) {
        headers['DDERL-Adapter'] = dderlState.adapter;
    }
    headers['DDERL-Session'] = (dderlState.session != null ? '' + dderlState.session : '');
    confirm_jq({title: "Confirm restart", content:''},
            function() {
                $.ajax({
                    type: 'POST',
                    url: 'app/restart',
                    data: JSON.stringify({}),
                    dataType: "JSON",
                    contentType: "application/json; charset=utf-8",
                    headers: headers,
                    context: null,
                    success: function(response, textStatus, request) {
                        if (response.hasOwnProperty('restart')) {
                           if (response.restart == 'ok') { location.reload(true); }
                           else if (response.restart.hasOwnProperty('error')) {
                               alert_jq(response.restart.error);
                           }
                           else {
                               console.error("malformed response " + JSON.stringify(response));
                           }
                        } else {
                            console.error("malformed response " + JSON.stringify(response));
                        }
                    },
                    error: function (request, textStatus, errorThrown) {
                        console.log('Request restart Error, status: ' + textStatus);
                    }
                });
            });
}

function process_logout() {
    dderlState.connection = null;
    dderlState.adapter = null;
    $(".ui-dialog-content").dialog('close');
    $('#dashboard-menu').empty();
    if (!dderlState.session) {
        return;
    }
    dderlState.session = null;
    resetPingTimer();

    $('#login-button').html('');
    $('#btn-change-password').data("logged_in_user", "");
    $('#login-msg').html('Welcome guest');
    if(window.opener) {
        window.opener.process_logout();
    }
    if(children) {
        for(var i=0; i < children.length; ++i){
            if(!children[i].closed) {
                children[i].process_logout();
            }
        }
    }
    loginAjax(null);
}

function change_login_password(loggedInUser, shouldConnect)
{
    password_change_dlg("Change DDerl account password", loggedInUser,
            function() {
                if($('#conf_password_login').val() == $('#password_change_login').val()) {
                    var newPassJson = {
                        change_pswd: {
                            user  : loggedInUser,
                            password  : md5Arr($('#old_password_login').val()),
                            new_password  : md5Arr($('#password_change_login').val())
                        }};
                    ajaxCall(null,'login_change_pswd',newPassJson,'login_change_pswd', function(data) {
                        if(data == "ok") {
                            $("#dialog-change-password").dialog("close");
                            resetPingTimer();
                            if(shouldConnect) {
                                connect_dlg();
                            }
                        }
                        else {
                            alert_jq('Change password falied : ' + data);
                        }
                    });
                }
                else alert_jq("Confirm password missmatch!");
            });
}
