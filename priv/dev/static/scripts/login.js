import $ from 'jquery';
import { alert_jq, confirm_jq } from '../dialogs/dialogs';
import { dderlState, ajaxCall, resetPingTimer, password_change_dlg } from './dderl';
import { md5Arr } from './md5';
import { connect_dlg } from './connect';
import { stopScreensaver, startScreensaver } from './screensaver';

function update_user_information(user) {
    $('#btn-change-password').data("logged_in_user", user);
    $('#login-button').html('Log out ' + user);
    dderlState.username = user;
}

function refresh_header_information() {
    document.title = dderlState.node;
    $('#version').text(' | ' + dderlState.vsn);
    $('#node').text(dderlState.node + '@' + dderlState.host);
}

export function loginAjax(data = {}) {
    ajaxCall(null, 'login', data, 'login', loginCb);
}

window.loginCb = loginCb;

function loginCb(resp) {
    try {
        if (window.opener && window.opener.isScreensaver && window.opener.loginCb && $.isFunction(window.opener.loginCb)) {
            window.opener.loginCb(resp);
            window.close();
            return;
        }
    }
    catch (e) {
        //error due to accessing window from a different domain
        console.log(e);
    }

    if (dderlState.screensaver && window.tab && !resp.saml) {
        window.tab.close();
    }

    if (resp.hasOwnProperty('vsn')) {
        dderlState.vsn = resp.vsn;
    }
    if (resp.hasOwnProperty('node')) {
        dderlState.node = resp.node;
    }
    if (resp.hasOwnProperty('host')) {
        dderlState.host = resp.host;
    }
    if (resp.hasOwnProperty('app')) {
        dderlState.app = resp.app;
    }
    if (resp.hasOwnProperty('rowNumLimit')) {
        dderlState.rowNumLimit = resp.rowNumLimit;
    }

    if (!dderlState.screensaver) {
        refresh_header_information();
    }

    var cookies = document.cookie;
    if (cookies) {
        var cs = cookies.split("; ");
        for (var i = 0; i < cs.length; i++) {
            if (cs[i].startsWith(dderlState.node + "DDERL-XSRF-TOKEN")) {
                dderlState.xsrfToken = cs[i].substring(cs[i].indexOf("=") + 1);
                break;
            }
        }
    }

    if (resp.hasOwnProperty('error')) {
        if (dderlState.screensaver) {
            display(
                {
                    title: "Session is locked",
                    screensaver: true,
                    fields: [{
                        type: "label",
                        val: resp.error,
                        color: "#DD1122"
                    }] //Swisscom red color
                });
        } else {
            var accountName = "";
            if (resp.hasOwnProperty('pwdmd5')) {
                accountName = resp.pwdmd5.accountName;
            }
            display({
                title: "Login",
                fields: [{
                    type: "text",
                    placeholder: "User",
                    name: "user",
                    val: accountName
                },
                {
                    type: "password",
                    placeholder: "Password",
                    name: "password",
                    val: ""
                },
                {
                    type: "label",
                    val: resp.error,
                    color: "#DD1122"
                }] //Swisscom red color
            });
            ajaxCall(null, 'login', {}, 'login', null);
        }
    } else if (resp.hasOwnProperty('pwdmd5')) {
        display({
            title: "Login",
            fields: [{
                type: "text",
                placeholder: "User",
                name: "user",
                val: resp.pwdmd5.accountName || dderlState.username
            },
            {
                type: "password",
                placeholder: "Password",
                name: "password",
                val: ""
            }]
        });
    } else if (resp.hasOwnProperty('smsott')) {
        display({
            title: "Enter Token",
            fields: [{
                type: "label",
                val: "A token is send through SMS to " + resp.smsott.to +
                    " for user " + resp.smsott.accountName +
                    ". Please enter the token below"
            },
            {
                type: "text",
                placeholder: "SMS Token",
                name: "smsott",
                val: ""
            }]
        });
    } else if (resp.hasOwnProperty('saml')) {
        if (resp.saml.hasOwnProperty('form')) {
            if (dderlState.screensaver && window.tab) {
                window.tab.document.body.innerHTML = resp.saml.form;
                var form = window.tab.document.getElementById('samlForm');
                form.submit();
            } else {
                $("body").append(resp.saml.form);
                $("#samlForm").submit();
            }
        }
    } else if (resp.hasOwnProperty('accountName')) {
        update_user_information(resp.accountName);
        dderlState.isLoggedIn = true;
        resetPingTimer();
        removeScreenSaver();
    } else if (resp.hasOwnProperty('changePass')) {
        change_login_password(resp.changePass, true);
    } else if (resp == 'logout') {
        removeScreenSaver();
        logout(true);
    } else {
        alert_jq("Unexpected " + JSON.stringify(resp));
    }
}

export function showScreeSaver() {
    startScreensaver();
    display({
        title: "Session is locked",
        fields: [],
        screensaver: true
    });
}

function removeScreenSaver() {
    if (dderlState.screensaver) {
        window.isScreensaver = false;
        dderlState.screensaver = false;
        stopScreensaver();
        $("#world").hide();
    } else {
        connect_dlg();
    }
}

function display(layout) {
    var dlg = $('<div title="' + layout.title + '" style="display:none">')
        .appendTo($('#login-bg').css('display', 'block').addClass('center'));
    var tab = $('<table border=0 width=100% cellspacing=0>')
        .appendTo(dlg);

    dlg.dialog({
        autoOpen: false,
        minHeight: 70,
        height: 'auto',
        width: 220,
        resizable: false,
        modal: false,
        position: { my: "left top", at: "left+50 top+20", of: "#login-bg" },
        closeOnEscape: false,
        classes: {
            'ui-dialog': 'dderl-dialog no-close'
        },
        appendTo: "#login-bg",
        open: function () {
            $(this).dialog("widget").css('z-index', 99999);
        },
        close: function () {
            $(this).dialog('destroy');
            $(this).remove();
            $('#login-bg').css('display', 'none');
        }
    });

    var focused = false;
    function setFocus(element, delay) {
        focused = true;
        setTimeout(function () { element.focus(); }, delay);
    }
    function loginEnterKeyPressHandler(e) {
        if (e.which === 13) {
            inputEnter(layout);
            dlg.dialog("close");
        }
    }
    for (let fldIdx = 0; fldIdx < layout.fields.length; fldIdx++) {
        let tr = $('<tr>').appendTo(tab);
        let td = $('<td valign=bottom>').appendTo(tr);
        if (layout.fields[fldIdx].type == "label") {
            var fieldLabel = $('<span>');
            if (layout.fields[fldIdx].color) {
                fieldLabel.css('color', layout.fields[fldIdx].color);
            }
            td.attr('colspan', 2)
                .attr('style', 'padding-top: 8px');

            fieldLabel.addClass('error-msg')
                .text(layout.fields[fldIdx].val)
                .appendTo(td);
        } else if (layout.fields[fldIdx].type == "text") {
            var txt = $('<input type="text" class="text ui-widget-content ui-corner-all"/>')
                .attr('placeholder', layout.fields[fldIdx].placeholder)
                .val(layout.fields[fldIdx].val)
                .keypress(loginEnterKeyPressHandler)
                .appendTo(td);
            if (layout.fields[fldIdx].val) {
                txt = txt.attr('readonly', 'readonly');
            }
            layout.fields[fldIdx].elm = txt;
            if (!focused && layout.fields[fldIdx].val.length === 0) {
                setFocus(txt, 100);
            }
        } else if (layout.fields[fldIdx].type == "password") {
            var pass = $('<input type="password" class="text ui-widget-content ui-corner-all"/>')
                .attr('placeholder', layout.fields[fldIdx].placeholder).css("margin-top", "3px")
                .val(layout.fields[fldIdx].val)
                .keypress(loginEnterKeyPressHandler)
                .appendTo(td);
            layout.fields[fldIdx].elm = pass;
            if (!focused) {
                setFocus(pass, 100);
            }
        }
    }
    let tr = $('<tr>').appendTo(tab);
    let td = $('<td class="center">').appendTo(tr);
    var button = $('<input type="button" class="button" value="Login">');
    button.appendTo(td);
    button.click(function () {
        inputEnter(layout);
        dlg.dialog("close");
    });
    dlg.dialog("open")
        .dialog("widget")
        .draggable("option", "containment", "#main-body");
}

function inputEnter(layout) {
    var data = {};
    if (!layout.screensaver) {
        for (var fldIdx = 0; fldIdx < layout.fields.length; ++fldIdx) {
            if (layout.fields[fldIdx].hasOwnProperty('elm')) {
                layout.fields[fldIdx].val = layout.fields[fldIdx].elm.val();
            }
            if (layout.fields[fldIdx].type != "label") {
                if (layout.fields[fldIdx].type == "password") {
                    data[layout.fields[fldIdx].name] = md5Arr(layout.fields[fldIdx].val);
                } else {
                    data[layout.fields[fldIdx].name] = layout.fields[fldIdx].val;
                }
            }
        }
    } else {
        window.isScreensaver = true;
        window.tab = window.open('', '_blank');
        data = {};
    }
    loginAjax(data);
}

export function logout(isForceful) {
    if (dderlState.connection && isForceful !== true) {
        ajaxCall(null, 'check_session', null, 'check_session', function () {
            confirm_jq({ title: "Confirm logout", content: '' }, function () {
                exec_logout();
            });
        }, function () {
            exec_logout();
        });
    } else {
        exec_logout();
    }

    function exec_logout() {
        ajaxCall(null, 'logout', '{}', 'logout', function (data) {
            console.log('Request logout Result ' + data);
            process_logout();
        }, function () {
            // We have to cleanup even when the server is not recheable
            process_logout();
        });
    }
}

//TODO: Does this function belong here ?
export function restart() {
    confirm_jq({ title: "Confirm restart", content: '' },
        function () {
            ajaxCall(null, 'restart', JSON.stringify({}), 'restart', function (data) {
                console.log('Request restart Result ' + data);
                if (data == 'ok') {
                    location.reload(true);
                } else if (data.hasOwnProperty('error')) {
                    alert_jq(data.error);
                } else {
                    console.error("malformed response " + JSON.stringify(data));
                }
            });
        });
}

export function new_connection_tab() {
    if (!dderlState.connection && !($("#dialog-db-login").hasClass('ui-dialog-content'))) {
        connect_dlg();
    } else {
        var newURL = window.location.protocol + "//" + window.location.host + window.location.pathname;
        console.log(newURL);
        window.open(newURL, "_blank");
    }
}

function process_logout() {
    dderlState.isLoggedIn = false;
    dderlState.connection = null;
    dderlState.adapter = null;
    dderlState.username = '';
    dderlState.screensaver = false;
    $(".ui-dialog-content").dialog('close');
    $('#dashboard-menu').empty();
    resetPingTimer();
    $('#login-button').html('');
    $('#btn-change-password').data("logged_in_user", "");
    $('#login-msg').html('Welcome guest');
    display({
        title: "Successfully logged out",
        fields: []
    });
}

export function change_login_password(loggedInUser, shouldConnect) {
    password_change_dlg("Change DDerl account password", loggedInUser, function () {
        if ($('#conf_password_login').val() == $('#password_change_login').val()) {
            var newPassJson = {
                change_pswd: {
                    user: loggedInUser,
                    password: md5Arr($('#old_password_login').val()),
                    new_password: $('#password_change_login').val()
                }
            };
            ajaxCall(null, 'login_change_pswd', newPassJson, 'login_change_pswd', function (data) {
                if (data == "ok") {
                    $("#dialog-change-password").dialog("close");
                    resetPingTimer();
                    dderlState.isLoggedIn = true;
                    update_user_information(loggedInUser);
                    if (shouldConnect) {
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
