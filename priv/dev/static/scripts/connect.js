import $ from 'jquery';
import {alert_jq, confirm_jq} from '../dialogs/dialogs';
import {dderlState, ajaxCall, resetPingTimer, initDashboards, show_qry_files,
        password_change_dlg} from './dderl';
import {loginAjax} from './login';
import {md5Arr} from './md5';
import {controlgroup_options} from '../jquery-ui-helper/helper.js';

// Jquery widgets are added to the global scope so including it 
// in one module will expose it for the rest.
import './dderl.combobox';

var adapters = null;
var owners = null;
var connects = null;

export function connect_dlg()
{
    var dlg = $('<div id="dialog-db-login">')
    .attr('title', "Connect to Database");

    var connect_common = $('<table>')
    .attr({border: 0, width: '100%', height: '85%', cellpadding: 0, cellspacing: 2})
    .appendTo(dlg);

    dlg.append($('<hr>'));

    var hideDiv = document.createElement('div');
    hideDiv.className = 'connect-hide-details';
    var hideIcon = document.createElement('span');
    hideIcon.className = 'fa fa-minus-square';
    hideDiv.appendChild(hideIcon);
    dlg.append(hideDiv);

    var connect_options = $('<div>')
    .css({width: '100%', height: '100%'})
    .appendTo(dlg);

    hideIcon.onclick = () => {
        if(connect_options[0].style.display === 'none') {
            hideIcon.className = 'fa fa-minus-square';
            connect_options[0].style.display = 'block';
            dlg[0].style.marginBottom = '0px';
        } else {
            hideIcon.className = 'fa fa-plus-square';
            connect_options[0].style.display = 'none';
            dlg[0].style.marginBottom = '20px';
        }
    };
    
    var adapter_list = $('<select class="ui-widget-content ui-corner-all">');
    var owners_list = $('<select class="ui-widget-content ui-corner-all">');
    var connection_list = $('<select class="ui-widget-content ui-corner-all">');

    connect_common.append(
        $('<tr>').append(
            $('<td>').attr({align: 'right', valign: 'center'})
                .append("DB Type"),
            $('<td>').attr({valign: 'bottom'})
                .append(adapter_list)
        ),
        $('<tr>').append(
            $('<td>').attr({align: 'right', valign: 'center'})
                .append("Ownership"),
            $('<td>').attr({valign: 'bottom'})
                .append(owners_list)
        ),
        $('<tr>').append(
            $('<td>').attr({align: 'right', valign: 'center'})
                .append("Connection Name"),
            $('<td>').attr({valign: 'bottom'})
                .append(connection_list)
        )
    );


    dlg = dlg.appendTo(document.body)
    .dialog({
        autoOpen: false,
        height: 'auto',
        width: 'auto',
        resizable: false,
        modal: true,
        position: { my: "left top", at: "left+50 top+20", of: "#main-body", collision : 'none' },
        appendTo: "#main-body",
        classes: {
            'ui-dialog': 'no-close overflow-visible'
        },
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        },
        buttons: [
            {
                id : 'btn-login',
                text:'Login / Save',
                click: function() {
                    login_save($(this), connection_list, adapter_list, owners_list);
                },
                icon: "fa fa-sign-in button-dialog-connect",
                iconPosition: "end",
                showLabel: true
            },
            {   text:'Delete',
                click:function() {
                    var conn = connection_list.find("option:selected");
                    var connData = conn.data('connect');
                    var dlg = $(this);
                    confirm_jq(
                        {title: "Confirm delete connection",
                            content: connData.name+' ('+connData.adapter+')'},
                        function() {
                            ajaxCall(null,'del_con', {del_con: {conid: connData.id}}, 'del_con', function(data) {
                                if(data.hasOwnProperty('error')) {
                                    alert_jq(JSON.stringify(data.error));
                                } else {
                                    conn.remove();
                                    if(connection_list.find("option:selected").length === 0) {
                                        dlg.dialog("close");
                                        loginAjax();
                                    } else {
                                        connection_list.parent().find('input').val(
                                            connection_list.find("option:selected").data('connect').name);
                                        connection_list.change();
                                    }
                                }
                            });
                        });
                },
                icon: "fa fa-trash button-dialog-connect",
                showLabel: false
            },
            {   text:'Clear',
                click: function() {
                    connect_options.find('input').val('');
                    connect_options.find('textarea').val('');
                    $("input:radio[name=method]:checked").val("local");
                    console.log($("input:radio[name=method]:checked").val());
                },
                icon: "fa fa-refresh button-dialog-connect",
                showLabel: false
            }
        ]
    })
    .dialog('open');
    dlg.dialog("widget").draggable("option","containment","#main-body");
    
    adapter_list.change(function() {
        if(adapter_list.children().length < 1) {
            for(var i=0; i < adapters.length; ++i)
                adapter_list.append($('<option>', {
                    value: adapters[i].id,
                    text : adapters[i].fullName 
                }));
                adapter_list.combobox();
        }
        owners_list.trigger("adapter_change");
    });

    owners_list
    .on('adapter_change', function() {
        console.log("empty the owners_list");
        owners_list.empty();
        owners_list.change();        
    })
    .change(function() {
        if(owners_list.children().length < 1) {
            var adapter = adapter_list.val();
            owners_list.parent().find('input').val('');
            for(var idx = 0; idx < owners[adapter].length; ++idx) {
                var optionAttrs = {
                    value: owners[adapter][idx],
                    text: owners[adapter][idx]
                };
                if(owners[adapter][idx] === dderlState.username) {
                    console.log("selecting...", owners[adapter][idx]);
                    optionAttrs.selected = "selected";
                }
                owners_list.append($('<option>', optionAttrs));
            }
            owners_list.combobox();
            owners_list.parent().find('input')
                .val(owners_list.find('option:selected').text());

            // FIXIT: Bad bad hack to remove scrollbar
            owners_list.parent().width(owners_list.next().width() +
                                       owners_list.next().children().last().width());
        } 
        connection_list.trigger("owner_change");
    });

    connection_list
    .on('owner_change', function() {
        connection_list.empty();
        connection_list.change();        
    })
    .change(function() {
        if(connection_list.children().length < 1) {

            var adapter = adapter_list.val();
            var owner = owners_list.val();
            var connectsArray = [];

            for(var id = 0; id < connects.length; ++id) {
                if(connects[id].adapter == adapter && connects[id].owner == owner) {
                    connectsArray.push({
                        dom: {
                            value: id,
                            text: connects[id].name
                        },
                        data: connects[id]
                    });
                }
            }
            
            connectsArray.sort(function(a, b) {
                if(a.data.method === b.data.method) {
                    return a.dom.text.localeCompare(b.dom.text);
                } else {
                    return a.data.method.localeCompare(b.data.method);
                }
            });
            
            for(var j = 0; j < connectsArray.length; ++j) {
                connection_list.append(
                    $('<option>', connectsArray[j].dom)
                        .data('connect', connectsArray[j].data)
                );
            }

            connection_list.combobox();
            connection_list.parent().find('input')
                .val(connection_list.find('option:selected').text());
        }
        load_connect_option(connection_list, connect_options);
        connect_options.find('input:text,input:password,textarea')
            .keypress(function(e) {
                if(e.which == 13) {
                    login_save(dlg, connection_list, adapter_list, owners_list);
                }
            });
    });
    
    ajaxCall(null, 'connect_info', {}, 'connect_info', function(connect_info) {
        adapters = connect_info.adapters;
        connects = connect_info.connections;
        owners = {};
        var ownersUnique = {};
        for(var id in connects) {
            if(!ownersUnique.hasOwnProperty(connects[id].adapter)) {
                ownersUnique[connects[id].adapter] = {};
                owners[connects[id].adapter] = [];
            }
            if(!ownersUnique[connects[id].adapter].hasOwnProperty(connects[id].owner)) {
                ownersUnique[connects[id].adapter][connects[id].owner] = true;
                owners[connects[id].adapter].push(connects[id].owner);
            }
        }

        adapter_list.empty();        
        adapter_list.change();

    });
}

function login_save(dlg, connection_list, adapter_list, owners_list) {
    if(dderlState.connecting === undefined) {
        $('#btn-login').button('disable');
        dderlState.connecting = true;
        var conn = connection_list.find("option:selected").data('connect');
        var conn_name = connection_list.parent().find('input').val();
        if(conn.name != conn_name)
            conn.id = null;
        conn.name    = conn_name;
        conn.adapter = adapter_list.val();
        conn.owner   = owners_list.parent().find('input').val();
        conn.method  = $("input:radio[name=method]:checked").val();
        if(conn.adapter == 'imem') {
            if(conn.method == 'local') {
                conn.schema = $('#schema').val();
                conn.secure = $('#secure').is(':checked');
            } else if(conn.method == 'tcp') {
                conn.schema = $('#schema').val();
                conn.host = $('#host').val();
                conn.port = $('#port').val();
                conn.user = $('#user').val();
                conn.password = $('#password').val();
                conn.secure = $('#secure').is(':checked');
            }
            // imem (tcp) expects passwords are md5
            if (conn.hasOwnProperty('password'))
                conn.password = md5Arr(conn.password);
        } else if(conn.adapter == 'oci') {
            // 'service', 'sid' and 'tns' input fields are
            // prefixed with inp_ to resolve conflict with
            // method radios
            if(conn.method == 'tns') {
                conn.tns = $('#inp_tns').val();
                conn.user = $('#user').val();
                conn.password = $('#password').val();
            } else if(conn.method == 'service' || conn.method == 'sid') {
                if(conn.method == 'service') {
                    conn.service = $('#inp_service').val();
                } else if(conn.method == 'sid') {
                    conn.sid = $('#inp_sid').val();
                }
                conn.host = $('#host').val();
                conn.port = $('#port').val();
                conn.user = $('#user').val();
                conn.password = $('#password').val();
            }
            if($('#language').length > 0) conn.language = $('#language').val();
            if($('#territory').length > 0) conn.territory = $('#territory').val();
        }

        dderlState.adapter = conn.adapter;
        ajaxCall(null, 'connect', conn, 'connect', function(resp) {
            function connectSuccessCb() {
                dlg.dialog("close");
                initDashboards();
                show_qry_files(false);
            }
            
            delete dderlState.connecting;
            if(resp.hasOwnProperty('owner') && resp.hasOwnProperty('conn_id')) {
                dderlState.connectionSelected =
                    {adapter: conn.adapter,
                    owner: resp.owner,
                    connection: ''+resp.conn_id};
                // Setting up the global connection.
                dderlState.connection = resp.conn;
                dderlState.connected_user = conn.owner;
                if($.isArray(connects)) {
                    dderlState.connections = connects.filter(function(c) {
                        return c.adapter == conn.adapter;
                    });
                } else {
                    dderlState.connections = []; 
                }

                var newTitle = '';
                if(dderlState.node) {
                    newTitle += dderlState.node + ' ';
                }
                if(resp.hasOwnProperty('extra') && resp.extra.hasOwnProperty('node')) {
                    newTitle += resp.extra.node;
                } else {
                    newTitle += conn.name;
                }
                document.title = newTitle;

                if(conn.method === 'local' && conn.secure === true) {
                    $('#btn-disconnect').addClass('disabled');
                }
                    
                if (resp.hasOwnProperty('extra') && resp.extra.hasOwnProperty('changePass')) {
                    change_connect_password(resp.extra.changePass, connectSuccessCb);
                } else if (resp.hasOwnProperty('extra') && resp.extra.hasOwnProperty('to')) {
                    validateSmsToken(conn.user, resp.extra, connectSuccessCb);
                } else {
                    connectSuccessCb();
                }
            } else if (resp.hasOwnProperty('error')) {
                alert_jq(resp.error);
                $('#btn-login').button('enable');
            } else {
                alert_jq(JSON.stringify(resp));
                $('#btn-login').button('enable');
            }
        }, function() {
            delete dderlState.connecting;
            $('#btn-login').button('enable');
        });
    } else {
        console.log("already trying to login");
    }
}

function load_connect_option(connection_list, connect_options) {
    var connect = connection_list.find("option:selected").data('connect');
    connect_options.empty();
    if(connect.adapter == "imem") {
        add_imem_options(connection_list, connect_options, connect);
    } else if (connect.adapter == "oci") {
        add_oci_options(connection_list, connect_options, connect);
    }
    connect_options.find("input:text,input:password,textarea")
        .addClass("text ui-widget-content ui-corner-all");

    var emptyInputs = connect_options.find('input:text[value=""],input:password[value=""]');
    if (emptyInputs.length > 0) emptyInputs[0].focus();
}

function add_methods(connection_list, connect_options, keyVals, defaultSelectedId, fn) {
    var div = $('<div>');
    for(var k in keyVals) {
        div.append(
            $('<input type="radio" id="'+k+'" name="method" value="'+k+'">'+
              '<label for="'+k+'">'+keyVals[k]+'</label>'));
    }
    
    div
    .appendTo(connect_options)
    .controlgroup(controlgroup_options())
    .attr('id','buttonList')
    .change(function() {
        var connect = connection_list.find("option:selected").data('connect');
        connect.method = $("input:radio[name=method]:checked").val();
        fn(connection_list, connect_options, connect);
        connect_options.find("input:text,input:password,textarea")
        .addClass("text ui-widget-content ui-corner-all");
        var emptyInputs = connect_options.find('input:text[value=""],input:password[value=""]');
        if (emptyInputs.length > 0) emptyInputs[0].focus();
    });
    $('#'+defaultSelectedId).attr("checked", true).checkboxradio("refresh");
}

function add_oci_options(connection_list, connect_options, connect) {
    connect_options.empty();
    add_methods(connection_list, connect_options,
                {tns: 'TNS', service : 'Service', sid : 'SID'},
                connect.method, add_oci_options);
    var options = $('<table>')
    .attr({border: 0, width: '100%', height: '100%', cellpadding: 0, cellspacing: 2})
    .appendTo(connect_options);
    if(connect.method == 'tns') {
        options.append(
            $('<tr>').append(
                $('<td>').attr('colspan',2)
                .append(
                    $('<textarea rows=10 cols=41 id="inp_tns">').val(connect.tns)
                )
            )
        );
    } else if (connect.method == 'service' || connect.method == 'sid') {
        var mthdLbl = $('<td>');
        var mthdVal = $('<td>');

        if(connect.method == 'service') {
            mthdLbl.append("Service");
            mthdVal.append($('<input type="text" id="inp_service">').val(connect.service));
        } else if(connect.method == 'sid') {
            mthdLbl.append("SID");
            mthdVal.append($('<input type="text" id="inp_sid">').val(connect.sid));
        }

        options.append(
            $('<tr>').append(mthdLbl, mthdVal),
            $('<tr>').append(
                $('<td>Host / IP</td>'),
                $('<td>').append(
                    $('<input type="text" id="host">').val(connect.host)
                )
            ),
            $('<tr>').append(
                $('<td>Port</td>'),
                $('<td>').append(
                    $('<input type="text" id="port">').val(connect.port)
                )
            )
        );
    } else {
        throw("Unknown connect method" + connect.method);
    }
    options.append(
            $('<tr>').append(
                $('<td>User</td>'),
                $('<td>').append(
                    $('<input type="text" id="user">').val(connect.user)
                    )
                ),
            $('<tr>').append(
                $('<td>Password</td>'),
                $('<td>').append(
                    $('<input type="password" id="password">')
                    )
                )
            );
    if(connect.hasOwnProperty('language'))
        options.append(
                $('<tr>').append(
                    $('<td>Language</td>'),
                    $('<td>').append(
                        $('<input type="text" id="language">').val(connect.language)
                        )
                    )
                );
    if(connect.hasOwnProperty('territory'))
        options.append(
                $('<tr>').append(
                    $('<td>Territory</td>'),
                    $('<td>').append(
                        $('<input type="text" id="territory">').val(connect.territory)
                        )
                    )
                );
}

function add_imem_options(connection_list, connect_options, connect) {
    connect_options.empty();
    add_methods(connection_list, connect_options,
                {local: 'Local', tcp : 'TCP'},
                connect.method, add_imem_options);
    
    var options = $('<table>')
    .attr({border: 0, width: '100%', height: '100%', cellpadding: 0, cellspacing: 2})
    .appendTo(connect_options);
    options.append(
            $('<tr>').append(
                $('<td>Schema</td>'),
                $('<td>').append(
                    $('<input type="text" id="schema">').val(connect.schema)
                    )
            )
    );
    if(connect.method == 'local') {
        $('#schema').attr("readonly", true);
        options.append(
            $('<tr>').append(
                $('<td>Secure</td>'),
                $('<td>').append(
                    $('<input type="checkbox" id="secure">')
                    .css('margin-left',0)
                    .attr('checked', connect.secure)
                )
            )
        );
    } else if (connect.method == 'tcp') {
        options.append(
            $('<tr>').append(
                $('<td>Host / IP</td>'),
                $('<td>').append(
                    $('<input type="text" id="host">').val(connect.host)
                )
            ),
            $('<tr>').append(
                $('<td>Port</td>'),
                $('<td>').append(
                    $('<input type="text" id="port">').val(connect.port)
                )
            ),
            $('<tr>').append(
                $('<td>User</td>'),
                $('<td>').append(
                    $('<input type="text" id="user">').val(connect.user)
                )
            ),
            $('<tr>').append(
                $('<td>Password</td>'),
                $('<td>').append(
                    $('<input type="password" id="password">')
                )
            ),
            $('<tr>').append(
                $('<td>Secure</td>'),
                $('<td>').append(
                    $('<input type="checkbox" id="secure">')
                    .css('margin-left',0)
                    .attr('checked', connect.secure)
                )
            )
        );
    } else {
        throw("Unknown connect method" + connect.method);
    }
}

export function disconnect_tab() {
    if($('#btn-disconnect').hasClass('disabled'))
        return;

    if (!dderlState.connection)
        return;

    $(".ui-dialog-content").dialog('close');
    $('#dashboard-menu').empty();

    ajaxCall(null, 'disconnect', {disconnect: {connection: dderlState.connection}}, 
        'disconnect', function(data) {
            console.log('Request disconnect result ' + data);
            dderlState.connection = null;
            dderlState.adapter = null;
            dderlState.connected_user = null;
            dderlState.service = null;
            connect_dlg();
        }, function(error) {
            console.log('Request disconnect result ' + error);
            dderlState.connection = null;
            dderlState.adapter = null;
            dderlState.connected_user = null;
            dderlState.service = null;
            $(".ui-dialog-content").dialog('close');
            $('#dashboard-menu').empty();
            connect_dlg();
        }
    );
}

export function close_tab() {
    if($('#btn-disconnect').hasClass('disabled'))
        return;

    if (!dderlState.connection)
        return;

    var headers = {};

    if (dderlState.adapter !== null) {
        headers['DDERL-Adapter'] = dderlState.adapter;
        headers['DDERL-Connection'] = dderlState.connection;
    }

    headers["X-XSRF-TOKEN"] = dderlState.xsrfToken;

    $.ajax({
        type: 'POST',
        url: 'app/close_tab',
        data: "{}",
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: null,

        success: function(_data, textStatus) {
            console.log('Request close_tab result ' + textStatus);
        },

        error: function (request, textStatus) {
            console.log('Request close_tab result ' + textStatus);
        }
    });

    // Since disconnect is called on tab the request does not go throught as the
    // connection is closed after the request is sent.
    // this is a workaround to send the request completely by not using sync req
    // following code would do sleep of 1 second.
    var now = new Date().getTime();
    while((new Date().getTime() - now) < 1000) {}
    console.log("giving up...");
}

export function change_connect_password(loggedInUser, connectSuccessCb) {
    password_change_dlg("Change connection password", '', function () {
        if ($('#conf_password_login').val() == $('#password_change_login').val()) {
            var newPassJson = {
                connection: dderlState.connection,
                service: dderlState.service,
                password: md5Arr($('#old_password_login').val()),
                new_password: $('#password_change_login').val()
            };
            ajaxCall(null, 'change_conn_pswd', newPassJson, 'change_conn_pswd', function (resp) {
                if (resp == "ok") {
                    $("#dialog-change-password").dialog("close");
                    resetPingTimer();
                    if ($.isFunction(connectSuccessCb))
                        connectSuccessCb();
                } else if (resp.hasOwnProperty('error')) {
                    alert_jq('Change password falied : ' + resp.error);
                } else {
                    alert_jq('Change password falied : ' + JSON.stringify(resp));
                }
            });
        }
        else alert_jq("Confirm password missmatch!");
    });
}

function validateSmsToken(user, data, connectSuccessCb)
{
    var dlg = $('<div title="Enter Token" style="diaply:none">')
        .appendTo(document.body);

    var tokenInp = $('<input type="text" class="text ui-widget-content ui-corner-all"/>');

    $('<table border=0 width=100% cellspacing=0>').append(
        $('<tr>').append(
            $('<td valign=bottom>')
            .attr('colspan',2)
            .append(
                $('<span>')
                .css({'word-wrap' : 'break-word',
                      display     : 'block',
                      width       : '300px'})
                .text("A token is send through SMS to "+data.to+
                      " for user "+user+". Please enter the token below"))
        ),
        $('<tr>').append(
            $('<td valign=bottom>')
            .attr('colspan',2)
            .append(tokenInp)
        )
    ).appendTo(dlg);

    dlg.dialog({
        autoOpen: false,
        minHeight: 100,
        height: 'auto',
        width: 'auto',
        resizable: false,
        modal: false,
        position: { my: "left top", at: "left+80 top+300", of: "#main-body", collision : 'none' },
        closeOnEscape: false,
        classes: {
            'ui-dialog': 'no-close'
        },
        appendTo: '#main-body',
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        }
    })
    .dialog("open")
    .dialog("widget")
    .draggable("option","containment","#main-body");

    tokenInp.keypress(function(e) {
        if(e.which == 13) {
            ajaxCall(null, 'smstoken',
                    {user: user,
                     connection: dderlState.connection,
                     smstoken : tokenInp.val()}, 'smstoken', function(resp) {
                if(resp.hasOwnProperty('error')) {
                    alert_jq(resp.error);
                } else if (resp.hasOwnProperty('changePass')) {
                    change_connect_password(resp.changePass,
                        function() {
                            connectSuccessCb();
                            dlg.dialog("close");
                        });
                } else {
                    connectSuccessCb();
                    dlg.dialog("close");
                }
            });
        }
    });

    setTimeout(function() { tokenInp.focus(); }, 100);
}
