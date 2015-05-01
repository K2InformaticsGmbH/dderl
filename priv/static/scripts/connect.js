var tabIdx=0;
var connects = null;

function load_connections()
{
    ajaxCall(null,'adapters',{}, 'adapters', function(data) {
        var adapters = data;
        for(var i=0; i < adapters.length; ++i)
            $('#adapter_list').append($('<option>', {
                value: adapters[i].id,
                text : adapters[i].fullName 
            }));
        $('#adapter_list').combobox();
        $("#adapter_list-input").prop('readonly', true).disableSelection();
        setTimeout(function() {
            ajaxCall(null,'connects',{}, 'connects', function(data) {
                connects = data;
                $('#connection_list').html('');
                var connectsArray = new Array();
                for(var id in connects) {
                    connectsArray.push({value: id, text: connects[id].name});
                }
                connectsArray.sort(function(a, b) {
                    return a.text == b.text ? 0: a.text < b.text ? -1 : 1;
                });
                for(var j = 0; j < connectsArray.length; ++j) {
                    $('#connection_list').append($('<option>', connectsArray[j]));
                }
                $('#connection_list').sort();
                set_owner_list($("#adapter_list").val());
                if(dderlState && dderlState.connectionSelected) {
                    setConnectValues(dderlState.connectionSelected);
                }
            });
        }, 1);
    });

    $('#adapter_list').change(function() {
        set_owner_list($("#adapter_list").val());
    });

    $('#owners_list').change(function() {
        set_conns_list($("#adapter_list").val(), $("#owners_list").val());
    });

    $('#connection_list').change(function() {
        var selectedId = $("#connection_list").val();
        var selectedText = $("#connection_list option:selected").text();
        $('#connection_list-input').val(selectedText);
        load_login_form(selectedId);
    });
}

function set_owner_list(adapter)
{
    var owners = new Object();
    $('#owners_list').empty();
    for(var id in connects)
        if(connects[id].adapter == adapter)
            owners[connects[id].owner] = true;
    for(var owner in owners)
        $('#owners_list').append($('<option>', {
            value: owner,
            text : owner 
        }));
    for(var id in connects)
        if(connects[id].adapter == adapter) {
            $('#owners_list option[value="'+connects[id].owner+'"]').attr("selected","selected"); 
            break;
        }
    $('#owners_list').combobox();
    if($("#owners_list").children().length === 0) {
        $('#owners_list-input').val("");
        set_conns_list(adapter, "");
    } else {
        var currentOwner = $('#owners_list').val();
        $('#owners_list-input').val(currentOwner);
        set_conns_list(adapter, currentOwner);
    }
}

function set_conns_list(adapter, owner)
{
    $('#connection_list').empty();
    var connectsArray = new Array();
    for(var id in connects) {
        if(connects[id].adapter == adapter && connects[id].owner == owner) {
            connectsArray.push({
                value: id,
                text : connects[id].name
            });
        }
    }
    connectsArray.sort(function(a, b) {
        return a.text == b.text ? 0: a.text < b.text ? -1 : 1;
    });
    for(var i = 0; i < connectsArray.length; ++i) {
        $('#connection_list').append($('<option>', connectsArray[i]));
    }
    $('#connection_list').combobox();
    if($("#connection_list").children().length === 0) {
        $('#connection_list-input').val("");
        load_login_form('');
    } else {
        var selectedId = $("#connection_list").val();
        var selectedText = $("#connection_list option:selected").text();
        $('#connection_list-input').val(selectedText);
        load_login_form(selectedId);
    }
}

function disconnect_tab() {
    if (!dderlState.connection) {
        return;
    }
    var headers = new Object();

    if (dderlState.adapter != null) {
        headers['DDERL-Adapter'] = dderlState.adapter;
    }
    headers['DDERL-Session'] = (dderlState.session != null ? '' + dderlState.session : '');
    $(".ui-dialog-content").dialog('close');
    $('#dashboard-menu').empty();

    $.ajax({
        type: 'POST',
        url: 'app/disconnect',
        data: JSON.stringify({disconnect: {connection: dderlState.connection}}),
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: null,

        success: function(_data, textStatus, request) {
            console.log('Request disconnect result ' + textStatus);
            dderlState.connection = null;
            dderlState.adapter = null;
            dderlState.connected_user = null;
            dderlState.service = null;
            connect_dlg();
        },

        error: function (request, textStatus, errorThrown) {
            console.log('Request disconnect result ' + textStatus);
            dderlState.connection = null;
            dderlState.adapter = null;
            dderlState.connected_user = null;
            dderlState.service = null;
            $(".ui-dialog-content").dialog('close');
            $('#dashboard-menu').empty();
            connect_dlg();
        }
    });
}

var children;
function new_connection_tab() {
    if(dderlState.session) {
        if(!dderlState.connection && !($("#dialog-db-login").hasClass('ui-dialog-content'))) {
            connect_dlg();
        } else {
            if(!children) {
                children = new Array();
            }
            var newURL = window.location.protocol+"//"+window.location.host+window.location.pathname;
            console.log(newURL);
            children.push(window.open(newURL, "_blank"));
        }
    }
}

function connect_dlg()
{
    $('<div id="dialog-db-login" title="Connect to Database">' +
      '  <table id="connect-html-table" border=0 width=100% height=85% cellpadding=0 cellspacing=0>' +
      '      <tr><td align=right valign=center>DB Type&nbsp;</td>' +
      '          <td valign=bottom><select id="adapter_list" class="ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>Ownership&nbsp;</td>' +
      '          <td valign=bottom><select id="owners_list" class="ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>Connection Name&nbsp;</td>' +
      '          <td valign=bottom><select id="connection_list" class="ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td colspan=2><hr></td></tr>' +
      '      <tr><td align=right valign=center>Connect Method&nbsp;</td>' +
      '          <td valign=center>'+
      '             <table border=0 cellpadding=0 cellspacing=0>' +
      '              <tr><td valign=center><input type="radio" name="db_type" value="service" checked></td>' +
      '                  <td valign=center>&nbsp;Service&nbsp;&nbsp;</td>' +
      '                  <td valign=center><input type="radio" name="db_type" value="sid"></td>' +
      '                  <td valign=center>&nbsp;SID</td>' +
      '                  <td valign=center><input type="radio" name="db_type" value="tns"></td>' +
      '                  <td valign=center>&nbsp;TNS</td></tr>' +
      '             </table>' +
      '          </td></tr>' +
      '      <tr><td align=right colspan=2>'+
      '         <table id="con_othrs" width=100% border=0 cellpadding=0 cellspacing=0 style="background-color:silver;">' +
      '          <tr><td align=right valign=center>Host&nbsp;</td>' +
      '          <td valign=bottom><input type="text" id="ip" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>Port&nbsp;</td>' +
      '          <td valign=bottom><input type="text" id="port" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center id="con_name">DB&nbsp;</td>' +
      '          <td valign=bottom><input type="text" id="service" class="text ui-widget-content ui-corner-all"/></td></tr></table>' +
      '         <table id="con_tns" width=100% border=0 cellpadding=0 cellspacing=0>' +
      '      <tr><td valign=bottom colspan="2"><textarea id="tnsstr" class="text ui-widget-content ui-corner-all" rows=10 cols=41/></td></tr></table>' +
      '      </td></tr>' +
      '      <tr><td align=right valign=center>Username&nbsp;</td>' +
      '          <td valign=bottom><input type="text" id="user" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>Password&nbsp;</td>' +
      '          <td valign=bottom><input type="password" id="password" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=left valign=center><input type="checkbox" id="secure" value="1">&nbsp;Secure</td></tr>' +
      '  </table>' +
      '  <input type="hidden" id="languange"><input type="hidden" id="territory"><input type="hidden" id="charset">' +
      '</div>')
    .appendTo(document.body)
    .dialog({
        autoOpen: false,
        height: 'auto',
        width: 'auto',
        resizable: false,
        modal: false,
        position: { my: "left top", at: "left+50 top+20", of: "#main-body" },
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        },
        open: function(evt, ui) {
            $('#adapter_list').width(200);
            $('#owners_list').width(200);
            $('#connection_list').width(200);
            $(this).dialog("widget").appendTo("#main-body");
        },
        buttons: {
            'Login / Save': function() {
                var connName = $('#connection_list-input').val();
                if (!connName) {
                    alert_jq('Please provide a connection name for the new connection, or select an existing one');
                    return;
                }
                document.title = connName;

                dderlState.adapter = $('#adapter_list option:checked').val();
                var Password = $('#password').val();
                var urlConnect = 'connect';
                var resp = 'connect'
                var NewPassword = null;
                if($('#new_password').val()) {
                    NewPassword = $('#new_password').val();
                    if(NewPassword != $('#confirm_new_password').val()) {
                        alert("Confirm password missmatch!");
                        return;
                    }
                    urlConnect = 'connect_change_pswd';
                    resp = 'connect_change_pswd';
                }

                var connectJson = {connect: {name      :connName,
                                             secure    :$('#secure').is(':checked'),
                                             ip        :$('#ip').val(),
                                             port      :$('#port').val(),
                                             service   :$('#service').val(),
                                             type      :$('input:radio[name=db_type]:checked').val(),
                                             user      :$('#user').val(),
                                             password  :Password,
                                             tnsstr    :$('#tnsstr').val(),
                                             languange :$('#languange').val(),
                                             territory :$('#territory').val(),
                                             charset   :$('#charset').val()}};

                if(NewPassword && urlConnect == 'connect_change_pswd') {
                    connectJson.connect.new_password = NewPassword;
                }
                // Add the current id if we are not creating a new connection
                if(connName === $("#connection_list option:selected").text()) {
                    var selectedConnIdAsInt = parseInt($("#connection_list").val())
                    if(!isNaN(selectedConnIdAsInt)) {
                        connectJson.connect.id = selectedConnIdAsInt;
                    }
                }

                var Dlg = $(this);
                ajaxCall(null, urlConnect, connectJson, resp, function(data) {
                    if(data == "expired") {
                        alert("Pasword expired, please change it");
                        $("#connect-html-table").append('<tr><td align=right valign=center>New Password&nbsp;</td><td valign=bottom><input type="password" id="new_password" class="text ui-widget-content ui-corner-all"/></td></tr>');
                        $("#connect-html-table").append('<tr><td align=right valign=center>Confirm Password&nbsp;</td><td valign=bottom><input type="password" id="confirm_new_password" class="text ui-widget-content ui-corner-all"/></td></tr>');
                    } else if(data.hasOwnProperty('error')) {
                        alert_jq(
                            'Unable to connect<br>'+
                            'Host : '+$("#ip").val()+'<br>'+
                            'Port : '+$("#port").val()+'<br>'+
                            'User : '+$("#user").val()+'<br>'+
                            'Error: '+data.error
                        );
                    } else {
                        saveConnectValues($("#adapter_list").val(), data.owner, data.conn_id);
                        //Setting up the global connection.
                        dderlState.connection = data.conn;
                        dderlState.connected_user = $('#user').val();
                        dderlState.service = $('#service').val();
                        Dlg.dialog("close");
                        initDashboards();
                        show_qry_files(false);
                    }
                });
            },
            'Delete': function() {
                delByName = $('#name').val();
                var selectedId = $('#connection_list').val();
                if (null !== selectedId && selectedId.length > 0) {
                    // delete in server
                    ajaxCall(null,'del_con', {del_con: {conid: parseInt(selectedId)}}, 'del_con', function(data) {
                        if(data.hasOwnProperty('error')) {
                            alert_jq(JSON.stringify(data.error));
                        } else {
                            // update list on success
                            $('#connection_list option[value="'+selectedId+'"]').remove();
                            delete connects[selectedId];
                            var id = null;
                            for(id in connects);
                            if(null != id) {
                                $('#connection_list-input').val(connects[id].name);
                                load_login_form(id);
                            } else {
                                $('#connection_list-input').val("");
                                load_login_form('');
                            }
                        }
                    });
                }
            }
        }
    })
    .dialog('open')
    .dialog("widget").draggable("option","containment","#main-body");

    $("#password").keypress(function(e) {
        if(e.which == 13) {
            $("#dialog-db-login").dialog('option', 'buttons')["Login / Save"].apply($("#dialog-db-login"));
        }
    });

    $('#con_tns').hide();
    $('#con_othrs').show();

    load_connections();

    // Changes between Service / SID / TNS
    $('input[name="db_type"]').change( function() {
      $('#con_tns').hide();
      $('#con_othrs').show();
      if($(this).val().toUpperCase() == 'TNS') {
        $('#con_tns').show();
        $('#con_othrs').hide();
      }
      $('#con_name').html($(this).val().toUpperCase() + '&nbsp;');
    });
}

function change_connect_password(loggedInUser)
{
    $('<div id="dialog-change-password" title="Change account password">' +
      '  <table border=0 width=100% height=85% cellpadding=0 cellspacing=0>' +
      '      <tr><td align=right valign=center>User&nbsp;</td>' +
      '          <td valign=center><b>'+loggedInUser+'</b></td></tr>' +
      '      <tr><td align=right valign=center>Old Password&nbsp;</td>' +
      '          <td valign=bottom><input type="password" id="old_password_login" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>New Password&nbsp;</td>' +
      '          <td valign=bottom><input type="password" id="password_change_login" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>Confirm Password&nbsp;</td>' +
      '          <td valign=bottom><input type="password" id="conf_password_login" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '  </table>' +
      '</div>').appendTo(document.body);
    $('#dialog-change-password').dialog({
        autoOpen: false,
        height: 200,
        width: 300,
        resizable: false,
        modal: false,
        open: function() {
            $(this).dialog("widget").appendTo("#main-body");
        },
        close: function() {
            $("#dialog-change-password").dialog('destroy');
            $("#dialog-change-password").remove();
        },
        buttons: {
            "Change Password": function() {
                if($('#conf_password_login').val() == $('#password_change_login').val()) {
                    var newPassJson = {
                        change_pswd: {
                            connection: dderlState.connection,
                            service : dderlState.service,
                            user  : loggedInUser,
                            password  : $('#old_password_login').val(),
                            new_password  : $('#password_change_login').val()
                        }};
                    ajaxCall(null, 'change_conn_pswd', newPassJson, 'change_conn_pswd', function(data) {
                        if(data == "ok") {
                            $("#dialog-change-password").dialog("close");
                            resetPingTimer();
                        }
                        else {
                            alert('Change password falied : ' + data);
                        }
                    });
                }
                else alert("Confirm password missmatch!");
            },
            Cancel: function() {
                $(this).dialog("close");
            }
        }
    })
    .dialog("open")
    .dialog("widget").draggable("option","containment","#main-body");
}

function saveConnectValues(adapter, owner, connection) {
    var connStr = connection + "";
    dderlState.connectionSelected = {adapter: adapter, owner: owner, connection: connStr};
}

function setConnectValues(selectedValues) {
    $("#adapter_list").val(selectedValues.adapter);
    var adapterText = $("#adapter_list option:selected").text();
    $("#adapter_list-input").val(adapterText);
    $("#adapter_list").change();

    $("#owners_list").val(selectedValues.owner);
    var ownerText = $("#owners_list option:selected").text();
    $("#owners_list-input").val(ownerText);
    $("#owners_list").change();

    $("#connection_list").val(selectedValues.connection);
    var selectedText = $("#connection_list option:selected").text();
    $('#connection_list-input').val(selectedText);
    load_login_form(selectedValues.connection);
}

function load_login_form(id) {
    if(connects.hasOwnProperty(id)) {
        document.title = connects[id].name;
        $('#ip').val(connects[id].ip);
        $('#port').val(connects[id].port);
        $('#service').val(connects[id].service);
        secureCheckbox = document.getElementById("secure");
        if(connects[id].secure != null && (connects[id].secure === true || connects[id].secure.toLowerCase() === "true")) {
            secureCheckbox.checked = true;
        } else {
            secureCheckbox.checked = false;
        }
        $('#sid').val(connects[id].sid);
        $('#user').val(connects[id].user);
        $('#tnsstr').val(connects[id].tnsstr);
        $('#languange').val(connects[id].language);
        $('#territory').val(connects[id].territory);
        $('#charset').val(connects[id].charset);
        $('input:radio[name=db_type][value='+connects[id].type+']').click();
        var conName = '';
        if(connects[id].hasOwnProperty('type')) {
            conName = connects[id].type.toUpperCase();
            if(conName == 'TNS') {
                $('#con_tns').show();
                $('#con_othrs').hide();
            }
        }
        $('#con_name').html(conName + "&nbsp;");
        $('#adapter_list option[value="'+connects[id].adapter+'"]').attr("selected","selected"); 
        $('#connection_list option[selected]').removeAttr('selected');
        $('#connection_list option[value="'+id+'"]').attr("selected","selected"); 
        $('#password').focus();
        $('#dialog-db-login').data(id);
        console.log('loaded connection id '+ id);
    }
    else {
        $('#connection_list option[selected]').removeAttr('selected');
    }
}
