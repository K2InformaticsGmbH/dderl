var tabIdx=0;
var connects = null;

function load_connections()
{
    ajaxCall(null,'/app/adapters',{}, 'adapters', function(data) {
        var adapters = data;
        for(var i=0; i<adapters.length; ++i)
            $('#adapter_list').append($('<option>', {
                value: adapters[i].id,
                text : adapters[i].fullName 
            }));
        $('#adapter_list').combobox();
        $("#adapter_list-input").prop('readonly', true).disableSelection();
        setTimeout(function() {
            ajaxCall(null,'/app/connects',{}, 'connects', function(data) {
                connects = data;
                $('#connection_list').html('');
                for(var id in connects) {
                    $('#connection_list').append($('<option>', {
                        value: id,
                        text : connects[id].name
                    }));
                }
                set_owner_list($("#adapter_list").val());
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
    for(var id in connects)
        if(connects[id].adapter == adapter && connects[id].owner == owner)
            $('#connection_list').append($('<option>', {
                value: id,
                text : connects[id].name
            }));
    for(var id in connects)
        if(connects[id].adapter == adapter && connects[id].owner == owner) {
            $('#connection_list option[value="'+name+'"]').attr("selected","selected"); 
            break;
        } else if (owner.length == 0) {
            break;
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
    var headers = new Object();

    if (adapter != null) {
        headers['adapter'] = adapter;
    }
    headers['dderl_sess'] = (session != null ? '' + session : '');

    $.ajax({
        type: 'POST',
        url: '/app/disconnect',
        data: JSON.stringify({disconnect: {connection: connection}}),
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: null,

        success: function(_data, textStatus, request) {
            console.log('Request disconnect result ' + textStatus);
            connection = null;
            adapter = null;
            $(".ui-dialog-content").dialog('close');
            connect_dlg();
        },

        error: function (request, textStatus, errorThrown) {
            console.log('Request disconnect result ' + textStatus);
            connection = null;
            adapter = null;
            $(".ui-dialog-content").dialog('close');
            connect_dlg();
        }
    });
}

var children;
function new_connection_tab() {
    if(session) {
        if(!connection && !($("#dialog-db-login").hasClass('ui-dialog-content'))) {
            connect_dlg();
        } else {
            if(!children) {
                children = new Array();
            }
            var newURL = window.location.protocol + "//" + window.location.host;
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
      '      <tr><td valign=bottom colspan="2"><textarea id="tnsstring" class="text ui-widget-content ui-corner-all" rows=10 cols=41/></td></tr></table>' +
      '      </td></tr>' +
      '      <tr><td align=right valign=center>Username&nbsp;</td>' +
      '          <td valign=bottom><input type="text" id="user" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>Password&nbsp;</td>' +
      '          <td valign=bottom><input type="password" id="password" class="text ui-widget-content ui-corner-all"/></td></tr>' +
      '  </table>' +
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

                adapter = $('#adapter_list option:checked').val();
                var Password = $('#password').val();
                Password = (adapter == 'imem' ? (is_MD5(Password) ? Password : MD5(Password)) : Password);
                var urlConnect = '/app/connect';
                var resp = 'connect'
                var NewPassword = null;
                if($('#new_password').val()) {
                    NewPassword = $('#new_password').val();
                    if(NewPassword != $('#confirm_new_password').val()) {
                        alert("Confirm password missmatch!");
                        return;
                    }
                    NewPassword = (adapter == 'imem' ? (is_MD5(NewPassword) ? NewPassword : MD5(NewPassword)) : NewPassword);
                    urlConnect = '/app/connect_change_pswd';
                    resp = 'connect_change_pswd';
                }
                var connectJson = {connect: {name      :connName,
                                             ip        :$('#ip').val(),
                                             port      :$('#port').val(),
                                             service   :$('#service').val(),
                                             type      :$('input:radio[name=db_type]:checked').val(),
                                             user      :$('#user').val(),
                                             password  :Password,
                                             tnsstring :$('#tnsstring').val()}};

                if(NewPassword && urlConnect == '/app/connect_change_pswd') {
                    connectJson.connect.new_password = NewPassword;
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
                        Dlg.dialog("close");
                        //Setting up the global connection.
                        connection = data;
                        show_qry_files();
                    }
                });
            },
            'Delete': function() {
                delByName = $('#name').val();
                var selectedId = $('#connection_list').val();
                if (null !== selectedId && selectedId.length > 0) {
                    // delete in server
                    ajaxCall(null,'/app/del_con', {del_con: {conid: parseInt(selectedId)}}, 'del_con', function(data) {
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

function load_login_form(id) {
    if(connects.hasOwnProperty(id)) {
        document.title = connects[id].name;
        $('#ip').val(connects[id].ip);
        $('#port').val(connects[id].port);
        $('#service').val(connects[id].service);
        $('#sid').val(connects[id].sid);
        $('#user').val(connects[id].user);
        $('#tnsstring').val(connects[id].tnsstring);
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
        console.log('loaded connection id '+connects[id].name);
    }
    else {
        $('#connection_list option[selected]').removeAttr('selected');
    //    $('#ip').val('');
    //    $('#port').val('');
    //    $('#service').val('');
    //    $('#sid').val('');
    //    $('#user').val('');
    //    $('#password').val('');
    //    $('#tnsstring').val('');
    }
}
