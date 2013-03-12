var tabIdx=0;
var connects = null;

function load_connections()
{
    ajax_post('/app/adapters', {}, null, null, function(data) {
        var adapters = data.adapters;
        for(var i=0; i<adapters.length; ++i)
            $('#adapter_list').append($('<option>', {
                value: adapters[i].id,
                text : adapters[i].fullName 
            }));
        setTimeout(function() {
            ajax_post('/app/connects', null, null, null, function(data) {
                connects = data.connects;
                $('#connection_list').html('');
                for(var id in connects)
                    $('#connection_list').append($('<option>', {
                        value: id,
                        text : connects[id].name
                    }));
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
    $('#owners_list').jecKill();
    //set_conns_list(adapter, '');
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
            set_conns_list(adapter, connects[id].owner);
            break;
        }
    $('#owners_list').jec();
}

function set_conns_list(adapter, owner)
{
    $('#connection_list').empty();
    $('#connection_list').jecKill();
    for(var id in connects)
        if(connects[id].adapter == adapter && connects[id].owner == owner)
            $('#connection_list').append($('<option>', {
                value: id,
                text : connects[id].name
            }));
    for(var id in connects)
        if(connects[id].adapter == adapter && connects[id].owner == owner) {
            $('#connection_list option[value="'+name+'"]').attr("selected","selected"); 
            load_login_form(id);
            break;
        } else if (owner.length == 0) {
            load_login_form('');
            break;
        }
    $('#connection_list').jec();
}

function connect_dlg()
{
    $('<div id="dialog-db-login" title="Connect to Database">' +
      '  <table border=0 width=100% height=85% cellpadding=0 cellspacing=0>' +
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
        modal: true,
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        },
        open: function(evt, ui) {
            $('#adapter_list').width(200);
            $('#owners_list').width(200);
            $('#connection_list').width(200);
        },
        buttons: {
            'Login / Save': function() {
                var newName = $('#connection_list').jecValue();
                var selName = $('#connection_list option[selected]').text();
                if (newName.length === 0 && selName.length === 0) {
                    alert_jq('Please provide a connection name for the new connection, or select an existing one');
                    return;
                }

                adapter = $('#adapter_list option:checked').val();
                Password = $('#password').val();
                Password = (adapter == 'imem' ? (is_MD5(Password) ? Password : MD5(Password)) : Password);
                var conname = (newName.length === 0 ? selName : newName);
                var connectJson = {connect: {name      :conname,
                                             ip        :$('#ip').val(),
                                             port      :$('#port').val(),
                                             service   :$('#service').val(),
                                             type      :$('input:radio[name=db_type]:checked').val(),
                                             user      :$('#user').val(),
                                             password  :Password,
                                             tnsstring :$('#tnsstring').val()}};
                owner = $('#user').val();
                var Dlg = $(this);
                ajax_post('/app/connect', connectJson, null, null, function(data) {
                    if(data.connect.hasOwnProperty('error')) {
                        alert_jq(
                            'Unable to connect<br>'+
                            'Host : '+$("#ip").val()+'<br>'+
                            'Port : '+$("#port").val()+'<br>'+
                            'User : '+$("#user").val()+'<br>'+
                            'Error: '+data.connect.error
                        );
                    } else {
                        Dlg.dialog("close");
                        show_qry_files(data.connect);
                    }
                });
            },
            'Delete': function() {
                delByName = $('#name').val();
                var selectedId = $('#connection_list').val();
                if (null !== selectedId && selectedId.length > 0) {
                    // delete in server
                    ajax_post('/app/del_con', {del_con: {conid: parseInt(selectedId)}}, null, null, function(data) {
                        if(data.del_con.hasOwnProperty('error')) {
                            alert_jq(JSON.stringify(data.del_con.error));
                        } else {
                            // update list on success
                            $('#connection_list option[value="'+selectedId+'"]').remove();
                            delete connects[selectedId];
                            var id = null;
                            for(id in connects);
                            if(null != id) load_login_form(id);
                        }
                    });
                }
            }
        }
    })
    .dialog('open');
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
//        $('#password').val("change_on_install"); // TODO remove after test
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
