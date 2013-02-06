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
                for(var name in connects)
                    $('#connection_list').append($('<option>', {
                        value: name,
                        text : name 
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
        var selectedName = $("#connection_list").val();
        document.title = pageTitlePrefix + selectedName;
        load_login_form(selectedName);
    });
}

function set_owner_list(adapter)
{
    var owners = new Object();

    $('#owners_list').empty();
    $('#owners_list').jecKill();
    //set_conns_list(adapter, '');
    for(var name in connects)
        if(connects[name].adapter == adapter)
            owners[connects[name].owner] = true;
    for(var owner in owners)
        $('#owners_list').append($('<option>', {
            value: owner,
            text : owner 
        }));
    for(var name in connects)
        if(connects[name].adapter == adapter) {
            $('#owners_list option[value="'+connects[name].owner+'"]').attr("selected","selected"); 
            set_conns_list(adapter, connects[name].owner);
            break;
        }
    $('#owners_list').jec();
}

function set_conns_list(adapter, owner)
{
    $('#connection_list').empty();
    $('#connection_list').jecKill();
    for(var name in connects)
        if(connects[name].adapter == adapter && connects[name].owner == owner)
            $('#connection_list').append($('<option>', {
                value: name,
                text : name
            }));
    for(var name in connects)
        if(connects[name].adapter == adapter && connects[name].owner == owner) {
            $('#connection_list option[value="'+name+'"]').attr("selected","selected"); 
            load_login_form(name);
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
            "Login / Save": function() {
                adapter = $('#adapter_list option:checked').val();
                Password = $("#password").val();
                Password = (adapter == "imem" ? (is_MD5(Password) ? Password : MD5(Password)) : Password);
                var connectJson = {connect: {name      :$("#connection_list").parent().children()[0].value,
                                             ip        :$("#ip").val(),
                                             port      :$("#port").val(),
                                             service   :$("#service").val(),
                                             type      :$('input:radio[name=db_type]:checked').val(),
                                             user      :$("#user").val(),
                                             password  :Password,
                                             tnsstring :$("#tnsstring").val()}};
                owner = $("#user").val();
                var name = $('#connection_list option:checked').val();
                var Dlg = $(this);
                ajax_post('/app/connect', connectJson, null, null, function(data) {
                    if(data.connect == "ok") {
                        document.title = name;
                        var nm = name.replace(/\s/, '_');
                        //if($('#main-content-tabs #page_'+nm).length < 1) {
                        //    $('#main-content-tabs')
                        //    .append($('<div id="page_'+nm+'"></div>').addClass('sub-tabs'))
                        //    .tabs('add', '#page_'+nm, name);
                        //    tabIdx = 0;
                        //} else if($('#main-content-tabs #page_'+nm+tabIdx).length < 1) {
                        //    $('#main-content-tabs')
                        //    .append($('<div id="page_'+nm+tabIdx+'"></div>').addClass('sub-tabs'))
                        //    .tabs('add', '#page_'+nm+tabIdx, name);
                        //} else {
                        //    ++tabIdx;
                        //    $('#main-content-tabs')
                        //    .append($('<div id="page_'+nm+tabIdx+'"></div>').addClass('sub-tabs'))
                        //    .tabs('add', '#page_'+nm+tabIdx, name);
                        //}
                        //$('#main-content-tabs').data('curtab', $('#main-content-tabs').tabs('select', 0));
                        Dlg.dialog("close");
                        show_qry_files();
                    }
                    else {
                        alert(data.connect);
                    }
                });
            },
            // "Save": function() {
            //     name = $("#connection_list").parent().children()[0].value;
            //     saveSettings = {name      :name,
            //                     adapter   :$('#adapter_list option:checked').val(),
            //                     ip        :$('#ip').val(),
            //                     port      :$('#port').val(),
            //                     service   :$('#service').val(),
            //                     type      :$('input:radio[name=db_type]:checked').val(),
            //                     user      :$('#user').val(),
            //                     password  :MD5($('#password').val()),
            //                     tnsstring :$('#tnsstring').val()};
            //     connects[name] = saveSettings;
            //     $('<option value="'+name+'">'+name+'</option>').appendTo($('#connection_list'));
            //     load_login_form(name);

            //     ajax_post('/app/save', saveSettings, null, null, function(data) {
            //         alert(data.save);
            //     });
            // },
            "Delete": function() {
                delByName = $("#name").val();
                delete connects[delByName];
                $('#connection_list option[value="'+delByName+'"]').remove();
                var name = null;
                for(name in connects);
                if(null != name) load_login_form(name);

                ajax_post('/app/save', connects, null, null, function(data) {
                    alert(data.result);
                });
            }
        }
    })
    .dialog("open");
    $('#con_tns').hide();
    $('#con_othrs').show();

    load_connections();

    // Changes between Service / SID / TNS
    $("input[name='db_type']").change( function() {
      $('#con_tns').hide();
      $('#con_othrs').show();
      if($(this).val().toUpperCase() == 'TNS') {
        $('#con_tns').show();
        $('#con_othrs').hide();
      }
      $('#con_name').html($(this).val().toUpperCase() + "&nbsp;");
    });
}

function load_login_form(name) {
    if(connects.hasOwnProperty(name)) {
        $('#ip').val(connects[name].ip);
        $('#port').val(connects[name].port);
        $('#service').val(connects[name].service);
        $('#sid').val(connects[name].sid);
        $('#user').val(connects[name].user);
        $('#tnsstring').val(connects[name].tnsstring);
        $('input:radio[name=db_type][value='+connects[name].type+']').click();
        var conName = '';
        if(connects[name].hasOwnProperty('type')) {
            conName = connects[name].type.toUpperCase();
            if(conName == 'TNS') {
                $('#con_tns').show();
                $('#con_othrs').hide();
            }
        }
        $('#con_name').html(conName + "&nbsp;");
        $('#adapter_list option[value="'+connects[name].adapter+'"]').attr("selected","selected"); 
        $('#connection_list option[value="'+name+'"]').attr("selected","selected"); 
        $('#password').val("change_on_install"); // TODO remove after test
        $('#password').focus();
    }
    else {
        $('#ip').val('');
        $('#port').val('');
        $('#service').val('');
        $('#sid').val('');
        $('#user').val('');
        $('#password').val('');
        $('#tnsstring').val('');
    }
}
