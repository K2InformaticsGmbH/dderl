var tabIdx=0;
function connect_dlg()
{
    $('<div id="dialog-db-login" title="Connect to Database">' +
      '  <table border=0 width=100% height=85% cellpadding=0 cellspacing=0>' +
//      '      <tr><td align=right valign=center>Connections&nbsp;</td>' +
//      '          <td valign=center><select id="config_list" class="ui-widget-content ui-corner-all"/></td></tr>' +
      '      <tr><td align=right valign=center>DB Type&nbsp;</td>' +
      '          <td valign=bottom>'+get_db_types_html()+'</td></tr>' +
      '      <tr><td colspan=2><hr></td></tr>' +
      '      <tr><td align=right valign=center>Connection Name&nbsp;</td>' +
      '          <td valign=bottom>'+
      '<select id="config_list" class="ui-widget-content ui-corner-all"/>'+
      //'<input type="text" id="name" class="text ui-widget-content ui-corner-all"/>'+
      '</td></tr>' +
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
        buttons: {
            "Login": function() {
                adapter = $('#adapter_list option:checked').val();
                var connectJson = {connect: {ip        :$("#ip").val(),
                                             port      :$("#port").val(),
                                             service   :$("#service").val(),
                                             type      :$('input:radio[name=db_type]:checked').val(),
                                             user      :$("#user").val(),
                                             password  :$("#password").val(),
                                             tnsstring :$("#tnsstring").val()}};
                owner = $("#user").val();
                var name = $('#config_list option:checked').val();
                ajax_post('/app/connect', connectJson, null, null, function(data) {
                    if(data.connect) {
                        document.title = name;
                        var nm = name.replace(/\s/, '_');
                        if($('#main-content-tabs #page_'+nm).length < 1) {
                            $('#main-content-tabs')
                            .append($('<div id="page_'+nm+'"></div>').addClass('sub-tabs'))
                            .tabs('add', '#page_'+nm, name);
                            tabIdx = 0;
                        } else if($('#main-content-tabs #page_'+nm+tabIdx).length < 1) {
                            $('#main-content-tabs')
                            .append($('<div id="page_'+nm+tabIdx+'"></div>').addClass('sub-tabs'))
                            .tabs('add', '#page_'+nm+tabIdx, name);
                        } else {
                            ++tabIdx;
                            $('#main-content-tabs')
                            .append($('<div id="page_'+nm+tabIdx+'"></div>').addClass('sub-tabs'))
                            .tabs('add', '#page_'+nm+tabIdx, name);
                        }
                        $('#main-content-tabs').data('curtab', $('#main-content-tabs').tabs('select', 0));
                        show_qry_files();
                    }
                    else {
                        alert(data.msg);
                    }
                });
                $(this).dialog("close");
            },
            "Save": function() {
                name = $("#config_list").parent().children()[0].value;
                saveSettings = {adapter   :$('#adapter_list option:checked').val(),
                                name      :name,
                                ip        :$('#ip').val(),
                                port      :$('#port').val(),
                                service   :$('#service').val(),
                                type      :$('input:radio[name=db_type]:checked').val(),
                                user      :$('#user').val().toUpperCase(),
                                password  :$('#password').val(),
                                tnsstring :$('#tnsstring').val()};
                logins[name] = saveSettings;
                $('<option value="'+name+'">'+name+'</option>').appendTo($('#config_list'));
                load_login_form(name);

                ajax_post('/app/save', saveSettings, null, null, function(data) {
                    alert(data.save);
                });
            },
            "Delete": function() {
                delByName = $("#name").val();
                delete logins[delByName];
                $('#config_list option[value="'+delByName+'"]').remove();
                var name = null;
                for(name in logins);
                if(null != name) load_login_form(name);

                ajax_post('/app/save', logins, null, null, function(data) {
                    alert(data.result);
                });
            }
        }
    })
    .dialog("open");
    $('#con_tns').hide();
    $('#con_othrs').show();

    $("input[name='db_type']").change( function() {
      $('#con_tns').hide();
      $('#con_othrs').show();
      if($(this).val().toUpperCase() == 'TNS') {
        $('#con_tns').show();
        $('#con_othrs').hide();
      }
      $('#con_name').html($(this).val().toUpperCase() + "&nbsp;");
    });
    
    $('#config_list').html('');
    ajax_post('/app/get_connects', null, null, null, function(data) {
        logins = data;
        for(var name in logins)
            $('<option value="'+name+'">'+name+'</option>').appendTo($('#config_list'));
        for(var name in logins) {
            load_login_form(name);
            break;
        }
        $('#config_list').combobox();
    });
    $('#config_list').change(function() {
        var selectedName = $("#config_list").val();
        document.title = pageTitlePrefix + selectedName;
        load_login_form(selectedName);
    });
}

function load_login_form(name) {
    //$('#name').val(name);
    //$('#config_list').val(name);
    $('#ip').val(logins[name].ip);
    $('#port').val(logins[name].port);
    $('#service').val(logins[name].service);
    $('#sid').val(logins[name].sid);
    $('#user').val(logins[name].user);
    $('#password').val(logins[name].password);
    $('#tnsstring').val(logins[name].tnsstring);
    $('input:radio[name=db_type][value='+logins[name].type+']').click();
    var conName = '';
    if(logins[name].hasOwnProperty('type')) {
        conName = logins[name].type.toUpperCase();
        if(conName == 'TNS') {
            $('#con_tns').show();
            $('#con_othrs').hide();
        }
    }
    $('#con_name').html(conName + "&nbsp;");
    $('#adapter_list option[value="'+logins[name].adapter+'"]').attr("selected","selected"); 
    $('#config_list option[value="'+name+'"]').attr("selected","selected"); 
}
