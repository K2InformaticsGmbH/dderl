var logins = null;
var db_types = [{type: "oci", desc: "Oracle"}
               ,{type: "imem", desc: "Mnesia Cluster"}];
var adapter = null;
function display_login()
{
    if($('#login-button').html().indexOf('out') > 0) {
        $('#login-button').html('');
        $("#db-tables-views").html('');
        $('#change-pswd-button').data("logged_in_user", "");
        $('#login-msg').html('Welcome guest');
    }
    var dlg = $('<div id="dialog-login" title="Login to DDerl" style="diaply:none">'
     +'  <table border=0 width=100% cellpadding=0 cellspacing=0>'
     +'      <tr><td align=right valign=center>User&nbsp;</td>'
     +'          <td valign=bottom><input type="text" id="user_login" class="text ui-widget-content ui-corner-all"/></td></tr>'
     +'      <tr><td align=right valign=center>Password&nbsp;</td>'
     +'          <td valign=bottom><input type="password" id="password_login" class="text ui-widget-content ui-corner-all"/></td></tr>'
     +'  </table>'
     +'</div>')
    .appendTo(document.body);

    dlg.dialog({
        autoOpen: false,
        minHeight: 100,
        height: 'auto',
        width: 'auto',
        resizable: false,
        modal: true,
        closeOnEscape: false,
        dialogClass: 'no-close',
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        }
    })
    .dialog("open");

    $("#password_login").keypress(function(e) {
        if(e.which == 13) {
            var loginJson = {login: { user      :   $('#user_login').val(),
                                      password  :   MD5($('#password_login').val())}};
            ajax_post('/app/login', loginJson, null, null, function(data) {
                if(data.login == "ok") {
                    var user = $('#user_login').val();
                    $('#change-pswd-button').data("logged_in_user", user);
                    $('#login-button').html('Log out ' + user);
                    $("#dialog-login").dialog("close");
                    display_db_login();
                }
                else
                    alert('Login falied : ' + data.login);
            });        
        }
    });
}

function change_password()
{
    var loggedInUser = $('#change-pswd-button').data("logged_in_user");
    if(loggedInUser == undefined || loggedInUser.length == 0) {
        alert("Please login first!");
        return;
    }

    $('<div id="dialog-change-password" title="Change DDerl account password">' +
      '  <table border=0 width=100% height=85% cellpadding=0 cellspacing=0>' +
      '      <tr><td align=right valign=center>User&nbsp;</td>' +
      '          <td valign=center><b>'+loggedInUser+'</b></td></tr>' +
      '      <tr><td align=right valign=center>Password&nbsp;</td>' +
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
        modal: true,
        close: function() {
            $("#dialog-change-password").dialog('destroy');
            $("#dialog-change-password").remove();
        },
        buttons: {
            "Change Password": function() {
                if($('#conf_password_login').val() == $('#password_change_login').val()) {
                    var newPassJson = {change_pswd: { user      :loggedInUser,
                                                      password  :MD5($('#password_change_login').val())}};
                    ajax_post('/app/login_change_pswd', newPassJson, null, null, function(data) {
                        if(data.change_pswd == "ok")
                            $("#dialog-change-password").dialog("close");
                        else
                            alert('Change password falied : ' + data.change_pswd);
                    });
                }
                else alert("Confirm password missmatch!");
            },
            Cancel: function() {
                $(this).dialog("close");
            }
        }
    });
    $('#dialog-change-password').dialog("open");
}

function get_db_types_html()
{
    var html = '<select id="adapter_list" class="ui-widget-content ui-corner-all">';
    for(var i=0; i<db_types.length; ++i)
        html += '<option value='+db_types[i].type+'>'+db_types[i].desc+'</option>';
    html += '</option>';
    return html;
}

function load_nodes(elm)
{
    elm.show();
    ajax_post('/app/imem_nodes', null, null, null, function(data) {
        for(var i=0; i<data.nodes.length; ++i)
            $('<option value="'+data.nodes[i]+'">'+data.nodes[i]+'</option>').appendTo(elm);
    });
}

var tabIdx=0;
function display_db_login()
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
                        document.title = name + " DDerl 1.0";
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
                    }
                    else {
                        alert(data.msg);
                    }
                });
                $(this).dialog("close");
            },
            "Save": function() {
                name = $('#config_list option:checked').val();
                saveSettings = {adapter  :$('#adapter_list option:checked').val(),
                                ip       :$("#ip").val(),
                                port     :$("#port").val(),
                                service  :$("#service").val(),
                                type     :$('input:radio[name=db_type]:checked').val(),
                                user     :$("#user").val().toUpperCase(),
                                password :$("#password").val(),
                                tnsstring :$("#tnsstring").val()};
                logins[name] = saveSettings;
                $('<option value="'+name+'">'+name+'</option>').appendTo($('#config_list'));
                load_login_form(name);

                ajax_post('/app/save', logins, null, null, function(data) {
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
            },
            Cancel: function() {
                $(this).dialog("close");
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
        document.title = pageTitlePrefix + $(this).val();
        load_login_form($(this).val());
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
    $('#password').val(logins[name].password);
    $('#tnsstring').val(logins[name].tnsstring);
    $('input:radio[name=db_type][value='+logins[name].type+']').click();
    if(logins[name].type.toUpperCase() == 'TNS') {
        $('#con_tns').show();
        $('#con_othrs').hide();
    }
    $('#con_name').html(logins[name].type.toUpperCase() + "&nbsp;");
    $('#adapter_list option[value="'+logins[name].adapter+'"]').attr("selected","selected"); 
    $('#config_list option[value="'+name+'"]').attr("selected","selected"); 
}
