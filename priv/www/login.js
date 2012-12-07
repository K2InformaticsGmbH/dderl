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
                    connect_dlg();
                    //display_db_login();
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
    }).dialog("open");
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
