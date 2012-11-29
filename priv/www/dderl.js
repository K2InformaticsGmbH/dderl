function show_logs()
{
    ajax_post('/app/logs', {}, null, null, function(data) {
        $('<div style="diaply:none" />')
        .append($('<select id="logs_list" class="ui-corner-all" size=100 style="width:100%; height:100%"/>')
                .dblclick(function(){
                    window.open($('#logs_list option:selected').val());
                })
        )
        .appendTo(document.body)
        .dialog({
            autoOpen: false,
            height: 400,
            width: 300,
            resizable: false,
            modal: true,
            title: "Logs",
            close: function() {
                $(this).dialog('destroy');
                $(this).remove();
            },
            buttons: {
                "Delete": function() {
                    var selLogItem = $('#logs_list option:selected');
                    ajax_post('/app/delete_log', {log:{file:selLogItem.val()}}, null, null, function(data) {selLogItem.remove();});
                }
            }
        })
        .dialog("open");

        for(var i=0;i<data.logs.length; ++i)
            $('<option value="'+data.logs[i]+'">'+data.logs[i]+'</option>').appendTo($('#logs_list'));
    });
}

var session = null;

function ajax_post(url, dataJson, headers, context, successFun) {
    if(headers == null) headers = {};
    if(context == null) context = {};
    if(dataJson == null) dataJson = JSON.stringify({});
    else dataJson = JSON.stringify(dataJson);

    headers["dderl_sess"] = (session != null ? '' + session : '');
    if (adapter != null)
        headers["adapter"] = adapter;

    $.ajax({
        type: 'POST',
        url: url,
        data: dataJson,
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: context,
        success: function(_data) {
            if(_data.hasOwnProperty('session'))
                session = _data.session;
            if(successFun != null)
                successFun.call(context, _data);
        }
    });
}

function show_qry_files()
{
    var tab = $('#main-content-tabs').data('curtab');
    if(tab != null || tab != undefined) {
        ajax_post('/app/files', {}, null, null, function(data) {
            $('<div id="dialog-show-files" title="Query Files" style="display:none"></div>')
            .append($('<select id="files_list" class="ui-corner-all" size=100 style="width:100%; height:100%"/>')
                    .dblclick(function() {
                        load_table($('#files_list option:selected').data("context"));
                    })
                   )
            .appendTo(tab);
            for(var i=0;i<data.files.length; ++i)
                $('<option value="'+data.files[i].content+'">'+data.files[i].name+'</option>')
                .appendTo($('#files_list'))
                .data("context", data.files[i]);
            $("#dialog-show-files").dialog({
                autoOpen: false,
                height: 300,
                width: 200,
                resizable: false,
                modal: false,
                close: function() {
                    $(this).dialog('destroy');
                    $(this).remove();
                },
                buttons: {
                    "Delete": function() {
                        var selFile = $('#files_list option:selected');
                        ajax_post('/app/del_file', {del: {file_name: selFile.text()}}, null, null, function(data) {selFile.remove();});
                    }
                }
            })
            .dialog("open");
        });
    }
}

function load_new_table(tableName)
{
    ajax_post('/app/get_query', {get_query: {table: tableName}}, null, null, function(data) {
        load_table(data.qry);
    });
}

function load_table(context)
{
    var query = context.content;
    var tableName = context.name;
    ajax_post('/app/query', {query: {qstr: query, id:context.id}}, null, null, function(table) {
        var statement = table.statement;

        context.columns = table.headers;
        context.initFun = function(tblDlg) {
            tblDlg.bind('requery', function(e, sqlObj) {
                ajax_post('/app/stmt_close', {stmt_close: {statement: statement, row_num: -1}},
                          null, null, null);
                tblDlg.dialog('destroy');
                tblDlg.remove();
                context.content = sqlObj;
                load_table(context);
            });
        };        
        context.destroyFun = function() {
            ajax_post('/app/stmt_close', {stmt_close: {statement: statement, row_num: -1}}, null, null, null);
        };
       
        context.countFun = function(countUpdateFun) {
            ajax_post('/app/get_buffer_max', {get_buffer_max: {statement: statement}},
                      null, null, countUpdateFun);
        };

        context.rowFun = function(opsfetch, rowNum, renderFun, renderFunArgs) {
            var Cmd = '/app/row';
            switch(opsfetch) {
                case OpsFetchEnum.NEXT:
                    Cmd += '_next';
                    break;
                case OpsFetchEnum.PREVIOUS:
                    Cmd += '_prev';
                    break;
                case OpsFetchEnum.TOEND:
                    Cmd += '_next';
                    rowNum = 10000000; // 10 mil for end of table for most table
                    break;
                default:
                    Cmd += '_next';
                    break;
            }
            if(rowNum == null)
                rowNum = -1;
            ajax_post(Cmd, {row: {statement: statement, row_num: rowNum}}, null, null,
            function(data) {
                renderFunArgs[renderFunArgs.length] = data;
                renderFun.apply(this, renderFunArgs);
            });
        };
        renderTable(context);
    });
}

function edit_table()
{
    context = $('#tbl-opts').data('data');
    edit_sql(context.tblDlg, context.content);
}

function save_table()
{
    context = $('#tbl-opts').data('data');
    qStr = context.content.replace(/(\r\n|\n|\r)/gm," ");
    undefinedTableIdx = 0;
    ajax_post("/app/save_file", {save: {file_name:context.name, file_content:qStr}}, null, null, null);
}

function save_as_table()
{
    context = $('#tbl-opts').data('data');
    qStr = context.content.replace(/(\r\n|\n|\r)/gm," ");
    undefinedTableIdx = 0;
    $('<div><input type="text" value="'+context.name+'"/></div>')
    .appendTo(document.body)
    .dialog({
        autoOpen: false,
        height: 105,
        width: 'auto',
        modal: true,
        resizable: false,
        title: "Save SQL as",
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        },
        buttons: {
            "Save": function() {
                var fileName = $(this).children('input').val();
                ajax_post("/app/save_file", {save: {file_name:fileName, file_content:qStr}}, null, null, null);
                $(this).dialog('close');
            }
        }
    }).dialog("open");
}

$(window).resize(function() {
    $('#db-tables-views').height($(window).height() - $('#menubar').height() - 2);
});

$(".grid-header .g-ui-icon").addClass("ui-state-default ui-corner-all");

var pageTitlePrefix = null;
$(document).ready(function() {
    if(Object.hasOwnProperty('freeze')) {
        $('#main-content-tabs')
        .tabs()
        .bind('tabsselect', function(event, ui) {
         ui.options // options used to intialize this widget
         ui.tab // anchor element of the selected (clicked) tab
         ui.panel // element, that contains the contents of the selected (clicked) tab
         ui.index // zero-based index of the selected (clicked) tab
         $('#main-content-tabs').data('curtab', ui.panel);
        })
        .hide();

        $( ".tabs-bottom .ui-tabs-nav, .tabs-bottom .ui-tabs-nav > *" )
	    		.removeClass( "ui-corner-all ui-corner-top" )
	    		.addClass( "ui-corner-bottom" );
        if(session == null) {
            if(null == pageTitlePrefix)
                pageTitlePrefix = document.title + " "; // IE can't trim()
                //pageTitlePrefix = document.title.trim() + " ";
            display_login();
        }
    } else {
        $('#main-menu-bar').hide();
        alert("Dinosours are extinct. Upgrade!");
    }
});
