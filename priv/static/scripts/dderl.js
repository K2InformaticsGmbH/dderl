var OpsBufEnum = { APPEND  : 1
                 , PREPEND : 2
                 , REPLACE : 3
                 };

var OpsFetchEnum = { NEXT     :1
                   , PREVIOUS :2
                   , JUMPNEXT :3
                   , JUMPPREV :4
                   , TOEND    :5
                   , TOBEGIN  :6
                   , RELOAD   :7
                   };

if(Object.hasOwnProperty('freeze')) {
    Object.freeze(OpsBufEnum);
    Object.freeze(OpsFetchEnum);
}

String.prototype.visualLength = function()
{
    var ruler = $('#txtlen');
    ruler.html(''+this);
    return ruler.width();
}

function getUniqueTime() {
  var time = new Date().getTime();
  while (time == new Date().getTime());
  return new Date().getTime();
}

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
        success: function(_data, textStatus, request)
        {            
            console.log('dderl session '+JSON.stringify(request.getResponseHeader('dderl_sess')));
            //if(_data.hasOwnProperty('session'))
            //    session = _data.session;
            var s = request.getResponseHeader('dderl_sess');
            if(s != null)
                session = s;
            if(successFun != null)
                successFun.call(context, _data);
        }
//        success: function(_data) {
//            if(_data.hasOwnProperty('session'))
//                session = _data.session;
//            if(successFun != null)
//                successFun.call(context, _data);
//        }
    });
}

function show_qry_files(conn)
{
    $('<div>')
    .appendTo(document.body)
    .table({
        autoOpen    : false,
        dderlConn   : conn,
        dderlSession: session,
        dderlAdapter: adapter,
    })
    .table('loadViews')
    .table('open');
}

function alert_jq(string)
{
    var dlgDiv =
        $('<div id="dialog-message" title="DDerl message"></div>')
        .appendTo(document.body)
        .append('<p><span class="ui-icon ui-icon-info" style="float: left; margin: 0 7px 50px 0;"></span>'+string+'</p>')
        .dialog({
            modal: true,
            width: 300,
            height: 300,
            buttons: {
              Ok: function() {
                $( this ).dialog( "close" );
              }
            },
            close: function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        });
}

function prepare_table(context)
{
    if(context.hasOwnProperty('error')) {
        alert_jq(table.error);
        console.log(table.error);
        return;
    }
    context.initFun = function(tblDlg) {
        tblDlg.bind('requery', function(e, sqlObj) {
            ajax_post('/app/stmt_close', {stmt_close: {statement: context.statement, row_num: -1}},
                      null, null, function(data) {if (data.hasOwnProperty('error')) { alert_jq(data.error); } });
            tblDlg.dialog('destroy');
            tblDlg.remove();
            context.content = sqlObj;
            load_table(context);
        });
    };
    context.destroyFun = function() {
        ajax_post('/app/stmt_close', {stmt_close: {statement: context.statement, row_num: -1}},
                      null, null, function(data) {if (data.hasOwnProperty('error')) { alert_jq(data.error); } });
    };

    context.countFun = function(countUpdateFun) {
        ajax_post('/app/get_buffer_max', {get_buffer_max: {statement: context.statement}},
                  null, null, function(data) { countUpdateFun(data.get_buffer_max); } );
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
        ajax_post(Cmd, {row: {statement: context.statement, row_num: rowNum}}, null, null,
        function(data) {
            renderFunArgs[renderFunArgs.length] = data;
            renderFun.apply(this, renderFunArgs);
        });
    };
    renderTable(context);
}

function load_table(context)
{
    var query = context.content;
    ajax_post('/app/query', {query: {qstr: query, id: context.id}}, null, null, function(table) {
        if(table.hasOwnProperty('error')) {
            alert_jq(table.error);
            return;
        }
        var statement = table.statement;
        context.columns = table.columns;
        context.statement = statement;
        prepare_table(context);
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
    var colnamesizes = new Array();
    var cols = context.grid.getColumns();
    // Column names and width
    for(var idx = 0; idx < cols.length; ++idx)
        if(cols[idx].name.length > 0)
            colnamesizes[colnamesizes.length] = {name: cols[idx].name, width: cols[idx].width};
    // Table width/height/position
    var w = context.tblDlg.width();
    var h = context.tblDlg.height();
    var x = context.tblDlg.dialog('widget').position().left;
    var y = context.tblDlg.dialog('widget').position().top;
    var saveView = {save_view : {table_layout : {width : w,
                                                height : h,
                                                     y : y,
                                                     x : x},
                                column_layout : colnamesizes,
                                         name : context.name,
                                      content : qStr}
                   };
    ajax_post("/app/save_view", saveView, null, null, function(data) {
        if (data.save_view != "ok") {
            alert_jq(data.save_view);
        }
    });
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

$(".grid-header .g-ui-icon").addClass("ui-state-default ui-corner-all");
