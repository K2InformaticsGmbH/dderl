function prepareHeader(headers) {
    // Column Data
    var header = new Array();
    header[header.length] = {  id: "sel",
                             name: "#",
                            field: "id",
                         behavior: "select",
                         cssClass: "cell-selection",
                            width: 40,
              cannotTriggerInsert: true,
                        resizable: false,
                       selectable: false};
    for (i=0;i<headers.length;++i)
        header[header.length] = {  id: headers[i],
                                 name: headers[i],
                                field: headers[i],
                               editor: Slick.Editors.Text,
                             minWidth: 10 * headers[i].length,
                                width: 10 * headers[i].length,
                            resizable: true,
                           selectable: true};
    return header;
}

function loadRowsToDV(dlg, headers, tblRows, dataView) {
    var row_id = dlg.data("row_id");
    dataView.beginUpdate();
    for(i=0; i<tblRows.length; ++i) {
        var row = {id: ++row_id};
        for(j=0; j<tblRows[i].length; ++j)
            row[headers[j]] = tblRows[i][j];
        dataView.addItem(row);
    }
    dlg.data("row_id", row_id);
    dataView.endUpdate();
}

function render_table(tableNameList, query, owner)
{
    if(query.length == 0) return;
    var origTableName = tableNameList.join('_');
    var tableName = correct_table_name(origTableName);
    var dialogueId = "dialog-" + tableName;
    var row_id = 0;
    if(document.getElementById(dialogueId) != null) {
        if($('#'+dialogueId).data("query").toLowerCase() == query.toLowerCase()) {
            $('#'+dialogueId).dialog("moveToTop");
            return;
        } else {
            $('#'+dialogueId).dialog('destroy');
            $('#'+dialogueId).remove();
            dlgX = dlgY = 0;
        }
    }

    $('<div id=' + dialogueId + '/>').appendTo(document.body);
    if(dlgX == 0) dlgX = $('#db-tables-views').width();
    if(dlgY == 0) dlgY = $('#menubar').height();
    var qLink = $('<img id=state_'+tableName+' class="download_incomplete"/>&nbsp;'+
            '&nbsp;<a href="javascript:void(0)">'+owner+'.'+origTableName+'</a>'); //'@'+$(ip).val()+
    qLink.click(function() {
        edit_sql($('#'+dialogueId), query);
    });
    $('#'+dialogueId).dialog({
        autoOpen: false,
        height: 500,
        width: 500,
        modal: false,
        position: [dlgX, dlgY],
        title: qLink,
        close: function() {
            ajax_post('/app/stmt_close', {stmt_close: {statement: $('#'+dialogueId).data("statement")}}, null, null, null);
            $('#'+this.id).dialog('destroy');
            $('#'+this.id).remove();
            dlgX = dlgY = 0;
            loading(false, $('#'+dialogueId));
        },
        dragStop: function(event, ui) {
            loadingIndicator
                .css("top", $('#'+this.id).dialog("option", "position")[1] + $('#'+this.id).height() / 2 - loadingIndicator.height() / 2)
                .css("left", $('#'+this.id).dialog("option", "position")[0] + $('#'+this.id).width() / 2 - loadingIndicator.width() / 2);
        },
        drag: function(event, ui) {
            loadingIndicator
                .css("top", $('#'+this.id).dialog("option", "position")[1] + $('#'+this.id).height() / 2 - loadingIndicator.height() / 2)
                .css("left", $('#'+this.id).dialog("option", "position")[0] + $('#'+this.id).width() / 2 - loadingIndicator.width() / 2);
        }
    });
    $('#'+dialogueId).data("query", query);
    $('#'+dialogueId).bind('requery', function(e, sqlObj) {
        ajax_post('/app/stmt_close', {stmt_close: {statement: $('#'+this.id).data("statement")}}, null, null, null);
        $('#'+this.id).dialog('destroy');
        $('#'+this.id).remove();
        /*/
        ajax_post('/app/build_qry', {build_qry: JSON.stringify(sqlObj)}, null, this, function(data) {
            render_table(tableNameList, data.sql, owner)
        });
        /*/
        render_table(tableNameList, sqlObj, owner);
        //*/
    });

    var gDiv = $('<div id=qr'+tableName+' style="width: 100%; border: 1px solid rgb(128, 128, 128);"></div>').appendTo($('#'+dialogueId));
    var options = {editable: true,
               enableAddRow: false,
        enableColumnReorder: true,
       enableCellNavigation: true,
         asyncEditorLoading: false,
                   autoEdit: false,
                     zIndex: 1300};

    var dataView = new Slick.Data.DataView({ inlineFilters: false });
    var tblGrid = new Slick.Grid('#qr'+tableName, dataView, [{id:""}], options);
    var copyManager = new Slick.CellCopyManager();

    tblGrid.registerPlugin(copyManager);
    tblGrid.setSelectionModel(new Slick.CellRowColSelectionModel());

    dataView.onRowCountChanged.subscribe(function (e, args) {
      tblGrid.updateRowCount();
      tblGrid.render();
    });

    dataView.onRowsChanged.subscribe(function (e, args) {
      tblGrid.invalidateRows(args.rows);
      tblGrid.render();
    });

    tblGrid.onScroll.subscribe(function(e, args) {
        var gcP = args.grid.getCanvasNode().parentNode;
        //console.log(''+(gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop)));
        if (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop) < 0) {
            var dlg = $('#'+dialogueId);
            if(!dlg.data("finished"))
                ajax_post('/app/row', {row: {statement: dlg.data("statement")}}, null, this, function(data) {
                    var tblRows = data.rows;
                    if(tblRows.length > 0) {
                        dlg.data("finished", false);
                        $('#state_'+tableName).removeClass("download_complete");
                        $('#state_'+tableName).addClass("download_incomplete");
                        loadRowsToDV(dlg, dlg.data("headers"), tblRows, dataView);
                    } else {
                        dlg.data("finished", true);
                        $('#state_'+tableName).removeClass("download_incomplete");
                        $('#state_'+tableName).addClass("download_complete");
                    }
                });
        }
    });

    /////////////////
    //
    copyManager.onPasteCells.subscribe(function (e, args) {
        if (args.from.length !== 1 || args.to.length !== 1) {
            throw "This implementation only supports single range copy and paste operations";
        }

        var from = args.from[0];
        var to = args.to[0];
        var val;
        var columns = tblGrid.getColumns();
        var data = tblGrid.getData();
        for (var i = 0; i <= from.toRow - from.fromRow; i++) {
            for (var j = 0; j <= from.toCell - from.fromCell; j++) {
                if (i <= to.toRow - to.fromRow && j <= to.toCell - to.fromCell) {
                    val = data.getItem(from.fromRow + i)[columns[from.fromCell + j].name];
                    data.getItem(to.fromRow + i)[columns[to.fromCell + j].name] = val;
                    tblGrid.invalidateRow(to.fromRow + i);
                }
            }
        }
        tblGrid.render();
    });
    /////////////////

    $('#'+dialogueId).data("dataView", dataView)
                     .data("grid", tblGrid)
                     .data("gridDiv", gDiv);

    $('#'+dialogueId).dialog("open");
    $('#'+dialogueId).dialog("moveToTop");

    loading(true, $('#'+dialogueId));

    var sessionStr = (session != null ? '' + session : '');
    ajax_post('/app/query', {query: {qstr: query, table: tableName}}, null, null, function(table) {
        var table_id = 'qr'+table.table;
        var dlgId = "#dialog-"+table.table;
        var tblGrid = $(dlgId).data("grid");
        var statement = table.statement;
        $(dlgId).data("statement", statement);
        var columns = prepareHeader(table.headers);
        tblGrid.setColumns(columns);
        var columnpicker = new Slick.Controls.ColumnPicker(columns, tblGrid, options);

        ajax_post('/app/row', {row: {statement: statement}}, null,
            {headers: table.headers, dlgId: dlgId, table_id: table_id, table: table.table},
            function(_data) {
                var tblRows = _data.rows;
                if(tblRows.length > 0) {
                    var dlgId = this.dlgId;
                    var dataView = $(dlgId).data("dataView");
                    var table_id = this.table_id;
                    $(dlgId).data("row_id", 0);
                    loadRowsToDV($(dlgId), this.headers, tblRows, dataView);
                    
                    $(dlgId).data("headers", this.headers);
                    
                    $(dlgId).bind("dialogresize", function(event, ui) {
                            var tblGrid = $('#'+this.id).data("grid");
                            var gDiv = $('#'+this.id).data("gridDiv");
                            $(gDiv).height($('#'+this.id).height() - 10);
                            if(tblGrid != null) {
                                tblGrid.updateRowCount();
                                tblGrid.render();
                                tblGrid.resizeCanvas();
                            }
                    });
    //                var tblMetaStr = $(dlgId).dialog("option", "title")+' (columns '+this.headers.length+', rows '+tblRows.length+')';
    //                $(dlgId).dialog("option", "title", tblMetaStr);
                    loading(false, $(dlgId));
                    $('#'+this.table_id).height($(dlgId).height() - 10);
                    $(dlgId).data("grid").resizeCanvas();
                    dlgX += 15; dlgY += 25;
                } else {
                    $(dlgId).data("finished", true);
                    $('#state_'+this.table).removeClass("download_incomplete");
                    $('#state_'+this.table).addClass("download_complete");
                }
                if(tblRows.length < 150) {
                    $(dlgId).data("finished", true);
                    $('#state_'+this.table).removeClass("download_incomplete");
                    $('#state_'+this.table).addClass("download_complete");
                }
            });
    });
}
