var BUFFER_SIZE = 100;
var OpsBufEnum = Object.freeze({
                                 APPEND  : 1
                               , PREPEND : 2
                               , REPLACE : 3
                               });

var OpsFetchEnum = Object.freeze({
                                   NEXT     :1
                                 , PREVIOUS :2
                                 , JUMPNEXT :3
                                 , JUMPPREV :4
                                 , TOEND    :5
                                 , TOBEGIN  :6
                                 , RELOAD   :7
                                 });

var samplecolumns = [
  "Column 1",
  "Column 2",
  "Column 3",
  "Column 4",
  "Column 5",
];

var samplemaxrows = 1000;

function samplerows(opsfetch, startRow)
{
    var rows = new Array();
    var rowcount = BUFFER_SIZE;
    if(opsfetch == OpsFetchEnum.NEXT) {
        if(startRow + BUFFER_SIZE > samplemaxrows && startRow < samplemaxrows)
            rowcount = samplemaxrows - startRow + 1;
        else if(startRow == samplemaxrows)
            return {finished: true, startRow: startRow, rows: rows};
    }
    if(opsfetch != OpsFetchEnum.NEXT && startRow <= 1)
        return {finished: false, startRow: startRow, rows: rows};
    for (var i = 0; i < rowcount; i++) {
        rows[i] = new Array();
        for(var j=0;j<samplecolumns.length;++j)
            rows[i][j] = (opsfetch == OpsFetchEnum.NEXT ? startRow + i : startRow - i);
    }
    return {finished: false, startRow: startRow, rows: rows};
}

function renderSampleTable(tableName)
{
    renderTable(tableName, samplerows, samplecolumns);
}

function renderTable(tableName, rowfun, columns) {
    var defHeight = 500;
    var dlg = $('<div id="'+tableName+'_dlg" style="margin:0; padding:0;"></div>').appendTo(document.body);
    var table = $('<div id="'+tableName+'_grid" style="width:100%; height:'+(defHeight-50)+'px;"></div>')
                .appendTo($('<div style="border: 1px solid rgb(128, 128, 128); background:grey"></div>')
                .appendTo(dlg));
    var title = $('<img id="state_'+tableName+'" class="download_incomplete"/>&nbsp;&nbsp;<a href=#>'+tableName+'</a>');

    dlg.dialog({
        autoOpen: false,
        height: defHeight,
        width: 500,
        resizable: true,
        modal: false,
        title: title,
        close: function() {
            dlg.dialog('destroy');
            dlg.remove();
        },
    }).bind("dialogresize", function(event, ui) {
        table.height(dlg.height()-27)
             .width(dlg.width()-2)
             .data("grid").resizeCanvas();
    });

    dlg.dialog("open");

    table.data("rowrange", {startRow : 1, endRow : 1});
    table.data("finished", $('#state_'+tableName));

    $('<div style="position:absolute;bottom:0;width:96%;height:27px;"></div>')
    .append($('<button>Reload</button>')
            .button({icons: { primary: "ui-icon-arrowrefresh-1-e" }, text: false})
            .click(function()
            {
                loadRows(table, rowfun(OpsFetchEnum.RELOAD, table.data("rowrange").endRow), OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<button>Move to first</button>')
            .button({icons: { primary: "ui-icon-seek-first" }, text: false})
            .click(function()
            {
                table.data("rowrange").startRow = 1;
                loadRows(table, rowfun(OpsFetchEnum.TOBEGIN, table.data("rowrange").startRow), OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<button>Jump to previous page</button>')
            .button({icons: { primary: "ui-icon-seek-prev" }, text: false})
            .click(function()
            {
                loadRows(table, rowfun(OpsFetchEnum.JUMPPREV, table.data("rowrange").startRow), OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<input id="'+tableName+'_rownum" type="text" size=10>'))
    .append($('<button>Next page</button>')
            .button({icons: { primary: "ui-icon-play" }, text: false})
            .click(function()
            {
                var row = $('#'+tableName+'_rownum').val();
                var rowRange = table.data("rowrange");
                var op = OpsBufEnum.APPEND;        
                if(row.length > 0 && parseInt(row) != NaN) {
                    op = OpsBufEnum.REPLACE;
                    rowRange.endRow = parseInt(row);
                }
                loadRows(table, rowfun(OpsFetchEnum.NEXT, rowRange.endRow), op);
                return false;
            })
           )
    .append($('<button>Jump to next page</button>')
            .button({icons: { primary: "ui-icon-seek-next" }, text: false})
            .click(function()
            {
                loadRows(table, rowfun(OpsFetchEnum.JUMPNEXT, table.data("rowrange").endRow), OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<button>Move to end</button>')
            .button({icons: { primary: "ui-icon-seek-end" }, text: false})
            .click(function()
            {
                table.data("rowrange").startRow = 1;
                loadRows(table, rowfun(OpsFetchEnum.TOEND, table.data("rowrange").endRow), OpsBufEnum.REPLACE);
                return false;
            })
           )
    .appendTo(dlg.parent());

    loadTable(table, prepareColumns(columns));

    table.data("grid").onScroll.subscribe(function(e, args) {
        var gcP = args.grid.getCanvasNode().parentNode;
        //console.log('scrollHeight '+gcP.scrollHeight + ' offsetHeight ' + gcP.offsetHeight + ' gcP.scrollTop ' + gcP.scrollTop);
        if (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop) <= 0) {
            //console.log('bottom_event '+ (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop)));
            var rowRange = table.data("rowrange");
            loadRows(table, rowfun(OpsFetchEnum.NEXT, rowRange.endRow),  OpsBufEnum.APPEND);
        }
        if (gcP.scrollTop == 0) {
            //console.log('top_event '+ gcP.scrollTop);
            var rowRange = table.data("rowrange");
            loadRows(table, rowfun(OpsFetchEnum.PREVIOUS, rowRange.startRow), OpsBufEnum.PREPEND);
        }
    });
}

function prepareColumns(headers) {
    // Column Data
    var header = new Array();
    header[header.length] = {  id: "sel",
                             name: "",
                            field: "id",
                         behavior: "select",
                         cssClass: "cell-selection",
                            width: 40,
              cannotTriggerInsert: true,
                        resizable: true,
                       selectable: false};
    for (i=0;i<headers.length;++i)
        header[header.length] = {  id: headers[i].toLowerCase(),
                                 name: headers[i],
                                field: headers[i].toLowerCase(),
                               editor: Slick.Editors.Text,
                             minWidth: 10 * headers[i].length,
                                width: 10 * headers[i].length,
                            resizable: true,
                           selectable: true};
    return header;
}

function loadTable(table, columns)
{
    var options = {editable: true,
               enableAddRow: false,
        enableColumnReorder: true,
       enableCellNavigation: true,
         asyncEditorLoading: false,
                   autoEdit: false,
                     zIndex: 1300,
                  rowHeight: 15};

    var grid = new Slick.Grid('#' + table.attr('id'), [], columns, options);
    grid.setSelectionModel(new Slick.CellRowColSelectionModel());
    grid.onContextMenu.subscribe(function(e, args){
        var cell = grid.getCellFromEvent(e);
        var row = cell.row;
        var column = grid.getColumns()[cell.cell];
        grid.setActiveCell(row, cell);
        var data = table.data("rows")[cell.row][column.field];
    });

    add_context_menu($('#' + table.attr('id')), {
        'Browse Data'       : {evt: function() { alert('Add'); } },
        'Quick condition'   : {evt: function() { alert('Add'); } },
        'Hide Column'       : {evt: function() { alert('Add'); } },
    }
    );

    table.data("grid", grid)
         .data("columns", columns);
}

function add_context_menu(node, options)
{
    var cm_id = node.id+'_cm';
    var menu = $('<ul>')
                    .attr("id", cm_id)
                    .addClass("contextMenu")
                    .appendTo(node);

    var evts = new Object();
    for(var id in options) {
        var name = id.toLowerCase();
        evts[name] = (options[id].hasOwnProperty('evt') ? options[id].evt : function(){});
        var clsname = (options[id].hasOwnProperty('cls') ? options[id].cls.toLowerCase() : 'default'); 
        $('<li>')
            .addClass(clsname)
            .append($('<a>')
                        .attr("href", '#'+name)
                        .text(id)
                   ).appendTo(menu);
    }

    node.data("evts", evts)
        .contextMenu(
            { menu: cm_id
            , xShift: -385
            , yShift: -100
            },
            function(action, el, pos) {
                $(el).data("evts")[action]($(el));
            }
    );
}

function loadRows(table, rowObj, ops)
{
    var d = table.data("grid").getData();
    var c = table.data("columns");
    var r = table.data("rowrange");
    var rows = rowObj.rows;

    r.startRow = rowObj.startRow;
    var idx = (ops != OpsBufEnum.PREPEND ? r.endRow : r.startRow);
    
    if(ops == OpsBufEnum.REPLACE)
        d = new Array();

    for (var i = 0; i < rows.length; i++) {
        var row = {};
        row['id'] = (ops != OpsBufEnum.PREPEND ? idx + i : idx - i);

        if(ops == OpsBufEnum.PREPEND && idx - i <= 0) break;
        
        for(var j=1;j<c.length;++j)
            row[c[j].field] = rows[i][j-1];
        switch(ops) {
            case OpsBufEnum.APPEND:    d[d.length] = row;      break;
            case OpsBufEnum.REPLACE:   d[i] = row;             break;
            default:                d.splice(0, 0, row);    break;
        }
    }

    if (d.length > 2 * BUFFER_SIZE) {
        if(ops == OpsBufEnum.APPEND)    d.splice(0, d.length - BUFFER_SIZE);
        else                         d.splice(-(d.length - BUFFER_SIZE), d.length - BUFFER_SIZE);
    }

    table.data("grid").setData(d);
    table.data("grid").updateRowCount();
    table.data("grid").render();
    if(ops == OpsBufEnum.PREPEND && d[0].id > 1) table.data("grid").scrollRowIntoView(d.length-1);
    
    if(rowObj.finished)
        table.data("finished").removeClass("download_incomplete").addClass("download_complete");
    else
        table.data("finished").removeClass("download_complete").addClass("download_incomplete");

    table.data("rowrange", {startRow : d[0].id, endRow :d[d.length-1].id});
}
