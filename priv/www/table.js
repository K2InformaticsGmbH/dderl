var BUFFER_SIZE = 200;
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

function renderTable(tabName, columns, rowfun, editFun, width, height, position) {
    var tableName = tabName.replace(/[\.]/, '_');
    if (width    == undefined) width = 500;
    if (height   == undefined) height = 500;
    if (position == undefined) position = 'center';

    var dlg = $('<div id="'+tableName+'_dlg" style="margin:0; padding:0;"></div>').appendTo(document.body);
    var table = $('<div id="'+tableName+'_grid" style="width:100%; height:'+(height-47)+'px;"></div>')
                .appendTo($('<div style="border: 1px solid rgb(128, 128, 128); background:grey"></div>')
                .appendTo(dlg));
    var title = $('<a href=#>'+tabName+'</a>').click(function() {
        editFun(dlg);
    });
    
    dlg.dialog({
        autoOpen: false,
        height: height,
        width: width,
        resizable: true,
        modal: false,
        title: title,
        position: position,
        canMinimize:true,
        canMaximize:true,
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
    table.data("dlg", dlg);

    var statuslight = $('<img class="download_incomplete" style="border:none; position:relative; top:5px; left:0px;"/>');
    table.data("finished", statuslight);

    addFooter(dlg.parent(), tableName, table, statuslight, rowfun);

    loadTable(table, prepareColumns(columns));
    table.data("finished")
        .removeClass("download_incomplete")
        .removeClass("download_complete")
        .addClass("downloading");
    rowFunWrapper(rowfun, table, OpsFetchEnum.NEXT, null, loadRows, OpsBufEnum.APPEND);

    table.data("grid").onScroll.subscribe(function(e, args) {
        var gcP = args.grid.getCanvasNode().parentNode;
        //console.log('scrollHeight '+gcP.scrollHeight + ' offsetHeight ' + gcP.offsetHeight + ' gcP.scrollTop ' + gcP.scrollTop);
        if (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop) <= 0) {
            console.log('bottom_event '+ (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop)));
            rowFunWrapper(rowfun, table, OpsFetchEnum.NEXT, null, loadRows, OpsBufEnum.APPEND);
        }
        else if (gcP.scrollTop == 0) {
            console.log('top_event '+ gcP.scrollTop);
            rowFunWrapper(rowfun, table, OpsFetchEnum.PREVIOUS, null, loadRows, OpsBufEnum.PREPEND);
        }
    });
}

function rowFunWrapper(rowfun, table, opts, rowNum, loadFun, loadFunOpts)
{
    table.data("finished")
        .removeClass("download_incomplete")
        .removeClass("download_complete")
        .addClass("downloading");

    rowfun(opts, rowNum, loadFun, [table, loadFunOpts]);
}

function addFooter(parent_node, tableName, table, statuslight, rowfun)
{
    $('<div style="position:absolute;bottom:0;width:96%;height:27px;"></div>')
    .append(statuslight)
    .append($('<img style="border:none; width:5px;"/>'))
    .append($('<button>Reload</button>')
            .button({icons: { primary: "ui-icon-arrowrefresh-1-e" }, text: false})
            .click(function()
            {
                rowFunWrapper(rowfun, table, OpsFetchEnum.RELOAD, null, loadRows, OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<button>Move to first</button>')
            .button({icons: { primary: "ui-icon-seek-first" }, text: false})
            .click(function()
            {
                table.data("rowrange").startRow = 1;
                rowFunWrapper(rowfun, table, OpsFetchEnum.TOBEGIN, null, loadRows, OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<button>Jump to previous page</button>')
            .button({icons: { primary: "ui-icon-seek-prev" }, text: false})
            .click(function()
            {
                rowFunWrapper(rowfun, table, OpsFetchEnum.JUMPPREV, null, loadRows, OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<input type="text" size=10>')
            .keypress(function(evt)
            {
                if(evt.which == 13) {
                    var row = parseInt($(this).val());
                    if(row != NaN)
                        rowFunWrapper(rowfun, table, OpsFetchEnum.NEXT, row, loadRows, OpsBufEnum.REPLACE);
                }
                return true;
            })
           )
    .append($('<button>Next page</button>')
            .button({icons: { primary: "ui-icon-play" }, text: false})
            .click(function()
            {
                rowFunWrapper(rowfun, table, OpsFetchEnum.NEXT, null, loadRows, OpsBufEnum.APPEND);
                return false;
            })
           )
    .append($('<button>Jump to next page</button>')
            .button({icons: {primary: "ui-icon-seek-next" }, text: false})
            .click(function()
            {
                rowFunWrapper(rowfun, table, OpsFetchEnum.JUMPNEXT, null, loadRows, OpsBufEnum.REPLACE);
                return false;
            })
           )
    .append($('<button>Move to end</button>')
            .button({icons: {primary: "ui-icon-seek-end" }, text: false})
            .click(function()
            {
                rowFunWrapper(rowfun, table, OpsFetchEnum.TOEND, null, loadRows, OpsBufEnum.REPLACE);
                return false;
            })
           )
    .appendTo(parent_node);
}

function prepareColumns(headers) {
    // Column Data
    var header = new Array();
    header[header.length] = {  id: "sel",
                             name: "",
                            field: "id",
                         behavior: "select",
                         cssClass: "cell-selection",
                         maxWidth: 35,
                         minWidth: 35,
                            width: 35,
              cannotTriggerInsert: true,
                        resizable: true,
                         sortable: false,
                       selectable: false};
    for (i=0;i<headers.length;++i)
        header[header.length] = {  id: headers[i].toLowerCase(),
                                 name: headers[i],
                                field: headers[i].toLowerCase(),
                               editor: Slick.Editors.Text,
                             minWidth: 10 * headers[i].length,
                                width: 10 * headers[i].length,
                            resizable: true,
                             sortable: false,
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
                  rowHeight: 15,
            forceFitColumns: true };

    var node_id = '#' + table.attr('id');
    var grid = new Slick.Grid(node_id, [], columns, options);
    grid.setSelectionModel(new Slick.CellRowColSelectionModel());
    grid.onContextMenu.subscribe(function(e, args){
        var cell = grid.getCellFromEvent(e);
        var row = cell.row;
        var column = grid.getColumns()[cell.cell];
        var data = grid.getData()[cell.row][column.field];
        var R = new Slick.Range();
        R.toCell = R.fromCell = cell.cell;
        R.toRow = R.fromRow = cell.row;
        grid.getSelectionModel().setSelectedRanges([R]);
        grid.setActiveCell(row, cell.cell)
        e.preventDefault();
        var off = grid.getActiveCellPosition();
        var dlgPos = table.data("dlg").dialog('widget').position();
        if(off != null) {
            $(node_id+'_cm')
                .data("data", data)
                .css("top", off.top - dlgPos.top - 25)
                .css("left", off.left - dlgPos.left + 15)
                .show();

            $("body").one("click", function () {
              $(node_id+'_cm').hide();
            });
        }
    });

    add_context_menu($(node_id), {
        'Browse Data'       : {evt: function(data) {
            load_new_table(data); } 
        },
        'Quick condition'   : {evt: function() { alert('Quick condition'); } },
        'Hide Column'       : {evt: function() { alert('Hide Column'); } },
    });

    table.data("grid", grid)
         .data("columns", columns);
}

function add_context_menu(node, options)
{
    var cm_id = node.attr('id')+'_cm';
    var menu = $('<ul id="'+cm_id+'">')
                    .attr("id", cm_id)
                    .addClass("context_menu")
                    .hide()
                    .appendTo(node);

    var evts = new Object();
    for(var id in options) {
        var name = id.toLowerCase();
        evts[name] = (options[id].hasOwnProperty('evt') ? options[id].evt : function(){});
        var clsname = (options[id].hasOwnProperty('cls') ? options[id].cls.toLowerCase() : 'default'); 
        $('<li>')
            .attr("action", name)
            .text(id)
            .appendTo(menu);
    }

    node.data("evts", evts);
    $('#'+cm_id).click(function (e) {
        var action = $(e.target).attr("action");
        var data = $(this).data("data");
        evts[action](data);
      });
}

function loadRows(table, ops, rowObj)
{
    var d = table.data("grid").getData();
    var c = table.data("columns");
    var rows = rowObj.rows;

    if(ops == OpsBufEnum.REPLACE)
        d = new Array();

    var vBMin = (d.length > 0    ? parseInt(d[0].id) : 0);
    var vBMax = (d.length > 0    ? parseInt(d[d.length-1].id) : 0);
    var dBMin = (rows.length > 0 ? parseInt(rows[0][c.length-1]) : 0);
    var dBMax = (rows.length > 0 ? parseInt(rows[rows.length-1][c.length-1]) : 0);

    console.log('Data Buf ('+ dBMin + ', ' + dBMax + ')');

    for (var i = 0; i < rows.length; i++) {
        var row = {};
        for(var j=1;j<c.length;++j)
            row[c[j].field] = rows[i][j-1];
        row['id'] = rows[i][j-1];
        var k = 0;
        for(k=0;k<d.length;++k)
            if(d[k].id == row.id) {
                d[k] = row;
                break;
            }
        if (dBMin <= vBMin && dBMax <= vBMax) { // prepend with head replace
            if(k >= d.length) {
                if(i > 0) {
                    for(k=0;k<d.length;++k)
                        if(d[k].id == rows[i-1][c.length-1]) {
                            d.splice(k+1, 0, row);
                            break;
                        }
                } else
                    d.splice(0, 0, row);
            }
        }
        else if (dBMin >= vBMin && dBMax >= vBMax) { // tail replace then append
            if(k >= d.length)
                d[d.length] = row;
        }
    }

    if (d.length > 2 * BUFFER_SIZE) {
        if(ops == OpsBufEnum.APPEND)    d.splice(0, d.length - BUFFER_SIZE);
        else                            d.splice(-(d.length - BUFFER_SIZE), d.length - BUFFER_SIZE);
    }

    table.data("grid").setData(d);
    table.data("grid").updateRowCount();
    table.data("grid").render();
//    if(ops == OpsBufEnum.PREPEND && d[0].id > 1) table.data("grid").scrollRowIntoView(d.length-1);
    
    if(rowObj.done)
        table.data("finished")
            .removeClass("downloading")
            .removeClass("download_incomplete")
            .addClass("download_complete");
    else
        table.data("finished")
            .removeClass("downloading")
            .removeClass("download_complete")
            .addClass("download_incomplete");

    if(d.length > 0) console.log('View Buf ('+ parseInt(d[0].id) + ', ' + parseInt(d[d.length-1].id) + ')');
}

///////////////////////// SAMPLE-TEST ///////////////////////////////////////
var samplecolumns = [
  "Column 1",
  "Column 2",
  "Column 3",
  "Column 4",
  "Column 5",
  "Column 6",
  "Column 7",
  "Column 8"
];

var samplemaxrows = 1000;

var startRow = 0;
var endRow = 0;
var sample_buf_sz = BUFFER_SIZE / 2;
function samplerows(opsfetch, rowNum, renderFun, renderFunArgs)
{
    var rows = new Array();
    var rowcount = sample_buf_sz;
    var ret = null;
    var startIndex = endRow + 1;
    if(rowNum != null) startIndex = rowNum;
    if(opsfetch == OpsFetchEnum.PREVIOUS)
        startIndex = startRow - 1;
    if(startIndex < 0) startIndex = 0;

    if(opsfetch == OpsFetchEnum.NEXT) {
        if(startIndex + sample_buf_sz > samplemaxrows && startIndex < samplemaxrows)
            rowcount = samplemaxrows - startIndex + 1;
        else if(startIndex >= samplemaxrows) {
            ret = {done: true, rows: rows};
            renderFunArgs[renderFunArgs.length] = ret;
            renderFun.apply(this, renderFunArgs);
            return;
        }
    }
    if(opsfetch != OpsFetchEnum.NEXT && startIndex <= 1) {
        ret = {done: false, rows: rows};
        renderFunArgs[renderFunArgs.length] = ret;
        renderFun.apply(this, renderFunArgs);
        return;
    }
    for (var i = 0; i < rowcount; i++) {
        rows[i] = new Array();
        for(var j=0;j<samplecolumns.length;++j)
            rows[i][j] = (opsfetch == OpsFetchEnum.NEXT ? startIndex + i : startIndex - (rowcount - i - 1));
        rows[i][j] = (opsfetch == OpsFetchEnum.NEXT ? startIndex + i : startIndex - (rowcount - i - 1));
        if(opsfetch == OpsFetchEnum.PREVIOUS && startIndex - i - 1 < 0)
            break;
    }
    startRow = (opsfetch == OpsFetchEnum.NEXT ? startIndex : startIndex - rowcount + 1);
    endRow = (opsfetch == OpsFetchEnum.NEXT ? startIndex + rowcount - 1 : startIndex);
    ret = {done: false, rows: rows};
    renderFunArgs[renderFunArgs.length] = ret;
    renderFun.apply(this, renderFunArgs);
}

function renderSampleTable(tableName)
{
    //renderTable(tableName, samplecolumns, samplerows, null, 400, 400, [10,10]);
    renderTable(tableName, samplecolumns, samplerows, null);
}
///////////////////////// SAMPLE-TEST ///////////////////////////////////////
