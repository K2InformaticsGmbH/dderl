var BUFFER_SIZE = 200;
var TOOLBAR_HEIGHT = 25;
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

function renderTable(ctx) {
    var tabName = ctx.name;
    var statement = ctx.statement;
    var columns = ctx.columns;
    var initFun = ctx.initFun;
    var destroyFun = ctx.destroyFun;
    var countFun = ctx.countFun;
    var rowFun = ctx.rowFun;

    var tableName = tabName.replace(/[ \.@]/g, '_');

    var width=500, height=500, position='center';
    if (ctx != null && ctx != undefined && ctx.hasOwnProperty('table_layout')) {
        var tablayout = ctx.table_layout;
        if (tablayout.hasOwnProperty('width') && tablayout.width > 0) width = tablayout.width;
        if (tablayout.hasOwnProperty('height') && tablayout.height > 0) height = tablayout.height;
        if (tablayout.hasOwnProperty('x') && tablayout.hasOwnProperty('y') && tablayout.x >= 0 && tablayout.y >= 0)
            position = [tablayout.x, tablayout.y];
    }

    // Initial cleanup -- can't open two instances of the same table
    $('#'+tableName+'_grid_row_context').remove();
    $('#'+tableName+'_grid_header_context').remove();
    $('#'+tableName+'_grid_title_context').remove();
    $('#'+tableName+'_dlg').remove();

    var dlg = $('<div id="'+tableName+'_dlg" style="margin:0; padding:0;"></div>').appendTo(document.body);

    var table = $('<div id="'+tableName+'_grid"></div>')
    .css('position', 'absolute')
    .css('top', '0')
    .css('left', '0')
    .css('right', '0')
    .css('bottom', TOOLBAR_HEIGHT+'px')
    .css('overflow', 'auto')
    .css('border-style', 'solid')
    .css('border-width', '1px')
    .css('border-color', 'lightblue')
    .appendTo(dlg);

    var footer = $('<div></div>')
    .css('height', TOOLBAR_HEIGHT+'px')
    .css('position', 'absolute')
    //.css('top', 'auto')
    .css('left', '0')
    .css('right', '0')
    .css('bottom', '0')
    .css('overflow', 'hidden')
    .appendTo(dlg);

    dlg.dialog({
        autoOpen: false,
        height: height,
        width: width,
        minHeight: 200,
        resizable: true,
        modal: false,
        title: tabName,
        position: position,
        canMinimize:true,
        canMaximize:true,
        closeOnEscape: false,
        focus: function(e,ui) {
            ctx.tblDlg = dlg;
            ctx.grid = table.data('grid');
            $('#tbl-opts').data('data', ctx);
            $('#tbl-opts').text(tabName + ' Options');
            $('#tbl-opts').show();
        },
        close: function() {
            $('#'+tableName+'_grid_row_context').remove();
            $('#'+tableName+'_grid_header_context').remove();
            $('#'+tableName+'_grid_title_context').remove();

            console.log('dlg close cancel tail timer ' + dlg.data('tail'));
            clearTimer(dlg);
            dlg.dialog('destroy');
            dlg.remove();
            if(destroyFun != null || destroyFun != undefined)
                destroyFun();
        }
    }).bind("dialogresize", function(event, ui) {
        table.width(dlg.width()-2);
        table.height(dlg.height()-TOOLBAR_HEIGHT-2);
        //footer.css('top', dlg.height()-TOOLBAR_HEIGHT-10);
        if(undefined != table.data('grid')) {
            var grid = table.data('grid');
            var data = grid.getData();
            grid.setData(data);
            grid.updateRowCount();
            grid.render();
            grid.resizeCanvas();
        }
    }).dialog("open");
  
    if(initFun != null || initFun != undefined)
        initFun(dlg);

    table.data("dlg", dlg);

    append_to_footer(footer, gen_btn_elm({ txt : 'Reload'                           // Refresh
                                         , icn : 'ui-icon-arrowrefresh-1-e'
                                         , clk : function() {
                                                     clearTimer(dlg);
                                                     dlg.dialog("close");
                                                     load_table(ctx);
                                                     return false;
                                                 }}));
    append_to_footer(footer, gen_btn_elm({ txt : 'Move to first'                    // |<
                                         , icn : 'ui-icon-seek-first'
                                         , clk : function() {
                                                     clearTimer(dlg);
                                                     rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, 1, OpsBufEnum.REPLACE);
                                                     return false;
                                                 }}));
    append_to_footer(footer, gen_btn_elm({ txt : 'Jump to previous page'            // <<
                                         , icn : 'ui-icon-seek-prev'
                                         , clk : function() {
                                                     clearTimer(dlg);
                                                     var d = table.data("grid").getData();
                                                     var rownum = table.data("grid").getViewport().top;
                                                     rownum = (d.length > rownum ? Math.floor(parseInt(d[rownum].id) / 2) : null);
                                                     rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, (rownum < 100 ? 1 : rownum), OpsBufEnum.REPLACE);
                                                     return false;
                                                 }}));
    var RowJumpTextBox = $('<input type="text" size=10 class="download_incomplete">')
        .keypress(function(evt)
            {
                clearTimer(dlg);
                if(evt.which == 13) {
                    var rownum = parseInt($(this).val());
                    if(rownum != NaN)
                        rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, rownum, OpsBufEnum.REPLACE);
                }
                return true;
            });
    table.data("finished", RowJumpTextBox);
    append_to_footer(footer, { elm : RowJumpTextBox                                 // RowJumpTextBox
                             , icn : null
                             , clk : function() { $(this).select(); }});
    append_to_footer(footer, gen_btn_elm({ txt : 'Next page'                        // >
                                         , icn : 'ui-icon-play'
                                         , clk : function() {
                                                     clearTimer(dlg);
                                                     var d = table.data("grid").getData();
                                                     var rownum = table.data("grid").getViewport().top;
                                                     rownum = (d.length > rownum ? parseInt(d[rownum].id) + 100 : null);
                                                     rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, rownum, OpsBufEnum.APPEND);
                                                     return false;
                                                 }}));
    append_to_footer(footer, gen_btn_elm({ txt : 'Jump to next page'                // >>
                                         , icn : 'ui-icon-seek-next'
                                         , clk : function() {
                                                     clearTimer(dlg);
                                                     var d = table.data("grid").getData();
                                                     var rownum = table.data("grid").getViewport().top;
                                                     rownum = (d.length > rownum ? 2 * parseInt(d[rownum].id) : null);
                                                     rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, (rownum < 200 ? 200 : rownum), OpsBufEnum.APPEND);
                                                     table.data("grid").scrollRowIntoView(d.length);
                                                     return false;
                                                 }}));
    append_to_footer(footer, gen_btn_elm({ txt : 'Move to end'                      // >|
                                         , icn : 'ui-icon-seek-end'
                                         , clk : function() {
                                                     ajax_post('/app/tail', {tail: {statement : statement, start: true}}, null, null, function(data) {});
                                                     table.data("grid").setData([]);
                                                     table.data("grid").updateRowCount();
                                                     table.data("grid").render();
                                                     dlg.data('tail', true);
                                                     rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, 1, OpsBufEnum.REPLACE);
                                                     console.log('tail register timer ' + dlg.data('tail'));
                                                     return false;
                                                 }}));
    append_to_footer(footer, gen_btn_elm({ txt : 'Commit changes'                   // Commit
                                         , icn : 'ui-icon-check'
                                         , clk : function() {
                                                     clearTimer(dlg);
                                                     var commitJson = {commit_rows: {statement : statement}};
                                                     ajax_post('/app/commit_rows', commitJson, null, null, function(data) {
                                                                 if(data.commit_rows == "ok") {
                                                                     console.log('commit success!');
                                                                 }
                                                                 else {
                                                                     alert('commit failed!\n' + data.commit_rows);
                                                                     console.log('commit failed!\n' + data.commit_rows);
                                                                 }
                                                             });
                                                     return false;
                                                 }}));
    append_to_footer(footer, gen_btn_elm({ txt : 'Discard changes'                  // Discard
                                         , icn : 'ui-icon-close'
                                         , clk : function() {
                                                     clearTimer(dlg);
                                                     return false;
                                                 }}));


    table.data("finished")
        .removeClass("download_incomplete")
        .removeClass("download_complete")
        .addClass("downloading");
    loadTable(table, statement, prepareColumns(columns));
    rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, 1, OpsBufEnum.APPEND);

    table.data("grid").onScroll.subscribe(function(e, args) {
        if(table.data("shouldScroll")) {
            var gcP = args.grid.getCanvasNode().parentNode;
            if(table.data('scrollTop') != undefined && table.data('scrollTop') != gcP.scrollTop) {
//console.log('scrollHeight '+gcP.scrollHeight + ' offsetHeight ' + gcP.offsetHeight + ' gcP.scrollTop ' + gcP.scrollTop);
                if (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop) <= 0) {
                    console.log('bottom_event cancel tail timer ' + table.data("dlg").data('tail'));
                    clearTimer(table.data("dlg"));
                    console.log('bottom_event '+ (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop)));
                    var d = args.grid.getData();
                    var rownum = args.grid.getViewport().bottom;
                    rownum = (d.length > rownum ? parseInt(d[rownum].id) + 1 : parseInt(d[d.length - 1].id) + 1);
                    rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.NEXT, rownum, OpsBufEnum.APPEND);
                }
                else if (gcP.scrollTop == 0) {
                    console.log('bottom_event cancel tail timer ' + table.data("dlg").data('tail'));
                    clearTimer(table.data("dlg"));
                    console.log('top_event '+ gcP.scrollTop);
                    var d = args.grid.getData();
                    var rownum = args.grid.getViewport().top;
                    rownum = (d.length > rownum ? parseInt(d[rownum].id) - 1 : parseInt(d[d.length - 1].id) - 1);
                    rowFunWrapper(countFun, rowFun, table, OpsFetchEnum.PREVIOUS, (rownum < 1 ? 1 : rownum),
                                  OpsBufEnum.PREPEND);
                }
            }
            else
                table.data('scrollTop', gcP.scrollTop);
        }
        else
            table.data("shouldScroll", true);
    });

    table.data("grid").onKeyDown.subscribe(function(e, args) {
        if(e.keyCode == 46) { // Delete
            // Delete args.row
            var deleteJson = {delete_row: {statement : statement,
                                          rowid      : args.row + 1}};
            ajax_post('/app/delete_row', deleteJson, null, null, function(data) {
                if(data.delete_row == "ok") {
                    grid_data = args.grid.getData();
                    grid_data.splice(args.row, 1);
                    args.grid.setData(grid_data);
                    args.grid.render();
                }
                else {
                    alert('delete failed');
                    console.log('delete failed');
                }
            });
        }
    });
}

function clearTimer(dlg)
{
    //clearInterval(dlg.data('tail'));
    dlg.data('tail', false);
}

function gen_btn_elm(itm) {
    itm['elm'] = $('<button>'+itm.txt+'</button>')
        .button({icons: {primary: itm.icn}, text: false})
        .click(itm.clk);
    return itm;
}

function append_to_footer(footer, itm) {
    footer.append(itm.elm);
}

function prepareColumns(headers) {
    // Column Data
    var header = new Array();
    header[header.length] = {  id: "sel",
                             name: "",
                            field: "id",
                         behavior: "select",
                         cssClass: "cell-selection",
                         minWidth: 35,
                            width: 35,
              cannotTriggerInsert: true,
                        resizable: true,
                         sortable: false,
                       selectable: false};
    for (i=0;i<headers.length;++i) {
        var fldid = headers[i];
        if(fldid == fldid.toLowerCase() && fldid == 'id')
            fldid = ('_'+fldid.toLowerCase());
        header[header.length] = {  id: fldid,
                                 name: headers[i],
                                field: fldid,
                               editor: Slick.Editors.Text,
                             minWidth: 20,
                                width: 10 * headers[i].length,
                            resizable: true,
                             sortable: false,
                           selectable: true};
    }
    return header;
}

function loadTable(table, statement, columns)
{
    var options = {editable: true,
               enableAddRow: true,
        enableColumnReorder: true,
       enableCellNavigation: true,
         asyncEditorLoading: false,
                   autoEdit: false,
                     zIndex: 1300,
                  rowHeight: 16};

    var node_id = '#' + table.attr('id');
    var grid = new Slick.Grid(node_id, [], columns, options);
    grid.setSelectionModel(new Slick.CellRowColSelectionModel());

    // Context Menus
    var row_cm_id = table.attr('id') + '_row_context';
    var header_cm_id = table.attr('id') + '_header_context';

    add_context_menu(row_cm_id, {
        'Browse Data'       : {evt: function(data) {
                ajax_post('/app/browse_data', {browse_data: { statement : statement,
                                                                    row : data.row,
                                                                    col : data.cell}}, null, null, function(ret) {
//                    var x = table.dialog('widget').position().left;
//                    var y = table.dialog('widget').position().top;
                    prepare_table(ret.browse_data);
                });
            }
        },
        'Quick condition'   : {evt: function() { alert('Quick condition'); } },
    });
    grid.onContextMenu.subscribe(function(e, args){
        e.preventDefault();
        $('#'+header_cm_id).hide();
        $('#'+row_cm_id).hide();
        var cell = grid.getCellFromEvent(e);
        var row = cell.row;
        var column = grid.getColumns()[cell.cell];
        var data = grid.getData()[cell.row][column.field];
        var R = new Slick.Range();
        R.toCell = R.fromCell = cell.cell;
        R.toRow = R.fromRow = cell.row;
        grid.getSelectionModel().setSelectedRanges([R]);
        grid.setActiveCell(row, cell.cell)
        var off = grid.getActiveCellPosition();
        var dlgPos = table.data("dlg").dialog('widget').position();
        if(off != null) {
            $('#'+row_cm_id)
                .data("data", cell)
                .css("top", off.top)
                .css("left", off.left + 15)
                .show(); 
        }
    });

    add_context_menu(header_cm_id, {
        'Browse Data'       : {evt: function() { alert('Browse Data'); } },
        'Quick condition'   : {evt: function() { alert('Quick condition'); } },
        'Hide Column'       : {evt: function(data) {
            var cols = data.grid.getColumns();
            cols.splice(cols.indexOf(data.column),1);
            data.grid.setColumns(cols);
        }}
    });
    grid.onHeaderContextMenu.subscribe(function(e, args){
        e.preventDefault();
        $('#'+header_cm_id).hide();
        $('#'+row_cm_id).hide();
        var dlgPos = table.data("dlg").dialog('widget').position();
        $('#'+header_cm_id)
            .data("data", args)
            .css("top", e.clientY - 10)
            .css("left", e.clientX)
            .show();
    });

    $(document.body).click(function () {
        $('#'+header_cm_id).hide();
        $('#'+row_cm_id).hide();
    });
    grid.onClick.subscribe(function(e, args){
        $('#'+header_cm_id).hide();
        $('#'+row_cm_id).hide();
    });

    table.data("grid", grid)
         .data("columns", columns);

    ctx = $('#tbl-opts').data('data');
    ctx.grid = grid;
    $('#tbl-opts').data('data', ctx);

    grid.onAddNewRow.subscribe(function (e, args) {
      var insertJson = {insert_data: {statement   : statement,
                                      col         : args.column.id,
                                      value       : args.item[args.column.id]}};
      ajax_post('/app/insert_data', insertJson, null, null, function(data) {
          if(isNaN(parseInt(data.insert_data))) {
              var msg = 'Insert failed ---------------------------------------------\n' +
                        'Row :   '+ args.item +
                        '\n---------------------------------------------------------';
              alert(msg);
              console.log(msg);
          }
          else {
              var id = parseInt(data.insert_data);
              var item = args.item;
              var data = grid.getData();
              item['id'] = id;
              grid.invalidateRow(data.length);
              data.push(item);
              grid.updateRowCount();
              grid.render();
          }
      });
    });
    
    grid.onCellChange.subscribe(function(e, args){
        var modifiedRow = grid.getData()[args.row];
        var cols = grid.getColumns();
        var updateJson = {update_data: {statement   : statement,
                                        rowid       : parseInt(modifiedRow.id),
                                        cellid      : args.cell,
                                        value       : modifiedRow[cols[args.cell].field]}};
        ajax_post('/app/update_data', updateJson, null, null, function(data) {
                    if(data.update_data != "ok") {
                        var msg = 'Update failed ---------------------------------------------\n' +
                                  'RowId :   '+ parseInt(modifiedRow.id) +
                                  '\nCell :  '+ args.cell +
                                  '\nValue : '+ modifiedRow[cols[args.cell].field] +
                                  '\nRow :   '+ modifiedRow +
                                  '\n---------------------------------------------------------';
                        alert(msg);
                        console.log(msg);
                    }
                    else {
                        console.log('update success ');
                    }
                });
    });
}

function add_context_menu(cm_id, options)
{
    var menu = $('<ul id="'+cm_id+'">')
                    .attr("id", cm_id)
                    .addClass("context_menu")
                    .hide()
                    .appendTo(document.body);

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

    $('#'+cm_id).click(function (e) {
        var action = $(e.target).attr("action");
        var data = $(this).data("data");
        evts[action](data);
      });
}

var rowStatusCheckInterval = 1000;
function rowFunWrapper(countFun, rowFun, table, opts, rowNum, loadFunOpts)
{
    function statusCheckFun() {
        console.log('status check tail '+(table.data('dlg') != undefined ? table.data('dlg').data('tail') : false));
        if(table.data("finished") != undefined)
            table.data("finished")
                .removeClass("downloading")
                .removeClass("download_incomplete")
                .addClass("download_complete");
        if(jQuery.isFunction(countFun)) {
            countFun(function(resp) {
                var isTail = (table.data('dlg') != undefined ? table.data('dlg').data('tail') : false);
                if(isTail == undefined) isTail = false;
                if(table.data("finished") != undefined)
                    table.data("finished").val(''+resp.count);
                if(isTail && table.data("grid") != undefined) {
                    var d = table.data("grid").getData();
                    var lastId = (d.length > 0 ? parseInt(d[d.length-1].id) : 0);
                    console.log('last row '+lastId+' cur buffer '+resp.count);
                    if (resp.count > lastId) {
                        var rowNum = ((resp.count - lastId) > 100 ? resp.count - 100 : lastId + 1);
                        console.log('fetching rows');
                        rowFun(OpsFetchEnum.NEXT, rowNum, loadRows, [table, rowNum, OpsBufEnum.APPEND]);
                    }
                }
                if(isTail)
                    table.data("finished")
                        .removeClass("download_incomplete")
                        .removeClass("download_complete")
                        .addClass("downloading");
                else if(!resp.finished)
                    table.data("finished")
                        .removeClass("downloading")
                        .removeClass("download_complete")
                        .addClass("download_incomplete");

                console.log('get_buffer_max finished '+resp.finished+' tail '+isTail);
                if(isTail)
                    setTimeout(statusCheckFun, rowStatusCheckInterval);
            });
        }
    };
    setTimeout(statusCheckFun, rowStatusCheckInterval);
    var isTail = (table.data('dlg') != undefined ? table.data('dlg').data('tail') : false);
    if(isTail == undefined) isTail = false;
    if(!isTail)
        rowFun(opts, rowNum, loadRows, [table, rowNum, loadFunOpts]);
}

var MAX_ROW_WIDTH = 600;
function loadRows(table, rowNum, ops, rowObj)
{
    var g       = table.data("grid");
    var dlg     = table.data("dlg");
    var d       = g.getData();
    var c       = g.getColumns();
    var rows    = rowObj.rows;
    var first   = (0 == d.length && false == dlg.data('tail'));

    if(ops == OpsBufEnum.REPLACE)
        d = new Array();

    var vBMin = (d.length > 0    ? parseInt(d[0].id) : 0);
    var vBMax = (d.length > 0    ? parseInt(d[d.length-1].id) : 0);
    var dBMin = (rows.length > 0 ? parseInt(rows[0][0]) : 0);
    var dBMax = (rows.length > 0 ? parseInt(rows[rows.length-1][0]) : 0);

//    console.log('Data Buf ('+ dBMin + ', ' + dBMax + ')');

    for (var i = 0; i < rows.length; i++) {
        var row = {};
        for(var j=1;j<c.length;++j) {
            row[c[j].field] = rows[i][j];
            var str = rows[i][j];
            var fieldWidth = $('#txtlen').text(str).width()+30;
            if(0 == d.length && c[j].width < fieldWidth) {
                c[j].width = fieldWidth;
                if (c[j].width > MAX_ROW_WIDTH)
                    c[j].width = MAX_ROW_WIDTH;
            }
        }
        if (0 == d.length)
            g.setColumns(c);

        row['id'] = rows[i][0];
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
            if(k >= d.length) {
                d[d.length] = row;
            }
        }
    }

    if (d.length > 2 * BUFFER_SIZE) {
        if(ops == OpsBufEnum.APPEND)    d.splice(0, d.length - BUFFER_SIZE);
        else                            d.splice(-(d.length - BUFFER_SIZE), d.length - BUFFER_SIZE);
    }

    g.setData(d);
    g.updateRowCount();
    g.render();
    var scrollIdx = 0;
    for(k=0;k<d.length;++k)
        if(parseInt(d[k].id) == rowNum) { scrollIdx = k; break; }
    if(scrollIdx == 0 && k >= d.length) scrollIdx = d.length - 1;
    table.data("shouldScroll", false);
    g.scrollRowIntoView(scrollIdx);
    
    if (first) {
        var rh = g.getOptions().rowHeight;
        var totHeight = (d.length + 5) * rh;
        var dlgTop = dlg.dialog('widget').position().top;
        totHeight = (totHeight > $(window).height() - dlgTop - 10 ? $(window).height() - dlgTop - 10 : totHeight);
        dlg.height(totHeight + 4);
        table.height(dlg.height() - TOOLBAR_HEIGHT);
        g.resizeCanvas();
    }

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

//    if(d.length > 0)
//        console.log('View Buf ('+ parseInt(d[0].id) + ', ' + parseInt(d[d.length-1].id) + ')');

    table.data("finished").val(rowObj.cache_max);
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
    setTimeout(
    function() {
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
    }, Math.floor((Math.random()*100)+1000));
}

var row_cnt=0;
function renderSampleTable(tableName)
{
    var ctx = 
    {
        name        : tableName,
        columns     : samplecolumns,
        initFun     : null,
        destroyFun  : null,
        countFun    : function(countUpdateFun) {
            ++row_cnt;
            setTimeout(
              countUpdateFun({count:row_cnt,finished:(row_cnt >= endRow ? true: false)})
            , Math.floor(Math.random()*10+1));
        },
        rowFun      : samplerows,
//        width       : 200,
//        height      : 200,
//        posX        : 10,
//        posY        : 10
    };
    renderTable(ctx);
}
///////////////////////// SAMPLE-TEST ///////////////////////////////////////
