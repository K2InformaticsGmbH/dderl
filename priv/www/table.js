var data_buffer_size = 20;

function renderTable(tableName, rowfun, columns) {
    var defHeight = 500;
    var dlg = $('<div id="'+tableName+'_dlg" style="margin:0; padding:0;"></div>').appendTo(document.body);
    var grid = $('<div id="'+tableName+'_grid" style="width:100%; height:'+(defHeight-47)+'px;"></div>')
                .appendTo($('<div style="border: 1px solid rgb(128, 128, 128); background:grey"></div>').appendTo(dlg));
    //$('<div style="background:green; bottom:0; height: 30px; width:95%;"></div>').appendTo(dlg);
    var title = '<table border=0 cellpadding=0 cellspacing=0><tr><td>'+tableName+'</td>'
              + '<td>&nbsp;&nbsp;&nbsp;&nbsp;</td>'
            //+ '<td></td>'
              +'</tr></table>';

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
        grid.height(dlg.height()-20);
        grid.width(dlg.width()-2);
        grid.data("grid").resizeCanvas();
    });

    dlg.dialog("open");

    dlg.parent().append('<div id="'+tableName+'_footer" style="position:absolute;bottom:0;width:96%;height:20px;">'
            +'</div>');

    $('<button onclick=\'nextRowfun($("#'+tableName+'_grid"),'+rowfun+')\' style="padding:0px;">Get More Data</button>').button({
            icons: { primary: "ui-icon-arrowreturnthick-1-s" }, text: false
    }).appendTo($('#'+tableName+'_footer'));
    $('<button onclick=\'nextRowfun($("#'+tableName+'_grid"),'+rowfun+')\' style="padding:4px;"></button>').button({
            icons: { primary: "ui-icon-triangle-1-e" }, text: false
    }).appendTo($('#'+tableName+'_footer'));
    $('<button onclick=\'nextRowfun($("#'+tableName+'_grid"),'+rowfun+')\' style="padding:4px;"></button>').button({
            icons: { primary: "ui-icon-arrowreturnthick-1-s" }, text: false
    }).appendTo($('#'+tableName+'_footer'));


    loadTable(grid, prepareColumns(columns));

    grid.data("grid").onScroll.subscribe(function(e, args) {
        var gcP = args.grid.getCanvasNode().parentNode;
        //console.log('scrollHeight '+gcP.scrollHeight + ' offsetHeight ' + gcP.offsetHeight + ' gcP.scrollTop ' + gcP.scrollTop);
        if (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop) <= 0) {
            console.log('bottom_event '+ (gcP.scrollHeight - (gcP.offsetHeight + gcP.scrollTop)));
            nextRowfun(grid, rowfun);
        }
        if (gcP.scrollTop == 0) {
            console.log('top_event '+ gcP.scrollTop);
            prevRowfun(grid, rowfun);
        }
    }
    );
}

function nextRowfun(table, rowfun)
{
     addNextRows(table, rowfun());
}

function prevRowfun(table, rowfun)
{
     addPrevRows(table, rowfun());
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

function loadTable(table, columns) {
    var options = {editable: true,
               enableAddRow: false,
        enableColumnReorder: true,
       enableCellNavigation: true,
         asyncEditorLoading: false,
                   autoEdit: false,
                     zIndex: 1300};

    var rows = [];
    var grid = new Slick.Grid('#' + table.attr('id'), rows, columns, options);
    grid.setSelectionModel(new Slick.CellRowColSelectionModel());
    grid.onContextMenu.subscribe(function(e, args){
        var cell = grid.getCellFromEvent(e);
        var row = cell.row;
        var column = grid.getColumns()[cell.cell]; // object containing name, field, id, etc
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
         .data("rows", rows)
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

var samplecolumns = [
  "Title",
  "Duration",
  "Duration1",
  "Duration2",
];

function samplerows()
{
    var rows = new Array();
    for (var i = 0; i < data_buffer_size; i++) {
        rows[i] = new Array();
        for(var j=0;j<samplecolumns.length;++j) {
            rows[i][j] = Math.floor((Math.random()*100)+1);
        }
    }
    return rows;
}

function addNextRows(table, rows)
{
  var d = table.data("rows");
  var c = table.data("columns");
  var idx = (d.length > 1 ? d[d.length-1][c[0].field] : 0);

  for (var i = 0; i < rows.length; i++) {
      var row = {};
      row[c[0].field] = idx + i + 1;
      for(var j=1;j<c.length;++j)
          row[c[j].field] = rows[i][j-1];
      d[d.length] = row;
  }

  if (d.length > 2 * data_buffer_size)
    d.splice(0, d.length - data_buffer_size);

  var ids=new Array;
  for(i=0;i<d.length;++i)ids[ids.length]=d[i].id;
  console.log('ids (' + ids.length + ') ' + ids);

  table.data("grid").updateRowCount();
  table.data("grid").render();    
}

function addPrevRows(table, rows)
{
  var d = table.data("rows");
  var c = table.data("columns");
  var idx = (d.length > 0 ? d[0][c[0].field] : 0);

  for (var i = 0; i < rows.length; i++) {
      var row = {};
      row[c[0].field] = idx - (i + 1);
      if(idx - (i + 1) < 0)
          break;
      for(var j=1;j<c.length;++j)
          row[c[j].field] = rows[i][j-1];
      d.splice(0, 0, row);
  }

  if (d.length > 2 * data_buffer_size)
    d.splice(-(d.length - data_buffer_size), d.length - data_buffer_size);

  var ids=new Array;
  for(i=0;i<d.length;++i)ids[ids.length]=d[i].id;
  console.log('ids (' + ids.length + ') ' + ids);

  table.data("grid").updateRowCount();
  table.data("grid").render();    
}
