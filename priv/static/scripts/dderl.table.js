(function( $ ) {
  $.widget( "dderl.table", $.ui.dialog, {

    _dlg            : null,
    _table          : null,
    _footer         : null,
    _grid           : null,
    _gdata          : [],
    _txtlen         : null,
    _fnt            : null,
    _fntSz          : null,
    _MAX_ROW_WIDTH  : 1000,

    _toolbarButtons : {
        'Reload'                : { typ : 'btn', icn : 'arrowrefresh-1-e',   clk : $.noop },
        'Move to first'         : { typ : 'btn', icn : 'seek-first',         clk : $.noop },
        'Jump to previous page' : { typ : 'btn', icn : 'seek-prev',          clk : $.noop },
        ''                      : { typ : 'txt', clk : function() { $(this).select(); } },
        'Next page'             : { typ : 'btn', icn : 'play',               clk : $.noop },
        'Move to end'           : { typ : 'btn', icn : 'seek-end',           clk : $.noop },
        'Move to end then Tail' : { typ : 'btn', icn : 'fetch-tail',         clk : $.noop },
        'Skip to end and Tail'  : { typ : 'btn', icn : 'fetch-only',         clk : $.noop },
        'Commit changes'        : { typ : 'btn', icn : 'check',              clk : $.noop },
        'Discard changes'       : { typ : 'btn', icn : 'close',              clk : $.noop },
        'Jump to next page'     : { typ : 'btn', icn : 'seek-next',          clk : $.noop }
    },

    // These options will be used as defaults
    options: { 
      clear: null,
      toolBarHeight: 27,
      slickopts: {
        editable: true,
        enableAddRow: true,
        enableColumnReorder: true,
        enableCellNavigation: true,
        asyncEditorLoading: false,
        autoEdit: false,
        zIndex: 1300,
        rowHeight: 20
      },
      open: function(e,ui) {},
      focus: function(e,ui) {},
      close: function() {
        $(this).dialog('destroy');
        $(this).remove();
      }
    },
 
    // Set up the widget
    _create: function() {
        var self = this;

        self._fnt = self.element.find("*").css('font-family');
        self._fntSz = self.element.find("*").css('font-size');

        // text pixel width measurement field
        self._txtlen =
            $('<span>')
            .css('display', 'none')
            .css('font-family', self._fnt)
            .css('font-size', self._fntSz)
            .appendTo(self.element);

        // the slickgrid table
        self._table =
            $('<div id="body">')
            .css('position', 'absolute')
            .css('top', '0')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', self.options.toolBarHeight+'px')
            .css('overflow', 'auto')
            .css('border-style', 'solid')
            .css('border-width', '1px')
            .css('border-color', 'lightblue')
            .appendTo(self.element);

        // footer for the toolbar
        self._footer =
            $('<div>')
            .css('height', self.options.toolBarHeight+'px')
            .css('position', 'absolute')
            //.css('top', 'auto')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', '0')
            .css('overflow', 'hidden')
            .appendTo(self.element);

        for(btnTxt in self._toolbarButtons) {
            var elm = self._toolbarButtons[btnTxt];
            if(elm.typ === 'btn')
                $('<button>')
                    .text(btnTxt)
                    .button({icons: {primary: 'ui-icon-' + elm.icn}, text: false})
                    .css('height', this.options.toolBarHeight+'px')
                    .click(elm.clk)
                    .appendTo(self._footer);
            else if(elm.typ === 'txt')
                $('<input>')
                    .attr('type', 'text')
                    .attr('size', 10)
                    .addClass('download_incomplete')
                    .css('margin-bottom', '0px')
                    .css('margin-left', '4px')
                    .css('height', (self.options.toolBarHeight-5)+'px')
                    .click(elm.clk)
                    .appendTo(self._footer);
        }
        self._footer.buttonset().css('height', self.options.toolBarHeight+'px');

        self._dlg =
            self.element.dialog(self.options)
            .bind("dialogresize", function(event, ui) 
            {
                self._table.width(self._dlg.width()-2);
                self._table.height(self._dlg.height()-self.options.toolBarHeight-2);
                //self._footer.css('top', self._dlg.height()-self.options.toolBarHeight-10);
                var data = self._grid.getData();
                self._grid.setData(data);
                self._grid.updateRowCount();
                self._grid.render();
                self._grid.resizeCanvas();
            });

    },
 
    // Opens the ui.dialog with a slickgrid in it
    open: function() {
        var self = this;
        self._dlg.dialog("open");
        self._grid = new Slick.Grid('#'+self._table.attr('id'), [], [], self.options.slickopts);
        self._grid.setSelectionModel(new Slick.CellRowColSelectionModel());
        self._gdata = self._grid.getData();
    },

    // Use the _setOption method to respond to changes to options
    _setOption: function( key, value ) {
                  switch( key ) {
                    case "clear":
                      // handle changes to clear option
                      break;
                  }
 
                  // In jQuery UI 1.9 and above, you use the _super method instead
                  this._super( "_setOption", key, value );
                },
 
    // Use the destroy method to clean up any modifications your widget has made to the DOM
    _destroy: function() {
              },

    setSlickOpts: function(_opts) {
        this.options.slickopts = _opts;
    },

    setColumns: function(_cols) {
        var self = this;

        // Column Data
        var columns = new Array();
        columns[columns.length] = {id: "sel",
                                 name: "",
                                field: "id",
                             behavior: "select",
                             cssClass: "cell-selection",
                             minWidth: 35,
                                width: 35,
                  cannotTriggerInsert: true,
                            resizable: false,
                             sortable: false,
                           selectable: false};
        for (i=0;i<_cols.length;++i) {
            var fldid = _cols[i];
            if(fldid == fldid.toLowerCase() && fldid == 'id')
                fldid = ('_'+fldid.toLowerCase());
            columns[columns.length] = {id: fldid,
                                     name: _cols[i],
                                    field: fldid,
                                   editor: Slick.Editors.Text,
                                 minWidth: 20,
                                    width: self._txtlen.text(_cols[i]).width()+25,
                                resizable: true,
                                 sortable: false,
                               selectable: true};
        }
        self._grid.setColumns(columns);
    },

    replaceRows: function(_rows) {
        if($.isArray(_rows) && _rows.length > 0) {
            var self = this;
            self._gdata = [];
            self._grid.setData(self._gdata);
            self.replaceRows(_rows);
        }
    },

    appendRows: function(_rows) {
        if($.isArray(_rows) && _rows.length > 0) {
            var self = this;
            var c = self._grid.getColumns();
            if($.isArray(_rows[0]) && _rows[0].length <= c.length) {
                var isIdMissing = (_rows[0].length === c.length ? false : true);
                for(var i=0;i<_rows.length;++i) {
                    if(isIdMissing) // add the missing id field
                        _rows[i].splice(0,0,self._gdata.length+1);
                    var row = {};
                    for(var j=0;j<c.length;++j) {
                        var str = _rows[i][j];
                        var fieldWidth = self._txtlen.text(str).width()+25;
                        if(c[j].width < fieldWidth) {
                            c[j].width = fieldWidth;
                            if (c[j].width > self._MAX_ROW_WIDTH)
                                c[j].width = self._MAX_ROW_WIDTH;
                        }
                        row[c[j].field] = _rows[i][j];
                    }
                    self._grid.setColumns(c);
                    self._gdata.push(row);
                    self._grid.updateRowCount();
                    self._grid.render();
                    self._grid.scrollRowIntoView(self._gdata.length-1)
                }
            }
        }
    }

  });
}( jQuery ) );
