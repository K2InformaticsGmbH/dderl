(function( $ ) {
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

    _url            : null,
    _stmt           : null,
    _session        : null,
    _adapter        : null,
    _cmd            : null,
    _clmlay         : null,
    _tbllay         : null,
    _rows_cache_max : null,
    _fetch_status   : null,

    // private event handlers
    _handlers       : { loadViews   : function(e, _views) {
                            var self = e.data;
                            self._checkRespViews(_views);
                            self._renderViews(_views);
                        },
                        loadRows    : function(e, _rows)  {
                            var self = e.data; 
                            self._checkRows(_rows);
                            self._renderRows(_rows);
                        },
                        browseData  : function(e, _table) {
                            var self = e.data; 
                            self._checkTable(_table);
                            self._renderTable(_table);
                        }
                      },

    _toolbarButtons : {'Reload'                : { typ : 'btn', icn : 'arrowrefresh-1-e',   clk : $.noop },
                       'Move to first'         : { typ : 'btn', icn : 'seek-first',         clk : $.noop },
                       'Jump to previous page' : { typ : 'btn', icn : 'seek-prev',          clk : $.noop },
                       ''                      : { typ : 'txt', clk : function() { $(this).select(); } },
                       'Next page'             : { typ : 'btn', icn : 'play',               clk : $.noop },
                       'Move to end'           : { typ : 'btn', icn : 'seek-end',           clk : $.noop },
                       'Move to end then Tail' : { typ : 'btn', icn : 'fetch-tail',         clk : $.noop },
                       'Skip to end and Tail'  : { typ : 'btn', icn : 'fetch-only',         clk : $.noop },
                       'Commit changes'        : { typ : 'btn', icn : 'check',              clk : $.noop },
                       'Discard changes'       : { typ : 'btn', icn : 'close',              clk : $.noop },
                       'Jump to next page'     : { typ : 'btn', icn : 'seek-next',          clk : $.noop }},

    // slick context menus
    _slkHdrCnxtMnu  : {'Browse Data'    : '_browseHeaderData',
                       'Sort Ascending' : '_sortHeaderAsc',
                       'Sort Decending' : '_sortHeaderDesc',
                       'Sort Clear'     : '_sortHeaderClear'},

    _slkCellCnxtMnu : {'Browse Data'    : '_browseCellData',
                       'AND filter'     : '_filterAnd',
                       'OR filter'      : '_filterOr',
                       'Filter Clear'   : '_filterClear'},

    // These options will be used as defaults
    options: {
        // dialog options default override
        toolBarHeight     : 20,
        height            : 500,
        width             : 500,
        minHeight         : 200,
        resizable         : true,
        modal             : false,
        title             : "_Set TITLE here_",
        canMinimize       : true,
        canMaximize       : true,
        closeOnEscape     : false,
        clear             : null,
        toolBarHeight     : 27,
        open              : function(e,ui) {},
        focus             : function(e,ui) {},
        close             : function() {
                              $(this).dialog('destroy');
                              $(this).remove();
                            },

        // slickgrid options default override
        slickopts         : { editable: true,
                              enableAddRow: true,
                              enableColumnReorder: true,
                              enableCellNavigation: true,
                              asyncEditorLoading: false,
                              autoEdit: false,
                              zIndex: 1300,
                              rowHeight: 20},

        // dderl options
        dderlAdapter      : null,
        dderlSession      : null,
        dderlStatement    : null,
        dderlCmd          : null,
        dderlClmlay       : null,
        dderlTbllay       : null
    },
 
    // Set up the widget
    _create: function() {
        var self = this;

        self._fnt = self.element.find("*").css('font-family');
        self._fntSz = self.element.find("*").css('font-size');

        // preserve some options
        if(self.options.dderlAdapter    !== self._adapter)  self._adapter   = self.options.dderlAdapter;
        if(self.options.dderlSession    !== self._session)  self._session   = self.options.dderlSession;
        if(self.options.dderlStatement  !== self._stmt)     self._stmt      = self.options.dderlStatement;
        if(self.options.dderlCmd        !== self._cmd)      self._cmd       = self.options.dderlCmd;
        if(self.options.dderlClmlay     !== self._clmlay)   self._clmlay    = self.options.dderlClmlay;
        if(self.options.dderlTbllay     !== self._tbllay)   self._tbllay    = self.options.dderlTbllay;

        // dialog elements

        // field for text width measurement in pixels
        // added to document.body once
        if($('#txtlen').length === 0) {
            self._txtlen =
                $('<span>')
                .attr('id', 'txtlen')
                .css('display', 'none')
                .css('font-family', self._fnt)
                .css('font-size', self._fntSz)
                .appendTo(document.body);
        } else {
            self._txtlen = $('#txtlen')
        }

        // slickgrid container
        self._table =
            $('<div id="id_for_slickgrid_'+getUniqueTime()+'">') // dummy id (required by slickgrid) not used anywere else
            .appendTo(self.element);

        // toolbar container
        self._footer = $('<div>').appendTo(self.element);

        self._createDlg();

        self._createSlickGrid();
        self._createDlgFooter();
        self._createContextMenus();

        // setting up the event handlers last to aid debugging
        self._setupEventHandlers();
    },

    _init: function() {
        var self = this;

        // default dialog open behavior
    	if ( self.options.autoOpen )
            self._dlg.dialog("open");
    },


    _createContextMenus: function() {
        var self = this;

        self._cnxtMenu('_slkCellCnxtMnu'); // cell context menu
        self._cnxtMenu('_slkHdrCnxtMnu');  // header context menu
    },
                    
    // create the context menu and add them to document.body
    // only if they do not exist
    _cnxtMenu: function(_menu) {
        if($('#'+_menu).length === 0) {
            var mnu = $('<ul>')
                .attr('id', _menu)
                .addClass("context_menu")
                .hide()
                .mouseleave(function(e) { e.preventDefault(); $(this).hide(); })
                .appendTo(document.body);
            for(var m in this[_menu])
                $('<li>')
                    .attr("action", m)
                    .click(this, function(e) {
                        var self = e.data;
                        self[_menu].dom.hide();
                        var gSelMdl = self._grid.getSelectionModel();
                        self._cnxtMenuAction(_menu, $(this).attr("action"), gSelMdl.getSelectedRanges());
                    })
                    .text(m)
                    .appendTo(mnu);
            this[_menu].dom = mnu;
        }
    },

    // delegate actions of context menu
    _cnxtMenuAction: function(_menu, _action, _ranges) {
        var funName = this[_menu][_action];
        var fun = $.proxy(this[funName], this);

        if($.isFunction(fun)) {
            console.log('applying fun '+funName+' for \''+_action+ '\' in '+_menu+' for '+_ranges.length+' slick range(s)');
            fun(_ranges);
        }
        else
            throw('unimplimented fun '+funName+' for \''+_action+ '\' in '+_menu+' for '+_ranges.length+' slick range(s)');
    },

    // browse_data actions
    _browseCellData: function(_ranges) {
        var self = this;
        
        // console.log('_browseCellData for '+_ranges.length+' slick range(s)');

        // test the range and throw unsupported exceptions
        if(_ranges.length >= 2 && (!(
                 _ranges[0].fromRow  === _ranges[0].toRow  && // single cell
                 _ranges[0].fromCell === _ranges[0].toCell &&
                 _ranges[1].fromRow  === _ranges[1].toRow  && // single cell
                 _ranges[1].fromCell === _ranges[1].toCell &&
                 _ranges[1].fromCell === _ranges[0].toCell && // same cell
                 _ranges[1].fromRow  === _ranges[0].toRow
                 ) || _ranges.length > 2))
                throw('cell level \'Browse Data\' don\'t support multiples and ranges');
        else {
            var cell    = _ranges[0];
            var column  = self._grid.getColumns()[cell.fromCell];
            var data    = self._grid.getData()[cell.fromRow][column.field];
            // console.log('browse_data @ '+column.name+' val '+data);
            this._ajaxCall('/app/browse_data',
                           { browse_data: { statement : this._stmt,
                                            row : cell.fromRow,
                                            col : cell.fromCell}},
                           'browse_data', 'browseData');
        }
    },
    _checkTable: function(_table) {
        // TODO sanity check the data _THROW_ if ERROR
    },
    _renderTable: function(_table) {
        var pos = [];
        if(!_table.hasOwnProperty('table_layout') || !_table.table_layout.hasOwnProperty('x')) {
            var dlg = this._dlg.dialog('widget');
            var titleBarHeight = $(dlg.find('.ui-dialog-titlebar')[0]).height();
            pos = [dlg.position().left + titleBarHeight + 10, dlg.position().top + titleBarHeight + 10]
        } else {
            pos = [_table.table_layout.x, _table.table_layout.y];
        }

        $('<div>')
        .appendTo(document.body)
        .table({
            autoOpen        : false,
            title           : _table.name,
            position        : pos,

            dderlAdapter    : this._adapter,
            dderlSession    : this._session,
            dderlStatement  : _table.statement,
            dderlCmd        : _table.content,
            dderlClmlay     : _table.column_layout,
            dderlTbllay     : _table.table_layout
        })
        .table('setColumns', _table.columns)
        .table('fetchRows', OpsFetchEnum.NEXT, 0)
        .table('open');
    },

    _createSlickGrid: function() {
        var self = this;

        // the slickgrid table
        self._table
            .css('position', 'absolute')
            .css('top', '0')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', self.options.toolBarHeight+'px')
            .css('border-style', 'solid')
            .css('border-width', '1px')
            .css('border-color', 'lightblue');

        // building slickgrid
        self._grid = new Slick.Grid('#'+self._table.attr('id'), [], [], self.options.slickopts);
        self._grid.setSelectionModel(new Slick.CellRowColSelectionModel());

        self._grid.onContextMenu.subscribe($.proxy(self._gridContextMenu, self));
        self._grid.onHeaderContextMenu.subscribe($.proxy(self._gridHeaderContextMenu, self));

            //self._table
            //    .jScrollPane()
            //    .css('position', 'absolute')
            //    .css('top', '0')
            //    .css('left', '0')
            //    .css('right', '0')
            //    .css('bottom', self.options.toolBarHeight+'px');
        self._gdata = self._grid.getData();
    },

    _setupEventHandlers: function() {
        // make this as context to private event handler functions
        // and register for named events
        for(var fun in this._handlers) {
//            this._handlers[fun] = $.proxy(this._handlers[fun], this);
            this.element.on(fun, null, this, this._handlers[fun]);
        }
    },

    _createDlgFooter: function() {
        var self = this;

        // footer for the toolbar
        self._footer
            .css('height', self.options.toolBarHeight+'px')
            .css('position', 'absolute')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', '0')
            .css('overflow', 'hidden');

        // footer items
        for(btnTxt in self._toolbarButtons) {
            var elm = self._toolbarButtons[btnTxt];

            var inph = self.options.toolBarHeight;
            if($.browser.msie) inph -= 2;

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
                    .button()
                    .addClass('download_incomplete')
                    .css('height', inph+'px')
                    .css('text-align', 'left')
                    .css('padding', '0')
                    .css('margin', '0')
                    .css('margin-left', '3')
                    .click(elm.clk)
                    .appendTo(self._footer);
        }
        self._footer.buttonset().css('height', (self.options.toolBarHeight)+'px');
    },

    _createDlg: function() {
        var self = this;                    
        self._dlg = self.element
            .dialog(self.options)
            .bind("dialogresize", function(event, ui) 
            {
                var data = self._grid.getData();
                self._grid.setData(data);
                self._grid.updateRowCount();
                self._grid.render();
                self._grid.resizeCanvas();
            });

        // for dialog title as html DOM / jQuery Obj
        self._dlg.data( "uiDialog" )._title = function(title) {
            title.html('');
            title.append( this.options.title );
        };

        // converting the title text to a link
        self._dlg.dialog('option', 'title', $('<a href="#">'+self.options.title+'</a>'));
    },
 
    // translations to default dialog behavior
    open: function() { this._dlg.dialog("open"); },

    // generic dderlserver call interface
    _ajaxCall: function(_url,_data,_resphead,_successevt) {
        var self = this;

        // if data is JSON object format to string
        if(_data == null) _data = JSON.stringify({});
        else
            try {
                _data = JSON.stringify(_data);
            } catch (ex) {
                console.error(_data + ' is not JSON');
                throw(ex);
            }

        $.ajax({
            type: 'POST',
            url: _url,
            data: _data,
            dataType: "JSON",
            contentType: "application/json; charset=utf-8",
            headers: {dderl_sess: self._session
                     ,adapter: self._adapter
                     },
            context: self,
            success: function(_data) {
                console.log('received '+ JSON.stringify(_data));

                // save the new session - legacy maybe removed TODO
                if(_data.hasOwnProperty('session'))
                    this.options.dderlSession = self._session = _data.session;

                if(_data.hasOwnProperty(_resphead)) {
                    if(this._handlers.hasOwnProperty(_successevt))
                        this.element.trigger(_successevt, _data[_resphead]);
                    else
                        throw('unsupported success event '+_successevt+' for '+_url);
                } else throw('resp '+_resphead+' doesn\'t match the request '+_url);
            }
        });
    },

    // context menus invocation for slickgrid
    _gridContextMenu: function(e, args) {
        e.preventDefault();

        var g           = args.grid;
        var cell        = g.getCellFromEvent(e);
        var row         = cell.row;
        var column      = g.getColumns()[cell.cell];
        var data        = g.getData()[cell.row][column.field];
        var gSelMdl     = g.getSelectionModel();
        var gSelecteds  = gSelMdl.getSelectedRanges();

        var missing = true;
        for(var i=0; i < gSelecteds.length; ++i) {
            var tRow = cell.row;
            var tCol = cell.cell;
            var bFRw = Math.min(gSelecteds[i].fromRow, gSelecteds[i].toRow);
            var bFCl = Math.min(gSelecteds[i].fromCell, gSelecteds[i].toCell);
            var bTRw = Math.max(gSelecteds[i].fromRow, gSelecteds[i].toRow);
            var bTCl = Math.max(gSelecteds[i].fromCell, gSelecteds[i].toCell);

            if(bFRw <= tRow && bFCl <= tCol && bTRw >= tRow && bTCl >= tCol) {
                 missing = false;
                 break;
             }
        }
        if(missing)
            gSelecteds.push(new Slick.Range(cell.row, cell.cell, cell.row, cell.cell));
        
        gSelMdl.setSelectedRanges(gSelecteds);

        // console.log('cnxtmnu @ cell -> cell ('+cell.cell+', '+row+') value '+data+' column '+column.field);
        // console.log('cnxtmnu @ cell -> ('+e.clientX+', '+e.clientY+')');

        this._slkHdrCnxtMnu.dom.hide();
        this._slkCellCnxtMnu.dom
            .css("top", e.clientY - 10)
            .css("left", e.clientX)
            .show();
    },
    _gridHeaderContextMenu: function(e, args){
        e.preventDefault();

        var g           = args.grid;
        var col         = g.getColumnIndex(args.column.id);
        var gSelMdl     = g.getSelectionModel();
        var gSelecteds  = gSelMdl.getSelectedRanges();

        gSelecteds.push(new Slick.Range(0, col, g.getDataLength() - 1, col));
        gSelMdl.setSelectedRanges(gSelecteds);

        // console.log('cnxtmnu @ header -> (name, id, field) = ('+args.column.name+', '+args.column.id+', '+args.column.field+')');
        // console.log('cnxtmnu @ header -> ('+e.clientX+', '+e.clientY+')');

        this._slkCellCnxtMnu.dom.hide();
        this._slkHdrCnxtMnu.dom
            .css("top", e.clientY - 10)
            .css("left", e.clientX)
            .show();
    },

    // loading the view table
    loadViews: function() { this._ajaxCall('/app/views', null, 'views', 'loadViews'); },
    _checkRespViews: function(_views) {
        // TODO throw exception if any error
        this._cmd    = _views.content;
        this._stmt   = _views.statement;
        if(_views.hasOwnProperty('column_layout') && _views.column_layout.length > 0) {
            this._clmlay = _views.column_layout;
        }
        if(_views.hasOwnProperty('table_layout')  && _views.table_layout.length  > 0) {
            this._tbllay = _views.table_layout;
        }
    },
    _renderViews: function(_views) {
        this._dlg.dialog('option', 'title', $('<a href="#">'+_views.name+'</a>'));
        this.options.title = _views.name;
        this.setColumns(_views.columns);
        this.fetchRows(OpsFetchEnum.NEXT, 0);
    },

    // loading rows
    fetchRows: function(_fetchop, _rwnum) {
        var cmd = 'row';
        switch(_fetchop) {
            case OpsFetchEnum.NEXT:
                cmd += '_next';
                break;
            case OpsFetchEnum.PREVIOUS:
                cmd += '_prev';
                break;
            case OpsFetchEnum.TOEND:
                cmd += '_next';
                _rwnum = 10000000; // 10 mil for end of table for most table
                break;
            default:
                cmd += '_next';
                break;
        }
        if(_rwnum == null)
            _rwnum = -1;
        this._ajaxCall('/app/'+cmd, {row: {statement: this._stmt, row_num: _rwnum}}, cmd, 'loadRows');
    },
    _checkRows: function(_rows) {         
        // TODO throw exception if any error
        this._rows_cache_max = _rows.cache_max;
        this._fetch_status = _rows.done;
    },
    _renderRows: function(_rows) {
        this.appendRows(_rows.rows);
    },

    // Use the _setOption method to respond to changes to options
    _setOption: function( key, value ) {
        var self = this;
        var save = false;
        switch( key ) {
          case "clear":
            // handle changes to clear option
            save = true;
            break;
          case "dderlAdapter":
            // handle changes to dderlAdapter option
            if(self._adapter === null || self._adapter === value) {
                self._adapter = value;
                save = true;
            }
            else
                $.error('adapter is already set to '+self._adapter+' and can\'t be changed to '+value);
            break;
          case "dderlSession":
            // handle changes to dderlSession option
            if(self._session === null || self._session === value) {
                self._session = value;
                save = true;
            }
            else
                $.error('session is already set to '+self._session+' and can\'t be changed to '+value);
            break;
        }
 
        // In jQuery UI 1.9 and above, you use the _super method instead
        if (save) this._super( "_setOption", key, value );
    },
 
    // Use the destroy method to clean up any modifications your widget has made to the DOM
    _destroy: function() {
    },

    /*
     * SlickGrid interface
     */
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
                    self._grid.invalidateRow(self._gdata.length-1);
                    self._grid.render();
                    self._grid.scrollRowIntoView(self._gdata.length-1)
                }
            }
        }
    }

  });
}( jQuery ) );
