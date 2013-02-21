(function( $ ) {
  var SLICK_BUFFER_SIZE = 200;

  $.widget( "dderl.table", $.ui.dialog, {

    _dlg            : null,
    _tableDiv       : null,
    _footerDiv      : null,
    _footerWidth    : 0,
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
    _fetchIsDone    : false,
    _fetchIsTail    : false,
    _fetchIsPush    : false,

    _dlgResized     : false,

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
                            self._renderNewTable(_table);
                        },
                        queryResult : function(e, _table) {
                            var self = e.data; 
                            self._checkTable(_table);
                            self._renderTable(_table);
                        },
                        tailResult : function(e, _tail) {
                            var self = e.data;
                            self._checkTailResult(_tail);
                        },
                        updateData : function(e, _update) {
                            var self = e.data;
                            self._checkUpdateResult(_update);
                        },
                        insertData : function(e, _insert) {
                            var self = e.data;
                            self._checkInsertResult(_insert);
                            self._insertResult(_insert);
                        },
                        commitResult : function(e, _commit) {
                            var self = e.data;
                            self._checkCommitResult(_commit);
                        },
                        stmtCloseResult : function(e, _stmtclose) {
                            var self = e.data;
                            self._checkStmtCloseResult(_stmtclose);
                        },
                        saveViewResult  : function(e, _saveView) {
                            var self = e.data;
                            self._checkSaveViewResult(_saveView);
                        }
                      },

    _toolbarButtons : {'Reload'                : { typ : 'btn', icn : 'arrowrefresh-1-e', clk : '_toolBarReload',   dom: '_tbReload' },
                       'Move to first'         : { typ : 'btn', icn : 'seek-first',       clk : '_toolBarSkFrst',   dom: '_tbSkFrst' },
                       'Jump to previous page' : { typ : 'btn', icn : 'seek-prev',        clk : '_toolBarJmPrev',   dom: '_tbJmPrev' },
                       ''                      : { typ : 'txt',                           clk : '_toolBarTxtBox',   dom: '_tbTxtBox' },
                       'Next page'             : { typ : 'btn', icn : 'play',             clk : '_toolBarGoNex',    dom: '_tbGoNext' },
                       'Jump to next page'     : { typ : 'btn', icn : 'seek-next',        clk : '_toolBarJmNext',   dom: '_tbJmNext' },
                       'Move to end'           : { typ : 'btn', icn : 'seek-end',         clk : '_toolBarSekEnd',   dom: '_tbSekEnd' },
                       'Move to end then Tail' : { typ : 'btn', icn : 'fetch-tail',       clk : '_toolBarSkTail',   dom: '_tbSkTail' },
                       'Skip to end and Tail'  : { typ : 'btn', icn : 'fetch-only',       clk : '_toolBarSkipTl',   dom: '_tbSkipTl' },
                       'Commit changes'        : { typ : 'btn', icn : 'check',            clk : '_toolBarCommit',   dom: '_tbCommit' },
                       'Discard changes'       : { typ : 'btn', icn : 'close',            clk : '_toolBarDiscrd',   dom: '_tbDiscrd' }},

    // dialog context menus
    _dlgTtlCnxtMnu  : {'Edit SQL'       : '_editCmd',
                       'Save View'      : '_saveView',
                       'Save View As'   : '_saveViewAs'},

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
        minHeight         : 50,
        minWidth          : 100,
        position          : { at        : 'left top',
                              my        : 'left top+21',
                              collision : 'flipfit' },
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
                              rowHeight: 20 },

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

        self._fnt = $(document.body).css('font-family');
        self._fntSz = $(document.body).css('font-size');

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
                .css('visibility', 'hidden')
                .css('font-family', self._fnt)
                .css('font-size', self._fntSz)
                .appendTo(document.body);
        } else {
            self._txtlen =
                $('#txtlen')
                .css('font-family', self._fnt)
                .css('font-size', self._fntSz)
        }

        // slickgrid container
        self._tableDiv =
            $('<div id="id_for_slickgrid_'+getUniqueTime()+'">') // dummy id (required by slickgrid) not used anywere else
            .appendTo(self.element);

        // toolbar container
        self._footerDiv = $('<div>').appendTo(self.element);

        // need the max footer with to set as dlg minWidth
        self._createDlgFooter();
        self._createDlg();

        self._createSlickGrid();
        self._createContextMenus();

        // setting up the event handlers last to aid debugging
        self._setupEventHandlers();
    },

    _init: function() {
        // default dialog open behavior
    	if ( this.options.autoOpen )
            this._dlg.dialog("open");
    },

    _createContextMenus: function() {
        var self = this;

        self._cnxtMenu('_slkCellCnxtMnu'); // cell context menu
        self._cnxtMenu('_slkHdrCnxtMnu');  // header context menu
        self._cnxtMenu('_dlgTtlCnxtMnu');  // header context menu
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
                    .click(function(e) {
                        var self = $('#'+_menu).data('cnxt');
                        if(undefined != self) {
                            self[_menu].dom.hide();
                            console.log('self title _cnxtMenu '+self.options.title);
                            self._cnxtMenuAction(_menu, $(this).attr("action"));
                        }
                    })
                    .text(m)
                    .appendTo(mnu);
            this[_menu].dom = mnu;
        }
    },

    // delegate actions of context menu
    _cnxtMenuAction: function(_menu, _action) {
        var funName = this[_menu][_action];
        var fun = $.proxy(this[funName], this);
        var data = null;
        if($.isFunction(fun)) {
            switch(_menu) {
                case '_slkHdrCnxtMnu':
                case '_slkCellCnxtMnu':
                    data = this._grid.getSelectionModel().getSelectedRanges();
                break;
                case '_dlgTtlCnxtMnu':
                    console.log('title action '+this.options.title);
                    data = this._cmd;
                break;
            }
            //console.log('applying fun '+funName+' for \''+_action+ '\' in '+_menu+' for '+data);
            fun(data);
        }
        else
            throw('unimplimented fun '+funName+' for \''+_action+ '\' in '+_menu);
    },

    _editCmd: function(cmd) {
        $('<div>')
        .appendTo(document.body)
        .sql({autoOpen  : false,
              title     : this.options.title,
              cmdOwner  : this
             })
        .sql('open')
        .sql("showCmd", this._cmd);
    },

    /*
     * Saving a table
     */
    _saveView: function() {
        this._saveViewWithName(this.options.title);
    },
    _saveViewAs: function() {
        var viewName = prompt("View name",this.options.title);
        if (null !== viewName)
            this._saveViewWithName(viewName);
    },
    _saveViewWithName: function(_viewName) {
        //qStr = this._cmd.content.replace(/(\r\n|\n|\r)/gm," ");
        var colnamesizes = new Array();
        var cols = this._grid.getColumns();
        // Column names and width
        for(var idx = 0; idx < cols.length; ++idx)
            if(cols[idx].name.length > 0)
                colnamesizes[colnamesizes.length] = {name: cols[idx].name, width: cols[idx].width};
        // Table width/height/position
        var w = this._dlg.width();
        var h = this._dlg.height();
        var x = this._dlg.dialog('widget').position().left;
        var y = this._dlg.dialog('widget').position().top;
        var saveView = {save_view : {table_layout : {width : w,
                                                    height : h,
                                                         y : y,
                                                         x : x},
                                    column_layout : colnamesizes,
                                             name : _viewName,
                                          content : this._cmd}
                       };
        console.log('saving view '+JSON.stringify(saveView));
        this._ajaxCall('/app/save_view', saveView, 'save_view', 'saveViewResult');
    },

    cmdReload: function(cmd) {
        if(this._cmd === cmd)
            console.log('command unchanged ['+cmd+']');
        else {
            console.log('command reloading ['+cmd+']');
            this._cmd = cmd;
            this._ajaxCall('/app/query', {query: {qstr : this._cmd}}, 'query', 'queryResult');
        }
    },


    // browse_data actions
    _browseCellData: function(_ranges) {
        var self = this;
        
        console.log('_browseCellData for '+_ranges.length+' slick range(s)');

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

    _createSlickGrid: function() {
        var self = this;

        // the slickgrid table
        self._tableDiv
            .css('position', 'absolute')
            .css('top', '0')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', self.options.toolBarHeight+'px')
            .css('border-style', 'solid')
            .css('border-width', '1px')
            .css('border-color', 'lightblue');

        // building slickgrid
        self._grid = new Slick.Grid('#'+self._tableDiv.attr('id'), [], [], self.options.slickopts);
        self._grid.setSelectionModel(new Slick.CellRowColSelectionModel());

        self._grid.onContextMenu.subscribe($.proxy(self._gridContextMenu, self));
        self._grid.onHeaderContextMenu.subscribe($.proxy(self._gridHeaderContextMenu, self));
        self._grid.onCellChange.subscribe($.proxy(self._gridCellChange, self));
        self._grid.onAddNewRow.subscribe($.proxy(self._gridAddNewRow, self));

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
        for(var fun in this._handlers)
            this.element.on(fun, null, this, this._handlers[fun]);
    },

    _createDlgFooter: function() {
        var self = this;

        // footer for the toolbar
        self._footerDiv
            .css('height', self.options.toolBarHeight+'px')
            .css('position', 'absolute')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', '0')
            .css('overflow', 'hidden');

        // footer items
        for(btnTxt in self._toolbarButtons) {
            var elm = self._toolbarButtons[btnTxt];

            var toolElmFn = function(e) {
                var self = e.data;
                var _btnTxt = $(this).text();
                var fName = self._toolbarButtons[_btnTxt].clk;
                //var f = $.proxy(self[fName], self);
                var f = self[fName];
                if($.isFunction(f))
                    f(self);
                else
                    throw('['+self.options.title+'] toolbar '+_btnTxt+' has unimplimented cb '+fName);
            };

            var inph = self.options.toolBarHeight;
            if($.browser.msie) inph -= 2;

            if(elm.typ === 'btn')
                self[elm.dom] =
                    $('<button>')
                    .text(btnTxt)
                    .button({icons: {primary: 'ui-icon-' + elm.icn}, text: false})
                    .css('height', this.options.toolBarHeight+'px')
                    .click(self, toolElmFn)
                    .appendTo(self._footerDiv);
            else if(elm.typ === 'txt')
                self[elm.dom] =
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
                    .click(self, toolElmFn)
                    .appendTo(self._footerDiv);
        }
        self._footerDiv
            .buttonset()
            .css('height', (self.options.toolBarHeight)+'px');

        // footer total width
        var childs = self._footerDiv.children();
        var totWidth = 0;
        for(var i=0; i<childs.length; ++i)
            totWidth += $(childs[i]).width();

        self._footerWidth = totWidth;
    },

    /*
     * Toolbar callbak functions
     */
    // NOTE: self is 'this' and 'this' is dom ;)
    _toolBarReload: function(self) {
        console.log('['+self.options.title+']'+' reloading '+self._cmd);
        self._ajaxCall('/app/query', {query: {qstr : self._cmd}}, 'query', 'queryResult');
    },

    _toolBarSkFrst: function(self) {
        self._grid.scrollRowIntoView(0);
        //console.log('['+self.options.title+'] cb _toolBarSkFrst');
    },
    _toolBarJmPrev: function(self) {
        var row = Math.floor(self._grid.getViewport().top / 2);
        if (row < 1) row = 0;
        self._grid.scrollRowIntoView(row);
        console.log('['+self.options.title+'] cb _toolBarJmPrev vp top '+row);
    },
    _toolBarTxtBox: function(self) {
        console.log('['+self.options.title+'] cb _toolBarTxtBox');
    },
    _toolBarGoNex: function(self) {
        console.log('['+self.options.title+'] cb _toolBarGoNex fetching from '+self._gdata[self._gdata.length-1].id);
        self.fetchRows(OpsFetchEnum.NEXT, parseInt(self._gdata[self._gdata.length-1].id)+1);
    },
    _toolBarJmNext: function(self) {
        var row = self._grid.getViewport().bottom * 2;
        if (row <= self._gdata.length) {
            self._grid.scrollRowIntoView(row);
            console.log('['+self.options.title+'] cb _toolBarJmNext vp bottom '+row);
        } else
            self.fetchRows(OpsFetchEnum.NEXT, parseInt(self._gdata[self._gdata.length-1].id)+1);
    },
    _toolBarSekEnd: function(self) {
        console.log('['+self.options.title+'] cb _toolBarSekEnd');
        self._fetchIsTail = false;
        self._fetchIsPush = true;
        self._ajaxCall('/app/tail',  {tail: {statement: self._stmt, push: self._fetchIsPush, tail: self._fetchIsTail}}, 'tail', 'tailResult');
    },
    _toolBarSkTail: function(self) {
        console.log('['+self.options.title+'] cb _toolBarSkTail');
        self._fetchIsTail = true;
        self._fetchIsPush = true;
        self._ajaxCall('/app/tail',  {tail: {statement: self._stmt, push: self._fetchIsPush, tail: self._fetchIsTail}}, 'tail', 'tailResult');
    },
    _toolBarSkipTl: function(self) {
        console.log('['+self.options.title+'] cb _toolBarSkipTl');
        self._fetchIsTail = true;
        self._fetchIsPush = false;
        self._ajaxCall('/app/tail',  {tail: {statement: self._stmt, push: self._fetchIsPush, tail: self._fetchIsTail}}, 'tail', 'tailResult');
    },
    _toolBarCommit: function(self) {
        console.log('['+self.options.title+'] cb _toolBarCommit');
        self._ajaxCall('/app/commit_rows',  {commit_rows: {statement: self._stmt}}, 'commit_rows', 'commitResult');
    },
    _toolBarDiscrd: function(self) {
        console.log('['+self.options.title+'] cb _toolBarDiscrd');
    },
    ////////////////////////////
    
    /*
     * _ajaxCall success callbacks
     */
    _checkTable: function(_table) {
        // TODO sanity check the data _THROW_ if ERROR
    },
    _checkRows: function(_rows) {         
        // TODO throw exception if any error
        this._rows_cache_max = _rows.cache_max;
        this._tbTxtBox.val(this._rows_cache_max);
        this._fetchIsDone = _rows.done;
        console.log('[AJAX] ets buffer count : '+this._rows_cache_max+', fetch status : '+this._fetchIsDone);
    },
    _checkRespViews: function(_views) {
        // TODO throw exception if any error
        this._cmd    = _views.content;
        this._stmt   = _views.statement;
        if(_views.hasOwnProperty('column_layout') && _views.column_layout.length > 0) {
            this._clmlay = _views.column_layout;
            if(_views.column_layout.length === 0) this._clmlay = null;
        }
        if(_views.hasOwnProperty('table_layout')  && _views.table_layout.length  > 0) {
            this._tbllay = _views.table_layout;
            if(_views.table_layout.length  === 0) this._tbllay = null;
        }
    },
    _checkTailResult: function(_tail) {
        console.log('[AJAX] tail resp '+JSON.stringify(_tail));
        if(_tail === 'ok') {
            this.fetchRows(OpsFetchEnum.NEXT, parseInt(this._gdata[this._gdata.length-1].id)+1);
        }
    },
    _checkUpdateResult: function(_update) {
        console.log('[AJAX] update_data resp '+JSON.stringify(_update));
        if(_update === 'ok') {
            console.log('update success');
        } else {
            if(_update.hasOwnProperty('error'))
                alert_jq('update failed!\n'+_update.error);
        }
    },
    _checkInsertResult: function(_insert) {
        if(isNaN(parseInt(_insert)) || !this.hasOwnProperty('__insertingRow')) {
            if(_insert.hasOwnProperty('error'))
                alert_jq('insert failed!\n'+_insert.error);
        } else {
            console.log('[AJAX] insert_data resp '+JSON.stringify(_insert));
        }
    },
    _checkCommitResult: function(_commit) {
        if(_commit === "ok")
            console.log('[AJAX] commit success!');
        else if(_commit.hasOwnProperty('error'))
            alert_jq('commit failed!\n'+_commit.error);
    },
    _checkStmtCloseResult: function(_stmtclose) {
        if(_stmtclose === "ok")
            console.log('[AJAX] statement_closed!');
        else if(_stmtclose.hasOwnProperty('error'))
            alert_jq('failed to close statement!\n'+_stmtclose.error);
    },
    _checkSaveViewResult: function(_saveView) {
        if(_saveView === "ok")
            console.log('[AJAX] view saved!');
        else if(_saveView.hasOwnProperty('error'))
            alert_jq('failed to save view!\n'+_saveView.error);
    },
    _insertResult: function(_insert) {
        if(!isNaN(parseInt(_insert)) && this.hasOwnProperty('__insertingRow')) {
            var item = this.__insertingRow;
            item['id'] = parseInt(_insert);
            this._grid.invalidateRow(this._gdata.length);
            this._gdata.push(item);
            this._grid.updateRowCount();
            this._grid.render();
            console.log('[AJAX] inserted data '+JSON.stringify(item));
        }
    },
    _renderViews: function(_views) {
        this._dlg.dialog('option', 'title', $('<a href="#">'+_views.name+'</a>'));
        this.options.title = _views.name;
        this.setColumns(_views.columns);
        this.fetchRows(OpsFetchEnum.NEXT, 0);
    },
    _renderTable: function(_table) {
        this._stmt = _table.statement;
        if(_table.hasOwnProperty('column_layout') && _table.column_layout.length > 0) {
            this._clmlay = _table.column_layout;
            if(_table.column_layout.length === 0) this._clmlay = null;
        }
        if(_table.hasOwnProperty('table_layout')  && _table.table_layout.length  > 0) {
            this._tbllay = _table.table_layout;
            if(_table.table_layout.length  === 0) this._tbllay = null;
        }
        this.setColumns(_table.columns);
        this._dlgResized = true;
        this._grid.setData([]);
        this._gdata = this._grid.getData();
        this.fetchRows(OpsFetchEnum.NEXT, 0);
    },
    _renderNewTable: function(_table) {
        var pos = [];
        if(!_table.hasOwnProperty('table_layout') || !_table.table_layout.hasOwnProperty('x')) {
            var dlg = this._dlg.dialog('widget');
            var titleBarHeight = $(dlg.find('.ui-dialog-titlebar')[0]).height();
            pos = [dlg.position().left + titleBarHeight + 10, dlg.position().top + titleBarHeight + 10]
        } else {
            pos = [_table.table_layout.x, _table.layout.y];
        }

        var cl = null;
        if(_table.hasOwnProperty('column_layout') && _table.column_layout.length > 0)
            cl = _table.column_layout.length;
        var tl = null;
        if(_table.hasOwnProperty('table_layout') && _table.table_layout.length > 0)
            tl = _table.table_layout.length;
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
            dderlClmlay     : cl,
            dderlTbllay     : tl
        })
        .table('setColumns', _table.columns)
        .table('fetchRows', OpsFetchEnum.NEXT, 0)
        .table('open');
    },
    _renderRows: function(_rows) {
        var self = this;
        
        //console.log('rows '+ JSON.stringify(_rows.rows));
        console.log('[AJAX] rendering '+ _rows.rows.length+' rows');
        this.appendRows(_rows.rows);

        // fetch till end and then stop
        if(!this._fetchIsDone && (this._fetchIsPush || this._fetchIsTail)) {
            this.fetchRows(OpsFetchEnum.NEXT, parseInt(this._gdata[this._gdata.length-1].id)+1);
        }

        // fetch till end and then continue tail
        else if(this._fetchIsDone && this._fetchIsTail) {
            this._tailTimer = setTimeout(function() {
                console.log('tailing...');
                self.fetchRows(OpsFetchEnum.NEXT, parseInt(self._gdata[self._gdata.length-1].id)+1);
            }, 1000);
        }
    },
    ////////////////////////////

    _createDlg: function() {
        var self = this;                    

        if(self._tbllay !== null) {
            self.options['width'] = self._tbllay.width;
            self.options['height'] = self._tbllay.height;
            self.options['position'] = [self._tbllay.x, self._tbllay.y];
        }

        // dlg width can't be less than footer width
        self.options.minWidth = self._footerWidth;
        self._dlg = self.element
            .dialog(self.options)
            .bind("dialogresize", function(event, ui) 
            {
                var data = self._grid.getData();
                self._grid.setData(data);
                self._grid.updateRowCount();
                self._grid.render();
                self._grid.resizeCanvas();
                self._dlgResized = true;
            });

        // for dialog title as html DOM / jQuery Obj
        self._dlg.data( "uiDialog" )._title = function(title) {
            title.html('');
            this.options.title
                .click(function(e) {
                    self._dlgTtlCnxtMnu.dom
                        .css("top", e.clientY - 10)
                        .css("left", e.clientX)
                        .data('cnxt', self)
                        .show();
                });
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

        console.log('[AJAX] TX '+_url);

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

                // save the new session - legacy maybe removed TODO
                if(_data.hasOwnProperty('session'))
                    this.options.dderlSession = self._session = _data.session;

                if(_data.hasOwnProperty(_resphead)) {
                    console.log('[AJAX] RX '+_resphead);
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

        this._slkHdrCnxtMnu.dom
            .removeData('cnxt')
            .hide();
        this._slkCellCnxtMnu.dom
            .css("top", e.clientY - 10)
            .css("left", e.clientX)
            .data('cnxt', this)
            .show();
    },
    _gridHeaderContextMenu: function(e, args) {
        e.preventDefault();

        var g           = args.grid;
        var col         = g.getColumnIndex(args.column.id);
        var gSelMdl     = g.getSelectionModel();
        var gSelecteds  = gSelMdl.getSelectedRanges();

        gSelecteds.push(new Slick.Range(0, col, g.getDataLength() - 1, col));
        gSelMdl.setSelectedRanges(gSelecteds);

        // console.log('cnxtmnu @ header -> (name, id, field) = ('+args.column.name+', '+args.column.id+', '+args.column.field+')');
        // console.log('cnxtmnu @ header -> ('+e.clientX+', '+e.clientY+')');

        this._slkCellCnxtMnu.dom
            .removeData('cnxt')
            .hide();
        this._slkHdrCnxtMnu.dom
            .css("top", e.clientY - 10)
            .css("left", e.clientX)
            .data('cnxt', this)
            .show();
    },
    _gridCellChange: function(e, args) {
        e.stopPropagation();

        var g           = args.grid;
        var modifiedRow = g.getData()[args.row];
        var cols        = g.getColumns();
        var updateJson  = {update_data: {statement   : this._stmt,
                                         rowid       : parseInt(modifiedRow.id),
                                         cellid      : args.cell,
                                         value       : modifiedRow[cols[args.cell].field]}};
        console.log('changed '+JSON.stringify(updateJson));

        this._ajaxCall('/app/update_data', updateJson, 'update_data', 'updateData');
    },
    _gridAddNewRow: function(e, args) {
        e.stopPropagation();

        var insertJson = {insert_data: {statement   : this._stmt,
                                        col         : args.column.id,
                                        value       : args.item[args.column.id]}};
        //console.log('inserting '+JSON.stringify(args.item));
        this['__insertingRow'] = args.item;
        this._ajaxCall('/app/insert_data', insertJson, 'insert_data', 'insertData');
    },

    // loading the view table
    loadViews: function() { this._ajaxCall('/app/views', null, 'views', 'loadViews'); },

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
        console.log('destroying...');
        this._ajaxCall('/app/stmt_close', {stmt_close: {statement: this._stmt}}, 'stmt_close', 'stmtCloseResult');
    },

    /*
     * SlickGrid interface
     */
    _getGridWidth: function() {
        var columns = this._grid.getColumns();
        var gWidth = 0;
        for(var i=0;i<columns.length;++i)
            gWidth += (1+columns[i].width);
        return gWidth;
    },

    _getGridHeight: function() {
        var rows = this._grid.getData().length + 2;
        return (rows * this.options.slickopts.rowHeight) + this.options.toolBarHeight;
    },

    setSlickOpts: function(_opts) {
        this.options.slickopts = _opts;
    },

    setColumns: function(_cols) {
        var self = this;
        var dlg = this._dlg.dialog('widget');

        // Column Data
        var columns = new Array();
        columns[columns.length] = {id: "sel",
                                 name: "",
                                field: "id",
                             behavior: "select",
                             cssClass: "cell-selection",
                                width: 30,
                             minWidth: 2,
                  cannotTriggerInsert: true,
                            resizable: true,
                             sortable: false,
                           selectable: false};
        for (i=0;i<_cols.length;++i) {
            var fldid = _cols[i];
            var fldWidth = self._txtlen.text(_cols[i]).width()+25;
            if(fldid == fldid.toLowerCase() && fldid == 'id')
                fldid = ('_'+fldid.toLowerCase());
            columns[columns.length] = {id: fldid,
                                     name: _cols[i],
                                    field: fldid,
                                   editor: Slick.Editors.Text,
                                 minWidth: fldWidth,
                                    width: fldWidth,
                                 //minWidth: _cols[i].visualLength(),
                                resizable: true,
                                 sortable: false,
                               selectable: true};
        }

        // load the column layout if its was saved
        if(self._clmlay !== null)
            for(var i=0;i<self._clmlay.length;++i) {
                for(var j=0;j<self._clmlay.length;++j) {
                    if(columns[i].name === self._clmlay[j].name)
                        columns[i].width = self._clmlay[j].width
                }
            }
        self._grid.setColumns(columns);

        if(self._tbllay === null)
            dlg.width(Math.min(Math.max(self._footerWidth, self._getGridWidth()), $(window).width()-dlg.offset().left-5));

        //console.log('dlg pos '+dlg.offset().left+','+dlg.offset().top);
    },

    replaceRows: function(_rows) {
        if($.isArray(_rows) && _rows.length > 0) {
            var self = this;
            self._gdata = [];
            self._grid.setData(self._gdata);
            self.replaceRows(_rows);
        }
    },

    // public function for loading rows
    // used by _ajaxCall but can also be used directly
    appendRows: function(_rows) {
        if($.isArray(_rows) && _rows.length > 0) {

            var c = this._grid.getColumns();
            var firstChunk = (this._gdata.length === 0);
            // only if first of the rows are atleast arrays and
            // they are not greater than number of columns including the index column
            if($.isArray(_rows[0]) && _rows[0].length <= c.length) {
                var self = this;
                var dlg = this._dlg.dialog('widget');
                var isIdMissing = (_rows[0].length === c.length ? false : true);

                var start = (new Date()).getTime();
                console.log('rows loading to slick...');

                for(var i=0;i<_rows.length;++i) {
                    var starRowLoad = (new Date()).getTime();

                    if(isIdMissing) // add the missing id field
                        _rows[i].splice(0,0,self._gdata.length+1);
                    var row = {};
                    for(var j=0;j<c.length;++j) {
                        // adjust columns only the first time
                        if (firstChunk) {
                            var str = _rows[i][j];
                            var fieldWidth = self._txtlen.text(str).width();
                            //var fieldWidth = str.visualLength();
                            fieldWidth = fieldWidth + 0.4 * fieldWidth;
                            if(c[j].width < fieldWidth) {
                                c[j].width = fieldWidth;
                                if (c[j].width > self._MAX_ROW_WIDTH)
                                    c[j].width = self._MAX_ROW_WIDTH;
                            }
                        }
                        row[c[j].field] = _rows[i][j];
                    }
                    firstChunk && self._grid.setColumns(c);
                    self._gdata.push(row);
                    self._grid.updateRowCount();
                    self._grid.invalidateRow(self._gdata.length-1);

                    // -- // rendering is an expensive operation so done intermittently
                    // -- if (i % 10 === 0) {
                    self._grid.render();
                    self._grid.scrollRowIntoView(self._gdata.length-1);
                    // -- }

                    //console.log('row '+row.id+' loaded in ' + ((new Date()).getTime() - starRowLoad) + 'ms');
                }

                self._grid.resizeCanvas();

                // only if the dialog don't have a predefined height/width
                if(self._tbllay === null && self._clmlay === null) {
                    // since columns' width doesn't change after the first block we can skip this
                    if (firstChunk || self._dlgResized) {
                        var gWidth = self._getGridWidth();
                        dlg.width(Math.min(Math.max(self._footerWidth, gWidth), $(window).width()-dlg.offset().left-10));
                        // adjusting the column to fill the rest of the window
                        // in case if gWidth is less than _footerWidth (set as minWidth of dlg)
                        if(gWidth < dlg.width()) {
                            c[c.length - 1].width += (dlg.width()-gWidth-10);
                            self._grid.setColumns(c);
                        }
                        self._dlgResized = false;
                    }

                    // console.log('column width adjusted in ' + ((new Date()).getTime() - start) + 'ms');
                    // start = (new Date()).getTime();

                    self._grid.resizeCanvas();

                    // dlg height to display max number of rows
                    var gHeight = self._getGridHeight();
                    this._dlg.height(Math.min(Math.min(dlg.height(), gHeight)+10, $(window).height()-dlg.offset().top-2*self.options.toolBarHeight));
                }

                // 
                // loading of rows is the costiliest of the operations
                // compared to computing and adjusting the table width/height
                // (so for now total time of function entry/exit is appromately equal to only row loading)
                //
                //console.log('dlg height adjusted in ' + ((new Date()).getTime() - start) + 'ms');
                console.log('rows loading completed in ' + ((new Date()).getTime() - start) + 'ms');
            }
        }
    }

  });
}( jQuery ) );
