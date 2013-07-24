(function( $ ) {

  $.widget( "dderl.table", $.ui.dialog, {

    _dlg            : null,
    _tableDiv       : null,
    _footerDiv      : null,
    _footerWidth    : 0,
    _grid           : null,
    _gdata          : null,
    _txtlen         : null,
    _fnt            : null,
    _fntSz          : null,
    _MAX_ROW_WIDTH  : 1000,

    _url            : null,
    _stmt           : null,
    _conn           : null,
    _session        : null,
    _adapter        : null,
    _cmd            : null,
    _clmlay         : null,
    _tbllay         : null,
    _origcolumns    : null,

    _fetchIsTail    : false,
    _fetchIsPush    : false,

    _dlgResized     : false,

    // sort and filter
    _sorts          : null,
    _filters        : null,
    _fltrDlg        : null,
    _INIT_WAIT_TAIL : 5,
    _rftchExpBkOff  : 5,
    _MAX_WAIT_TAIL  : 1000,

    // start button
    _startBtn       : null,
    _cmdStrs        : null,
    _divSqlEditor   : null,

    // for edit erlang terms
    _erlangCellPos  : null,
    _divDisable     : null,

    // pause continue tail on edition of cells
    _editedText     : null,
    _editorEscaped  : false,
    _loop           : "",

    // flag to avoid multiple calls to reorder
    _reorderCalled  : false,

    // private event handlers
    _handlers       : { loadViews       : function(e, _result) { e.data._renderViews(_result); },
                        browseData      : function(e, _result) { e.data._renderNewTable(_result); },
                        queryResult     : function(e, _result) { e.data._renderTable(_result); },
                        tailResult      : function(e, _result) { e.data._checkTailResult(_result); },
                        updateData      : function(e, _result) { e.data._checkUpdateResult(_result); },
                        insertData      : function(e, _result) { e.data._insertResult(_result); },
                        deleteData      : function(e, _result) { e.data._deleteResult(_result); },
                        commitResult    : function(e, _result) { e.data._checkCommitResult(_result); },
                        stmtCloseResult : function(e, _result) { e.data._checkStmtCloseResult(_result); },
                        saveViewResult  : function(e, _result) { e.data._checkSaveViewResult(_result); },
                        loadRows        : function(e, _result) { e.data._renderRows(_result); },
                        filterResult    : function(e, _result) { e.data._renderRows(_result); },
                        sortResult      : function(e, _result) { e.data._renderRows(_result); },
                        reorderResult   : function(e, _result) { e.data._renderRows(_result); },
                        editErlangTerm  : function(e, _result) { e.data._openErlangTermEditor(_result); }
                      },

    _toolbarButtons : {'restart'  : {tip: 'Reload',                typ : 'btn', icn : 'arrowrefresh-1-e', clk : '_toolBarReload',   dom: '_tbReload' },
                       '|<'       : {tip: 'Move to first',         typ : 'btn', icn : 'seek-first',       clk : '_toolBarSkFrst',   dom: '_tbSkFrst' },
                       '<<'       : {tip: 'Jump to previous page', typ : 'btn', icn : 'seek-prev',        clk : '_toolBarJmPrev',   dom: '_tbJmPrev' },
                       '<'        : {tip: 'Previous page',         typ : 'btn', icn : 'rev-play',         clk : '_toolBarGo2Prv',   dom: '_tbGoPrev' },
                       'textBox'  : {tip: '',                      typ : 'txt',                           clk : '_toolBarTxtBox',   dom: '_tbTxtBox' },
                       '>'        : {tip: 'Next page',             typ : 'btn', icn : 'play',             clk : '_toolBarGo2Nex',   dom: '_tbGoNext' },
                       '>>'       : {tip: 'Jump to next page',     typ : 'btn', icn : 'seek-next',        clk : '_toolBarJmNext',   dom: '_tbJmNext' },
                       '>|'       : {tip: 'Move to end',           typ : 'btn', icn : 'seek-end',         clk : '_toolBarSekEnd',   dom: '_tbSekEnd' },
                       '>|...'    : {tip: 'Move to end then Tail', typ : 'btn', icn : 'fetch-tail',       clk : '_toolBarSkTail',   dom: '_tbSkTail' },
                       '...'      : {tip: 'Skip to end and Tail',  typ : 'btn', icn : 'fetch-only',       clk : '_toolBarSkipTl',   dom: '_tbSkipTl' },
                       'commit'   : {tip: 'Commit changes',        typ : 'btn', icn : 'check',            clk : '_toolBarCommit',   dom: '_tbCommit' },
                       'rollback' : {tip: 'Discard changes',       typ : 'btn', icn : 'close',            clk : '_toolBarDiscrd',   dom: '_tbDiscrd' }},

    // dialog context menus
    _dlgTtlCnxtMnu  : {'Edit SQL'       : '_editCmd',
                       'Save View'      : '_saveView',
                       'Save View As'   : '_saveViewAs'},

    // slick context menus
    _slkHdrCnxtMnu  : {'Hide'             : '_hide',
                       'UnHide'           : '_unhide',
                       'Filter...'        : '_filterColumn',
                       'Filter Clear'     : '_filterClear',
                       'Sort...'          : '_sort',
                       'Sort ASC'         : '_sortAsc',
                       'Sort DESC'        : '_sortDesc',
                       'Sort Clear'       : '_sortClear'},
    _slkCellCnxtMnu : {'Browse Data'      : '_browseCellData',
                       'Filter'           : '_filter',
                       'Edit Erlang Term' : '_editErlangTerm'},

    // These options will be used as defaults
    options: {
        // dialog options default override
        toolBarHeight     : 22,
        height            : 200,
        width             : 200,
        minHeight         : 50,
        minWidth          : 100,
        resizable         : true,
        modal             : false,
        title             : "_Set TITLE here_",
        canMinimize       : true,
        canMaximize       : true,
        closeOnEscape     : false,
        clear             : null,
        toolBarHeight     : 20,
        open              : function(e,ui) {
                              $(this).dialog("widget").appendTo("#main-body");
                            },
        focus             : function(e,ui) {},
        close             : function() {
                              $(this).table('close_stmt');
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
                              rowHeight: 20,
                              editorLock: null
                            },

        // dderl options
        dderlConn         : null,
        dderlAdapter      : null,
        dderlSession      : null,
        dderlStatement    : null,
        dderlCmd          : null,
        dderlCmdStrs      : null,
        dderlClmlay       : null,
        dderlTbllay       : null,
        dderlStartBtn     : '>',
        dderlSortSpec     : null,
        dderlSqlEditor    : null,
    },
 
    // Set up the widget
    _create: function() {
        var self = this;

        self._origcolumns = {};
        self._gdata = [];

        self._fnt = $(document.body).css('font-family');
        self._fntSz = $(document.body).css('font-size');

        // preserve some options
        if(self.options.dderlConn       !== self._conn)     self._conn      = self.options.dderlConn;
        if(self.options.dderlAdapter    !== self._adapter)  self._adapter   = self.options.dderlAdapter;
        if(self.options.dderlSession    !== self._session)  self._session   = self.options.dderlSession;
        if(self.options.dderlStatement  !== self._stmt)     self._stmt      = self.options.dderlStatement;
        if(self.options.dderlCmd        !== self._cmd)      self._cmd       = self.options.dderlCmd;
        if(self.options.dderlCmdStrs    !== self._cmdStrs)  self._cmdStrs   = self.options.dderlCmdStrs;
        if(self.options.dderlClmlay     !== self._clmlay)   self._clmlay    = self.options.dderlClmlay;
        if(self.options.dderlTbllay     !== self._tbllay)   self._tbllay    = self.options.dderlTbllay;
        if(self.options.dderlStartBtn   !== self._startBtn) self._startBtn  = self.options.dderlStartBtn;
        if(self.options.dderlSortSpec   !== self._sorts)    self._sorts     = self.options.dderlSortSpec;
        if(self.options.dderlSqlEditor  !== self._divSqlEditor) {
            self._divSqlEditor = self.options.dderlSqlEditor;
        }

        // editor lock private to this table.
        if(!self.options.slickopts.editorLock) {
            self.options.slickopts.editorLock = new Slick.EditorLock();
        }

        // initialize the array containing the history if was not set in the options
        if(!self._cmdStrs) self._cmdStrs = [];

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
        self._tableDiv = $('<div>')
            .css('position', 'absolute')
            .css('top', '0')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', self.options.toolBarHeight+'px')
            .css('border-style', 'solid')
            .css('border-width', '1px')
            .css('border-color', 'lightblue')
            .appendTo(self.element);

        // toolbar container
        self._footerDiv = $('<div>').appendTo(self.element);

        // need the max footer with to set as dlg minWidth
        self._createDlgFooter();
        self._createDlg();

        self._createSlickGrid();
        self._createContextMenus();

        // setting up the event handlers last to aid debuggin
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
            for(var m in this[_menu]) {
                if($.type(this[_menu][m]) === "string") {
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
                }
            }
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
        var self = this;
        if(!self._divSqlEditor || !self._divSqlEditor.hasClass('ui-dialog-content')) {
            self._divSqlEditor = $('<div>')
                .appendTo(document.body)
                .sql({autoOpen  : false,
                      title     : this.options.title,
                      cmdOwner  : this._dlg,
                      history   : this._cmdStrs,
                      cmdFlat   : this._cmd,
                     })
                .sql('open');
        }
    },

    _addToEditorHistory: function(sql) {
        var self = this;
        var posInHistory = self._cmdStrs.indexOf(sql);
        var exist = (posInHistory !== -1);
        if(!exist) {
            self._cmdStrs.unshift(sql);
        }
        if(self._divSqlEditor && self._divSqlEditor.hasClass('ui-dialog-content')) {
            if(!exist) {
                self._divSqlEditor.sql("addToHistorySelect", sql);
            } else {
                self._divSqlEditor.sql("selHistorySelect", posInHistory, sql);
            }
        }
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

    _getTableLayout: function(_viewName) {
        var colnamesizes = new Array();
        var cols = this._grid.getColumns();
        for(var idx = 1; idx < cols.length; ++idx) {
            var newColName = cols[idx].name + "_" + idx;
            colnamesizes.push({
                name: newColName,
                width: cols[idx].width,
                hidden: false
            });
        }

        // Table width/height/position
        var w = this._dlg.dialog('widget').width();
        var h = this._dlg.dialog('widget').height();
        var x = this._dlg.dialog('widget').position().left;
        var y = this._dlg.dialog('widget').position().top;
        return {save_view : {table_layout : {width : w,
                                            height : h,
                                                 y : y,
                                                 x : x},
                            column_layout : colnamesizes,
                                     name : _viewName,
                                  content : this._cmd}
               };
    },

    _saveViewWithName: function(_viewName) {
        var self = this;
        // Column names and width.
        // Index starting at 1 to skip the id column.

        saveView = self._getTableLayout(_viewName);

        console.log('saving view '+JSON.stringify(saveView));
        this._ajax('/app/save_view', saveView, 'save_view', 'saveViewResult');
    },

    // Open plot for this table
    _plotTable: function() {
        $('<div>').appendTo(document.body)
            .plotTable({
                autoOpen     : false,
                title        : this.options.title,
                initialQuery : this._cmd,
            })
            .plotTable('open');
    },

    // Reload table: called from the sql editor to refresh this table.
    cmdReload: function(cmd, button) {
        console.log('command reloading ['+cmd+']');
        this._clmlay = null;
        this._cmd = cmd;
        this.options.dderlStartBtn = this._startBtn = button;
        this._ajax('/app/query', {query: {connection: connection, qstr : this._cmd}}, 'query', 'queryResult');
    },

    // columns hide/unhide
    _hide: function(_ranges) {
        var self = this;
        var columns = self._grid.getColumns();
        var toHide = {};
        for (var i=0; i<_ranges.length; ++i) {
            //Validate that id column is not on the selection.
            if(_ranges[i].fromCell !== 0 && _ranges[i].toCell !== 0)  {
                for(var j = _ranges[i].fromCell; j <= _ranges[i].toCell; ++j) {
                    toHide[j] = true;
                }
            }
        }
        var toHideArray = [];
        for(var j in toHide) {
            toHideArray[toHideArray.length] = parseInt(j);
        }
        toHideArray.sort(function(a,b) {return (a < b ? 1 : (a === b ? 0 : -1));});

        if(!self.hasOwnProperty('_hiddenColumns')) {
            self['_hiddenColumns'] = new Array();
        }

        for(var j=0; j < toHideArray.length; ++j) {
            self._hiddenColumns.push(
                {
                    idxCol: toHideArray[j],
                    colContent: columns[toHideArray[j]]
                }
            );
            columns.splice(toHideArray[j],1);
        }
        self._grid.setColumns(columns);
        self._gridColumnsReorder();
    },

    _unhide: function(_ranges) {
        var self = this;
        if(self.hasOwnProperty('_hiddenColumns')) {
            var columns = self._grid.getColumns();
            while(self._hiddenColumns.length > 0) {
                var columnToAdd = self._hiddenColumns.pop();
                columns.splice(columnToAdd.idxCol, 0, columnToAdd.colContent);
            }
            self._grid.setColumns(columns);
            delete self._hiddenColumns;
        }
        self._gridColumnsReorder();
    },

    // sorts
    _sort: function(_ranges) {
        var self = this;
        if(self._sorts === null) {
            self._sorts = new Object();
        }
        var cols = self._grid.getColumns();
        for (var i = 0; i < _ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                if(!self._sorts.hasOwnProperty(cols[c].field)) {
                    self._sorts[cols[c].field] = {name : cols[c].name, asc : true};
                }
            }
        }
        self._showSortGui();
    },
    _sortAsc: function(_ranges) {
        var self = this;
        if(self._sorts === null)
            self._sorts = new Object();
        var cols = this._grid.getColumns();
        for (var i=0; i<_ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                self._sorts[cols[c].field] = {name : cols[c].name, asc : true};
            }
        }
        if(Object.keys(self._sorts).length === 1) {
            self._ajax('/app/sort', {sort: {spec: self._sortSpec2Json(), statement: self._stmt}}, 'sort', 'sortResult');
        } else {
            self._showSortGui();
        }
    },
    _sortDesc: function(_ranges) {
        var self = this;
        if(self._sorts === null)
            self._sorts = new Object();
        var cols = this._grid.getColumns();
        for (var i = 0; i < _ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                self._sorts[cols[c].field] = {name : cols[c].name, asc : false};
            }
        }
        if(Object.keys(self._sorts).length === 1) {
            self._ajax('/app/sort', {sort: {spec: self._sortSpec2Json(), statement: self._stmt}}, 'sort', 'sortResult');
        } else {
            self._showSortGui();
        }
    },
    _sortClear: function(_ranges) {
        var self = this;
        if(self._sorts && !$.isEmptyObject(self._sorts)) {
            var cols = this._grid.getColumns();
            for (var i=0; i<_ranges.length; ++i) {
                var fromCell = Math.max(_ranges[i].fromCell, 1);
                for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                    delete self._sorts[cols[c].field];
                }
            }
        }
        if(self._sorts && !$.isEmptyObject(self._sorts)) {
            self._showSortGui();
        } else {
            self._sorts = null;
            self._ajax('/app/sort', {sort: {spec: [], statement: self._stmt}}, 'sort', 'sortResult');
        }
    },
    _showSortGui: function() {
        var self = this;
        var data = new Array();
        for (var s in self._sorts) {
            data.push({id: s, name: self._sorts[s].name, sort: (self._sorts[s].asc ? 'ASC' : 'DESC')});
        }

        var sortDlg =
            $('<div>')
            .css('width', 500)
            .appendTo(document.body);

        var sortDiv = $('<div>')
            .css('position', 'absolute')
            .css('top', 0)
            .css('left', 0)
            .css('right', 0)
            .css('bottom', 0)
            .css('border-style', 'solid')
            .css('border-width', '1px')
            .css('border-color', 'lightblue')
            .appendTo(sortDlg);

        // building slickgrid
        var sgrid = new Slick.Grid(sortDiv
                , data
                , [ // columns
                      {
                        id: "#",
                        name: "",
                        width: 40,
                        behavior: "selectAndMove",
                        selectable: true,
                        resizable: false,
                        cssClass: "cell-reorder dnd"
                      },
                      {
                        id: "name",
                        name: "Column",
                        field: "name",
                        width: 150,
                        selectable: true,
                        editor: Slick.Editors.Text
                      },
                      {
                        id: "sort",
                        name: "Sort",
                        field: "sort",
                        width: 100,
                        selectable: true,
                        cannotTriggerInsert: true
                      },
                      {
                        id: "select",
                        name: "",
                        field: "select",
                        width: 40,
                        selectable: true,
                        formatter: Slick.Formatters.Checkmark
                      }
                    ]
                , {
                    editable: true,
                    enableAddRow: false,
                    enableCellNavigation: true,
                    autoEdit: false
                  }
                );

        sgrid.setSelectionModel(new Slick.RowSelectionModel());
        sgrid.onClick.subscribe(function(e, args) {
            var col = sgrid.getColumns()[args.cell].id
            switch (col) {
                case "select": // delete the row
                    sgrid.getData().splice(args.row, 1);
                    sgrid.updateRowCount();
                    sgrid.render();
                    break;
                case "sort": // changing asc/desc
                    var sgd = sgrid.getData();
                    sgd[args.row][col] = (sgd[args.row][col] === 'ASC' ? 'DESC' : 'ASC');
                    sgrid.invalidateRow(args.row);
                    sgrid.render();
                    break;
                default:
                    console.log('nothing happnes in this column');
                    break;
            }
        });
        
        var moveRowsPlugin = new Slick.RowMoveManager({
          cancelEditOnDrag: true
        });

        moveRowsPlugin.onBeforeMoveRows.subscribe(function (e, data) {
          for (var i = 0; i < data.rows.length; i++) {
            // no point in moving before or after itself
            if (data.rows[i] == data.insertBefore || data.rows[i] == data.insertBefore - 1) {
              e.stopPropagation();
              return false;
            }
          }
          return true;
        });

        moveRowsPlugin.onMoveRows.subscribe(function (e, args) {
          var extractedRows = [], left, right;
          var rows = args.rows;
          var insertBefore = args.insertBefore;
          left = data.slice(0, insertBefore);
          right = data.slice(insertBefore, data.length);

          rows.sort(function(a,b) { return a-b; });

          for (var i = 0; i < rows.length; i++) {
            extractedRows.push(data[rows[i]]);
          }

          rows.reverse();

          for (var i = 0; i < rows.length; i++) {
            var row = rows[i];
            if (row < insertBefore) {
              left.splice(row, 1);
            } else {
              right.splice(row - insertBefore, 1);
            }
          }

          data = left.concat(extractedRows.concat(right));

          var selectedRows = [];
          for (var i = 0; i < rows.length; i++)
            selectedRows.push(left.length + i);

          sgrid.resetActiveCell();
          sgrid.setData(data);
          sgrid.setSelectedRows(selectedRows);
          sgrid.render();
        });

        sgrid.registerPlugin(moveRowsPlugin);

        var saveChange = function() {
            var sd = sgrid.getData();
            self._sorts = new Object();
            for (var i = 0; i < sd.length; ++i) {
                self._sorts[sd[i].id] = {name : sd[i].name,
                                         asc : (sd[i].sort === 'ASC' ? true : false) };
            }
            return self._sortSpec2Json();
        }
        
        sortDlg.dialog({
            width : 336,
            modal : false,
            title : 'Sorts',
            close : function() {
                saveChange();
                $(this).dialog('close');
                $(this).remove();
            },
            buttons: {
                'Sort' : function() {
                    var sortspec = saveChange();
                    self._ajax('/app/sort', {sort: {spec: sortspec, statement: self._stmt}}, 'sort', 'sortResult');
                    $(this).dialog('close');
                    $(this).remove();
                }
            }
        });

        sortDlg.dialog("widget").draggable("option", "containment", "#main-body");
        sortDlg.dialog("widget").appendTo("#main-body");
        //Lets put it where we have space...
        smartDialogPosition($("#main-body"), this._dlg, sortDlg, ['bottom','right','left','top','right']);
    },

    _ajax: function(url, data, resp, callback) {
        if(this._dlg.hasClass('ui-dialog-content')) {
            this._setTitleHtml($(this._dlg.dialog('option', 'title')).addClass('table-title-wait'));
        }
        ajaxCall(this, url, data, resp, callback);
    },
    
    _sortSpec2Json: function() {
        var self = this;
        var sortspec = new Array();
        for (var s in self._sorts) {
            var t = new Object();
            if(self._origcolumns.hasOwnProperty(s)) {
                t[self._origcolumns[s]] = self._sorts[s].asc;
            } else {
                t[s] = self._sorts[s].asc;
            }
            sortspec.push(t);
        }
        return sortspec;
    },
    
    _setSortSpecFromJson: function(self, origJson) {
        if(self._sorts === null) {
            self._sorts = new Object();
        }

        var cols = self._grid.getColumns();
        for (colpos in origJson) {
            var col_id = origJson[colpos].id;
            if(col_id === -1) {
                self._sorts[colpos] = {name : colpos, asc : origJson[colpos].asc};
            } else {
                self._sorts[cols[col_id].field] = {name : cols[col_id].name, asc : origJson[colpos].asc};
            }
        }
    },

    // filters
    _filterColumn: function(_ranges) {
        var self = this;
        var cols = self._grid.getColumns();
        if(self._filters === null) {
            self._filters = new Object();
        }
        for (var i=0; i<_ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                if(!self._filters.hasOwnProperty(cols[c].field)) {
                    self._filters[cols[c].field] =
                        {
                            inp : $('<textarea>')
                                .attr('type', "text")
                                .css('margin', 0)
                                .css('white-space','nowrap')
                                .css('overflow','auto')
                                .css('padding', 0),
                            vals: new Object(),
                            name: cols[c].name
                        };
                }
            }
        }
        self._showFilterGui();
    },
    _filterClear: function(_ranges) {
        var self = this;
        if(self._filters) {
            var cols = self._grid.getColumns();
            for (var i = 0; i < _ranges.length; ++i) {
                var fromCell = Math.max(_ranges[i].fromCell, 1);
                for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                    delete self._filters[cols[c].field];
                }
            }
        }
        if(self._filters && !$.isEmptyObject(self._filters)) {
            self._showFilterGui();
        } else {
            self._filters = null;
            self._ajax('/app/filter', {filter: {spec: {'undefined':[]}, statement: self._stmt}}, 'filter', 'filterResult');
        }
    },
    _filter: function(_ranges) {
        var self = this;
        if(self._filters === null) {
            self._filters = new Object();
        }
        var cols = this._grid.getColumns();
        for (var i = 0; i < _ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                if(!self._filters.hasOwnProperty(cols[c].field)) {
                    self._filters[cols[c].field] =
                        {
                            inp : $('<textarea>')
                                .attr('type', "text")
                                .css('margin', 0)
                                .css('white-space','nowrap')
                                .css('overflow','auto')
                                .css('padding', 0),
                            vals: new Object(),
                            name: cols[c].name
                        };
                }
                for(var r=_ranges[i].fromRow; r <= _ranges[i].toRow; ++r) {
                    self._filters[cols[c].field].vals[this._gdata[r][cols[c].field].replace(/\n/g,'\\n')] = true;
                }
            }
        }
        if(Object.keys(self._filters).length === 1) {
            for(var c in self._filters) {
                var strs = [];
                for(s in self._filters[c].vals) strs.push(s);
                self._filters[c].inp.val(strs.join('\n'));
            }
            var filterspec = self._filterSpec2Json('and');
            self._ajax('/app/filter', {filter: {spec: filterspec, statement: self._stmt}}, 'filter', 'filterResult');
        } else {
            self._showFilterGui();
        }
    },
    _showFilterGui: function() {
        var self = this;
        // first check if we have a filter open and close it.
        if(self._fltrDlg && self._fltrDlg.hasClass('ui-dialog-content')) {
            self._fltrDlg.dialog("close");
        }

        // number of current filters
        var fCount = 0;
        for (var c in self._filters)
            fCount++;

        self._fltrDlg =
            $('<div>')
            .css('width', 336)
            .appendTo(document.body);

        var fltrTbl =
            $('<table>')
            .css('height', '100%')
            .css('width', '100%')
            .attr('border', 0)
            .attr('cellpadding', 0)
            .attr('cellspacing', 0)   
            .appendTo(self._fltrDlg);
        for(var c in self._filters) {
            var strs = [];
            for(s in self._filters[c].vals) strs.push(s);
            self._filters[c].inp.val(strs.join('\n'));
            $('<tr>')
                .append($('<td>'))
                .append('<td>'+ self._filters[c].name +'</td>')
                .appendTo(fltrTbl);
            $('<tr>')
                .append('<td>in&nbsp;</td>')
                .append($('<td>').append(self._filters[c].inp))
                .appendTo(fltrTbl);
        }

        self._fltrDlg
            .dialog({
                width: fltrTbl.width(),
                modal: false,
                title:'Filter',
                position: { my: "left top", at: "left bottom", of: this._dlg },
                close : function() {
                        $(this).dialog('close');
                        $(this).remove();
                    },
                resize: function(e, ui) {
                    var dH = $(this).height() / fCount - 30;
                    var dW = $(this).width() - 30;
                    for(var c in self._filters) {
                        self._filters[c].inp.width(dW);
                        self._filters[c].inp.height(dH);
                    }
                }
            });

        self._fltrDlg.dialog("widget").draggable("option","containment", "#main-body");
        self._fltrDlg.dialog("widget").appendTo("#main-body");
        //Lets put it where we have space...
        smartDialogPosition($("#main-body"), this._dlg, self._fltrDlg, ['bottom','right','left','top','right']);

        var applyFiltersFn = function(type) {
            var filterspec = self._filterSpec2Json(type);
            self._ajax('/app/filter', {filter: {spec: filterspec, statement: self._stmt}}, 'filter', 'filterResult');
            $(this).dialog('close');
            $(this).remove();
        };

        var buttons = [];
        if(fCount > 1) {
            buttons = [
                { text: 'Match AND', click: function() { applyFiltersFn.apply(this, ['and']);}},
                { text: 'Match OR', click: function() { applyFiltersFn.apply(this, ['or']);}}
            ];
        } else {
            buttons = [
                { text: 'Apply', click: function() { applyFiltersFn.apply(this, ['and']);}}
            ];
        }
        self._fltrDlg.dialog('option', 'buttons', buttons);

        var dH = self._fltrDlg.height() / fCount - 30;
        var dW = self._fltrDlg.width() - 30;
        for(var c in self._filters) {
            self._filters[c].inp.width(dW);
            self._filters[c].inp.height(dH);
        }
    },
    _filterSpec2Json: function(type) {
        var self = this;
        var filterspec = new Object();
        filterspec[type] = new Array();
        for(var c in self._filters) {
            var _vStrings = self._filters[c].inp.val().split('\n');
            if (_vStrings.length === 1 && _vStrings[0].length === 0) _vStrings = [];
            var vStrings = new Array();
            self._filters[c].vals = new Object();
            for (var i=0; i<_vStrings.length; ++i) {
                vStrings[i] = _vStrings[i].replace(/\\n/g,'\n');
                self._filters[c].vals[_vStrings[i]] = true;
            }
            var fltr = new Object();
            fltr[self._origcolumns[c]] = vStrings;
            if(vStrings.length > 0) {
                filterspec[type].push(fltr);
            }
            else delete self._filters[c];
        }
        if (filterspec[type].length === 0) {
            delete filterspec[type];
            filterspec['undefined'] = [];
        }
        return filterspec;
    },
    
    // browse_data actions
    _browseCellData: function(_ranges) {
        var self = this;
        
        console.log('_browseCellData for '+_ranges.length+' slick range(s)');

        // test the range and throw unsupported exceptions
        if(_ranges.length > 2 || _ranges.length >= 2 && (!(
                _ranges[0].fromRow  === _ranges[0].toRow  && // single cell
                _ranges[0].fromCell === _ranges[0].toCell &&
                _ranges[1].fromRow  === _ranges[1].toRow  && // single cell
                _ranges[1].fromCell === _ranges[1].toCell &&
                _ranges[1].fromCell === _ranges[0].toCell && // same cell
                _ranges[1].fromRow  === _ranges[0].toRow)))
                throw('cell level \'Browse Data\' don\'t support multiples and ranges');
        else {
            var cell    = _ranges[0];
            var column  = self._grid.getColumns()[cell.fromCell];
            var data    = self._gdata[cell.fromRow];
            // console.log('browse_data @ '+column.name+' val '+JSON.stringify(data));
            self._ajax('/app/browse_data',
                           { browse_data: {connection : connection,
                                            statement : self._stmt,
                                                  row : data.id, //cell.fromRow,
                                                  col : this._origcolumns[column.field]}},
                           'browse_data', 'browseData');
        }
    },

    _editErlangTerm: function (_ranges) {
        var self = this;
        if(_ranges.length > 2 || _ranges.length >= 2 && (!(
                _ranges[0].fromRow  === _ranges[0].toRow  && // single cell
                _ranges[0].fromCell === _ranges[0].toCell &&
                _ranges[1].fromRow  === _ranges[1].toRow  && // single cell
                _ranges[1].fromCell === _ranges[1].toCell &&
                _ranges[1].fromCell === _ranges[0].toCell && // same cell
                _ranges[1].fromRow  === _ranges[0].toRow))) {
            throw('cell level \'Edit Erlang Term\' don\'t support multiples and ranges');
        } else {
            var cell = _ranges[0];
            var columnField = self._grid.getColumns()[cell.fromCell].field;
            var stringToFormat = unescapeNewLines(self._gdata[cell.fromRow][columnField]);
            self._erlangCellPos = {row: cell.fromRow, cell: cell.fromCell};
            self._ajax('/app/format_erlang_term', {
                format_erlang_term: {
                    erlang_term: stringToFormat,
                    expansion_level: "auto",
                    force: false
                }
            }, 'format_erlang_term', 'editErlangTerm');
        }
    },

    _createSlickGrid: function() {
        var self = this;

        // building slickgrid
        // a dummy column needed to be added to enable slickgrid to enable column re-order
        self._grid = new Slick.Grid(self._tableDiv, [], [{id: "_"}], self.options.slickopts);
        self._grid.setSelectionModel(new Slick.CellRowColSelectionModel());
        var copyManager = new Slick.CellExternalCopyManager();
        self._grid.registerPlugin(copyManager);
        copyManager.onPasteCells.subscribe($.proxy(self._gridPasteCells, self));

        self._grid.onContextMenu.subscribe($.proxy(self._gridContextMenu, self));
        self._grid.onHeaderContextMenu.subscribe($.proxy(self._gridHeaderContextMenu, self));
        self._grid.onCellChange.subscribe($.proxy(self._gridCellChange, self));
        self._grid.onBeforeEditCell.subscribe($.proxy(self._gridBeforeEdit, self));
        self._grid.onBeforeCellEditorDestroy.subscribe($.proxy(self._gridAfterEdit, self));
        self._grid.onAddNewRow.subscribe($.proxy(self._gridAddNewRow, self));
        self._grid.onColumnsReordered.subscribe($.proxy(self._gridColumnsReorder, self));
        self._grid.onKeyDown.subscribe($.proxy(self._handleKeyDown, self));
        self._grid.onClick.subscribe($.proxy(self._handleClick, self));
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
        for(btn in self._toolbarButtons) {
            var btnTxt = self._toolbarButtons[btn].tip;
            var elm = self._toolbarButtons[btn];

            var toolElmFn = function(e) {
                var self = e.data;
                var _btn = $(this).data('tag');
                var fName = self._toolbarButtons[_btn].clk;
                //var f = $.proxy(self[fName], self);
                var f = self[fName];
                if($.isFunction(f)) {
                    f(self);
                    self._grid.focus();
                } else {
                    throw('['+self.options.title+'] toolbar '+_btn+' has unimplimented cb '+fName);
                }
            };

            var inph = self.options.toolBarHeight;
            if($.browser.msie) inph -= 2;

            if(elm.typ === 'btn')
                self[elm.dom] =
                    $('<button>')
                    .text(btnTxt)
                    .data('tag', btn)
                    .button({icons: {primary: 'ui-icon-' + elm.icn}, text: false})
                    .css('height', inph+'px')
                    .click(self, toolElmFn)
                    .appendTo(self._footerDiv);
            else if(elm.typ === 'txt')
                self[elm.dom] =
                $('<input>')
                    .attr('type', 'text')
                    .attr('size', 10)
                    .data('tag', btn)
                    .button()
                    .addClass('tb_empty')
                    .css('height', (inph-2)+'px')
                    .css('text-align', 'right')
                    .css('padding', '0')
                    .css('margin', '0px -1px 0px 0px')
                    .keypress(function(evt) {
                        if(evt.which == 13) {
                            var rownum = parseInt($(this).val());
                            if(rownum != NaN) {
                                self["_toolBarTxtBoxVal"] = rownum;
                                evt.data = self;
                                toolElmFn.call(this, evt);
                            }
                        }
                        return true;
                    })
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
        var viewInfo = self._getTableLayout("");
        self._clmlay = viewInfo.column_layout;
        self._tbllay = viewInfo.table_layout;
        self.buttonPress("restart");
    },

    _toolBarSkFrst: function(self) {
        self.buttonPress("|<");
        //console.log('['+self.options.title+'] cb _toolBarSkFrst');
    },
    _toolBarJmPrev: function(self) {
        self.buttonPress("<<");
        console.log('['+self.options.title+'] cb _toolBarJmPrev');
    },
    _toolBarGo2Prv: function(self) {
        console.log('['+self.options.title+'] cb _toolBarGo2Prv');
        self.buttonPress("<");
    },
    _toolBarTxtBox: function(self) {
        if(self.hasOwnProperty('_toolBarTxtBoxVal')) {
            console.log('['+self.options.title+'] cb _toolBarTxtBox '+self._toolBarTxtBoxVal);
            self.buttonPress(self._toolBarTxtBoxVal);
        }
    },
    _toolBarGo2Nex: function(self) {
        console.log('['+self.options.title+'] cb _toolBarGo2Nex');
        self.buttonPress(">");
    },
    _toolBarJmNext: function(self) {
        self.buttonPress(">>");
        console.log('['+self.options.title+'] cb _toolBarJmNext');
    },
    _toolBarSekEnd: function(self) {
        console.log('['+self.options.title+'] cb _toolBarSekEnd');
        self.buttonPress(">|");
    },
    _toolBarSkTail: function(self) {
        console.log('['+self.options.title+'] cb _toolBarSkTail');
        self._rftchExpBkOff = self._INIT_WAIT_TAIL;
        self.buttonPress(">|...");
    },
    _toolBarSkipTl: function(self) {
        console.log('['+self.options.title+'] cb _toolBarSkipTl');
        self._rftchExpBkOff = self._INIT_WAIT_TAIL;
        self.buttonPress("...");
    },
    _toolBarCommit: function(self) {
        console.log('['+self.options.title+'] cb _toolBarCommit');
        self.buttonPress("commit");
    },
    _toolBarDiscrd: function(self) {
        console.log('['+self.options.title+'] cb _toolBarDiscrd');
        self.buttonPress("rollback");
    },
    ////////////////////////////
    
    /*
     * ajaxCall success callbacks
     */
    _checkTailResult: function(_tail) {
        console.log('[AJAX] tail resp '+JSON.stringify(_tail));
        if(_tail === 'ok') {
            this.fetchRows(OpsFetchEnum.NEXT, parseInt(this._gdata[this._gdata.length-1].id)+1);
        }
    },
    _checkUpdateResult: function(_update) {
        this.appendRows(_update);
        /* console.log('[AJAX] update_data resp '+JSON.stringify(_update));        
        if(_update === 'ok') {
            console.log('update success');
        } else {
            if(_update.hasOwnProperty('error'))
                alert_jq('update failed!\n'+_update.error);
        }*/
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
        if(_saveView === "ok") {
            console.log('[AJAX] view saved!');
            this._setTitleHtml($(this._dlg.dialog('option', 'title')).removeClass('table-title-wait'));
        }
        else if(_saveView.hasOwnProperty('error'))
            alert_jq('failed to save view!\n'+_saveView.error);
    },
    _insertResult: function(_insert) {
        this.appendRows(_insert);
    },

    _deleteResult: function(_delete) {
        this.appendRows(_delete);
        console.log('deleted '+JSON.stringify(_delete));
    },

    _renderViews: function(_views) {
        this._cmd    = _views.content;
        this._stmt   = _views.statement;
        this._conn   = _views.connection;
        if(_views.hasOwnProperty('column_layout') && _views.column_layout.length > 0) {
            this._clmlay = _views.column_layout;
            if(_views.column_layout.length === 0) this._clmlay = null;
        }
        if(_views.hasOwnProperty('table_layout')  && _views.table_layout.length  > 0) {
            this._tbllay = _views.table_layout;
            if(_views.table_layout.length  === 0) this._tbllay = null;
        }
        this._setTitleHtml($('<span>').text(_views.name).addClass('table-title'));
        this.options.title = _views.name;
        console.log('>>>>> table '+_views.name+' '+_views.connection);
        if(_views.hasOwnProperty('error'))
            alert_jq(_views.error);
        else {
            this.setColumns(_views.columns);
            if(_views.hasOwnProperty('sort_spec') && !$.isEmptyObject(_views.sort_spec)) {
                this._setSortSpecFromJson(this, _views.sort_spec);
            }
            this.buttonPress(this._startBtn);
        }
    },
    _renderTable: function(_table) {
        if(_table.hasOwnProperty('result') && _table.result === 'ok') {
            console.log('[_renderTable] no row query, closing dialog');
            this._dlg.dialog('close');
            return;
        }
        if(_table.hasOwnProperty('error')) {
            console.error('[_renderTable] missing statement - '+_table);
            this._dlg.dialog('close');
            alert_jq(_table.error);
            return;
        }
        if(!_table.hasOwnProperty('statement')) {
            console.error('[_renderTable] missing statement handle - '+_table);
            this._dlg.dialog('close');
            alert_jq('missing statement handle');
            return;
        }
        this._stmt = _table.statement;
        this._conn = _table.connection;
        if(_table.hasOwnProperty('column_layout') && _table.column_layout.length > 0) {
            this._clmlay = _table.column_layout;
            if(_table.column_layout.length === 0) this._clmlay = null;
        }
        if(_table.hasOwnProperty('table_layout')  && _table.table_layout.length  > 0) {
            this._tbllay = _table.table_layout;
            if(_table.table_layout.length  === 0) this._tbllay = null;
        }
        if(_table.hasOwnProperty('columns')) {
            this.setColumns(_table.columns);
            if(_table.hasOwnProperty('sort_spec') && !$.isEmptyObject(_table.sort_spec)) {
                this._setSortSpecFromJson(this, _table.sort_spec);
            }
            this._grid.setData([]);
            this._gdata = this._grid.getData();
            this.buttonPress(this._startBtn);
        } else {
            console.log('[_renderTable] missing columns - '+_table);
            alert_jq('missing columns');
            return;
        }
        console.log('>>>>> table '+_table.name+' '+_table.connection);
    },
    _renderNewTable: function(_table) {
        var tl = null;
        var cl = null;

        if(_table.hasOwnProperty('error')) {
            alert_jq(_table.error);
            return;
        }

        var pos = [];
        if(!_table.hasOwnProperty('table_layout') || !_table.table_layout.hasOwnProperty('x')) {
            var dlg = this._dlg.dialog('widget');
            var titleBarHeight = $(dlg.find('.ui-dialog-titlebar')[0]).height();
            pos = [dlg.position().left + titleBarHeight + 10, dlg.position().top + titleBarHeight + 10]
        } else {
            pos = [_table.table_layout.x, _table.table_layout.y];
            tl = _table.table_layout;
        }

        if(_table.hasOwnProperty('column_layout') && _table.column_layout.length > 0)
            cl = _table.column_layout;


        this._setTitleHtml($(this._dlg.dialog('option', 'title')).removeClass('table-title-wait'));
        var baseOptions = {
            autoOpen        : false,
            title           : _table.name,
            position        : pos,

            dderlAdapter    : this._adapter,
            dderlSession    : this._session,
            dderlConn       : this._conn,
            dderlStatement  : _table.statement,
            dderlCmd        : _table.content,
            dderlClmlay     : cl,
            dderlTbllay     : tl,
            dderlSortSpec   : ((_table.hasOwnProperty('sort_spec') && _table.sort_spec.length > 0)
                                 ? _table.sort_spec : null)
        };
        
        if(_table.hasOwnProperty('table_layout')) {
            if(_table.table_layout.hasOwnProperty('width')) {
                baseOptions.width = _table.table_layout.width;
            }
            if (_table.table_layout.hasOwnProperty('height')) {
                baseOptions.height = _table.table_layout.height;
            }
        }

        $('<div>')
        .appendTo(document.body)
        .table(baseOptions)
        .table('setColumns', _table.columns)
        .table('buttonPress', '>');
    },
    _renderRows: function(_rows) {
        var self = this;
        if(_rows.hasOwnProperty('rows')) {
            //console.log('rows '+ JSON.stringify(_rows.rows));
            console.log('[AJAX] rendering '+ _rows.rows.length+' rows');
            var rowsCount = _rows.rows.length;
            this.appendRows(_rows);

            // if new cmd is not in the list
            if(_rows.sql.length > 0) {
                self._reorderCalled = false;
                self._cmd = _rows.sql;
                self._addToEditorHistory(_rows.sql);
            } else if(rowsCount > 0 && !self._reorderCalled && _rows.loop.length === 0) {
                self._reorderCalled = true;
                self._gridColumnsReorder();
            }

            // command back request
            if(_rows.loop.length > 0) {
                if (self._grid.getCellEditor()) {
                    self._loop = _rows.loop;
                } else if (rowsCount > 0) {
                    console.log(rowsCount+' rows received, retrying '+_rows.loop);
                    this.buttonPress(_rows.loop);
                    this._rftchExpBkOff = this._INIT_WAIT_TAIL;
                } else {
                    this._rftchExpBkOff = (this._rftchExpBkOff * 2);
                    if(this._rftchExpBkOff > this._MAX_WAIT_TAIL) {
                        this._rftchExpBkOff = this._MAX_WAIT_TAIL;
                    }
                    console.log('no rows received, retrying '+_rows.loop+' after '+this._rftchExpBkOff+' ms');
                    setTimeout(function(){self.buttonPress(_rows.loop);}, this._rftchExpBkOff);
                }
            } else {
                this._rftchExpBkOff = 2; // end of command looping received
            }
        }
        else if(_rows.hasOwnProperty('error')) {
            alert_jq(_rows.error);
        }
    },

    _openErlangTermEditor: function(formattedString) {
        var self = this;
        // received response clear wait wheel
        self._setTitleHtml($(self._dlg.dialog('option', 'title')).removeClass('table-title-wait'));

        if(formattedString.hasOwnProperty('error')) {
            alert_jq('Error : '+ formattedString.error);
        } else {
            self.disableDialog();
            var thisIsMyEditor = $('<div>')
                .appendTo(document.body)
                .termEditor(
                    {
                        autoOpen  : false,
                        title     : "Erlang term editor",
                        termOwner : self,
                        container : self._divDisable,
                        term      : formattedString
                    }
                ).termEditor('open');
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
                self._grid.resizeCanvas();
                self._dlgResized = true;
            });

        self._dlg.dialog("widget").draggable("option","containment","#main-body");
        if(self.options.position.length === undefined) {
            self._dlg.dialog( "option", "position", {at : 'left top', my : 'left top', collision : 'none'} );
        }

        // converting the title text to a link
        self._setTitleHtml($('<span>').text(self.options.title).addClass('table-title'));
    },

    // context menus invocation for slickgrid
    _gridContextMenu: function(e, args) {
        e.preventDefault();

        var g           = args.grid;
        var cell        = g.getCellFromEvent(e);

        //Check if we are in a new row.
        if(!g.getData()[cell.row]) {
            return;
        }
        
        var row         = cell.row;
        var column      = g.getColumns()[cell.cell];
        var data        = g.getData()[cell.row][column.field];
        var gSelMdl     = g.getSelectionModel();
        var gSelecteds  = gSelMdl.getSelectedRanges();

        var missing = true;
        for(var i=0; i < gSelecteds.length; ++i) {
            if(gSelecteds[i].contains(cell.row, cell.cell)) {
                missing = false;
                break;
            }
        }
        if(missing) {
            gSelMdl.setSelectedRanges([new Slick.Range(cell.row, cell.cell, cell.row, cell.cell)]);
        }

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

        // right click on non-column zone
        if(args.column === undefined) return;

        var g           = args.grid;
        var col         = g.getColumnIndex(args.column.id);
        var gSelMdl     = g.getSelectionModel();
        var gSelecteds  = gSelMdl.getSelectedRanges();

        var fullCols = [];
        for(var i = 0; i < gSelecteds.length; ++i) {
            if(gSelecteds[i].fullCol) {
                fullCols.push(gSelecteds[i]);
            }
        }

        var found = false;
        for(var i = 0; i < fullCols.length; ++i) {
            if(fullCols[i].contains(0, col)) {
                found = true;
                break;
            }
        }

        if(found) {
            gSelMdl.setSelectedRanges(fullCols);
        } else {
            var lastRow = (g.getDataLength() === 0? 0 : g.getDataLength() -1);
            var newSelection = new Slick.Range(0, col, lastRow, col);
            newSelection.fullCol = true;
            gSelMdl.setSelectedRanges([newSelection]);
        }

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
        if(e) {
            e.stopPropagation();
        }

        var g           = args.grid;
        var modifiedRow = g.getData()[args.row];
        var cols        = g.getColumns();
        var updateJson  = {update_data: {connection  : this._conn,
                                         statement   : this._stmt,
                                         rowid       : parseInt(modifiedRow.id),
                                         cellid      : this._origcolumns[cols[args.cell].field],
                                         value       : modifiedRow[cols[args.cell].field]}};
        this._ajax('/app/update_data', updateJson, 'update_data', 'updateData');
        console.log('changed '+JSON.stringify(updateJson));

        // Update all rows from the selected range
        var selRanges = this._grid.getSelectionModel().getSelectedRanges();
        for(var i=0; i < selRanges.length; ++i)
            for(var ri = selRanges[i].fromRow; ri <= selRanges[i].toRow; ++ri) {
                this._gdata[ri].op = 'upd';
            }
        this._applyStyle();
    },
    _gridAddNewRow: function(e, args) {
        e.stopPropagation();
        var insertJson = {insert_data: {connection  : this._conn,
                                        statement   : this._stmt,
                                        col         : this._origcolumns[args.column.field],
                                        value       : args.item[args.column.id]}};
        //console.log('inserting '+JSON.stringify(args.item));
        this._ajax('/app/insert_data', insertJson, 'insert_data', 'insertData');
    },
    _gridPasteCells: function(e, args) {
        e.stopPropagation();
        var range = args.ranges[0];
        var modifiedRows = new Array();
        var gridData = this._gdata;
        var cols = this._grid.getColumns();
        var toCellSafe = Math.min(range.toCell, cols.length - 1);
        var fromCellSafe = Math.max(range.fromCell, 1);
        var rowsToRemove = new Array();
        for(var i = range.fromRow; i <= range.toRow; ++i) {
            var modifiedCells = new Array();
            for(var j = fromCellSafe; j <= toCellSafe; ++j) {
                var cellValue = gridData[i][cols[j].field];
                if(cellValue) {
                    modifiedCells.push({cellid: this._origcolumns[cols[j].field], value : cellValue});
                }
            }
            var rowId = parseInt(gridData[i].id);
            if(rowId) {
                modifiedRows.push({rowid: rowId, cells: modifiedCells});
            } else {
                rowsToRemove.push(i);
                modifiedRows.push({cells: modifiedCells});
            }
        }
        var pasteJson = {paste_data: {connection : this._conn,
                                      statement  : this._stmt,
                                      rows       : modifiedRows}};
        this._ajax('/app/paste_data', pasteJson, 'paste_data', 'updateData');

        // Update all rows from the selected range
        for(var i = range.fromRow; i <= range.toRow; ++i) {
            gridData[i].op = 'upd';
        }

        // Remove rows that will be later added by the server
        if(rowsToRemove.length > 0) {
            gridData.splice(rowsToRemove[0], rowsToRemove.length);
            this._grid.updateRowCount();
            this._grid.invalidate();
        }
        this._applyStyle();
    },

    _gridBeforeEdit: function(e, args) {
        if(args.item) {
            this._editedText = args.item[args.column.field];
        }
    },

    _gridAfterEdit: function(e, args) {
        var self = this;
        var cell = args.grid.getActiveCell();
        var columnField = args.grid.getColumns()[cell.cell].field;
        var loop = self._loop;
        self._loop = "";
        var cellChanged = false;
        if(self._editorEscaped) {
            self._editorEscaped = false;
        } else if(self._gdata[cell.row]) {
            cellChanged = self._gdata[cell.row][columnField] !== self._editedText;
        } else {
            cellChanged = args.editor.isValueChanged();
        }
        if(!cellChanged && loop) {
            self.buttonPress(loop);
        }
    },

    _handleKeyDown: function(e, args) {
        var keyCode = $.ui.keyCode;
        var col;
        var activeCell;
        if(e.keyCode === 27 && this._loop) { // Esc
            this._editorEscaped = true;
        }

        if(this._grid.getCellEditor()) {
            if(e.keyCode === keyCode.ENTER) {
                e.stopImmediatePropagation();
                do {
                    this._grid.navigateNext();
                    activeCell = this._grid.getActiveCell();
                    if(activeCell) {
                        col = activeCell.cell;
                    }
                } while (activeCell && !this._grid.getColumns()[col].editor);
            } else if(!this._grid.getCellEditor().isFocused()) {
                this._grid.editActiveCell();
            }
            // If we are in edit mode already.
            return;
        } else if(e.keyCode === 46) { // Del
            e.stopImmediatePropagation();
            // Delete all rows from the selected range
            var selRanges = this._grid.getSelectionModel().getSelectedRanges();
            var rids = [];
            for(var i = 0; i < selRanges.length; ++i) {
                for(var ri = selRanges[i].fromRow; ri <= selRanges[i].toRow; ++ri) {
                    if(this._gdata[ri].op !== 'ins') {
                        this._gdata[ri].op = 'del';
                    } else {
                        this._gdata.splice(ri, 1);
                    }
                    rids.push(this._gdata[ri].id);
                }
            }

            // Delete args.row
            var deleteJson = {delete_row: {statement : this._stmt,
                                           rowids    : rids}};
            this._ajax('/app/delete_row', deleteJson, 'delete_row', 'deleteData');

            this._applyStyle();

            this._grid.updateRowCount();
            this._grid.invalidate();
        } else if(e.keyCode === keyCode.ENTER || e.keyCode === 113) {
            e.stopImmediatePropagation();
            this._grid.editActiveCell();
            if(this._grid.getCellEditor()) {
                this._grid.getCellEditor().moveCaretToEnd();
            }
        } else if((e.keyCode >= 112 && e.keyCode <= 123) ||
                  $.inArray(e.keyCode, [keyCode.LEFT, keyCode.RIGHT, keyCode.UP,
                                        keyCode.DOWN, keyCode.PAGE_UP, keyCode.PAGE_DOWN,
                                        keyCode.CAPS_LOCK, keyCode.HOME, keyCode.END,
                                        keyCode.INSERT, keyCode.TAB, keyCode.ENTER,
                                        16, 17, 18, 225]) !== -1) {
            // Checks for keys we handle different.
            // 112-123 -> F1 - F12
            // 16, 17, 18 -> shift, ctrl, alt
            // 225 -> altgr on firefox ?
            return;
        } else {
            var activeCellNode = this._grid.getActiveCellNode();
            var isInEditMode = $(activeCellNode).hasClass("editable");

            activeCell = this._grid.getActiveCell()
            if (activeCell && !isInEditMode) {
                col = activeCell.cell;

                // would be nice to have xor in javascript to avoid confusing ternary operator.
                if (this._grid.getColumns()[col].editor && !(e.ctrlKey? !e.altKey : e.altKey)) {
                    this._grid.editActiveCell();
                }
            }
        }
    },

    _handleClick: function(e, args) {
        var self = this;
        self._dlg.dialog("moveToTop");
    },

    _gridColumnsReorder: function() {
        var self = this;
        var columns = self._grid.getColumns();
        var columnsPos = new Array();
        //id is the first column, it is not part of the sql
        for(var i = 1; i < columns.length; ++i) {
            columnsPos.push(self._origcolumns[columns[i].field]);
        }
        console.log(columnsPos);
        var reorderData = {reorder: {statement   : self._stmt,
                                     column_order: columnsPos}};
        self._ajax('app/reorder', reorderData, 'reorder', 'reorderResult');
    },

    _applyStyle: function() {
        var self = this;
        self._grid.removeCellCssStyles('delete');
        self._grid.removeCellCssStyles('update');
        var delStyle = new Object();
        var updStyle = new Object();
        var cols = self._grid.getColumns();
        for (var j=0;j<self._gdata.length; ++j)
            switch (self._gdata[j].op) {
                case 'del':
                    delStyle[j] = new Object();
                    for (var _i=0; _i<cols.length; ++_i)
                        delStyle[j][cols[_i].id] = 'slick-cell-del';
                    break;
                case 'upd':
                case 'ins':
                    updStyle[j] = new Object();
                    for (var _i=0; _i<cols.length; ++_i)
                        updStyle[j][cols[_i].id] = 'slick-cell-upd';
                    break;
            }
        if(!$.isEmptyObject(delStyle)) self._grid.setCellCssStyles('delete', delStyle);
        if(!$.isEmptyObject(updStyle)) self._grid.setCellCssStyles('update', updStyle);
    },

    close_stmt: function() {
        if(this._stmt && session && connection) {
            this.buttonPress("close");
        }
    },

    // loading the view table
    loadViews: function() { this._ajax('/app/views', null, 'views', 'loadViews'); },

    // loading rows
    buttonPress: function(button) {
        this._ajax('/app/button', {button: { connection: this._conn
                                               , statement: this._stmt
                                               , btn: button}}, 'button', 'loadRows');
    },

    disableDialog: function() {
        var self = this;
        if(!self._divDisable) {
            self._divDisable = $('<div>').addClass('ui-dialog-disabled');
        }
        self._divDisable.appendTo(self._dlg.dialog('widget'));
        self._divDisable.css('z-index', self._dlg.dialog('widget').css('z-index'));
    },

    enableDialog: function() {
        var self = this;
        if(!self._divDisable) {
            return;
        }
        self._divDisable.remove();
        self._grid.focus();
    },

    updateErlangCell: function(newErlangString) {
        var self = this;
        if(!self._erlangCellPos) {
            return;
        }
        var rowPos = self._erlangCellPos.row;
        var cell = self._erlangCellPos.cell;
        var columnField = self._grid.getColumns()[cell].field;
        self._gdata[rowPos][columnField] = newErlangString;
        self._gridCellChange(null, {
            grid : self._grid,
            row  : rowPos,
            cell : cell
        });
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

    _setTitleHtml: function(newTitle) {
        var self = this;
        self._dlg.dialog('option', 'title', newTitle[0].outerHTML);
        self._dlg.dialog("widget").find(".table-title").click(function(e) {
            self._dlgTtlCnxtMnu.dom
                .css("top", e.clientY - 10)
                .css("left", e.clientX)
                .data('cnxt', self)
                .show();
        });
    },

    // Use the destroy method to clean up any modifications your widget has made to the DOM
    _destroy: function() {
        console.log('destroying...');
        this._footerDiv.remove();
        this._tableDiv.remove();
        this._grid.destroy();
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
        var dlg = self._dlg.dialog('widget');

        // Column Data
        var columns = _cols;
        var fldWidth = 0;
        self._origcolumns = {};
        for (var i = 1; i < columns.length; ++i) {
            columns[i].formatter = Slick.Formatters.BinStringText;
            fldWidth = self._txtlen.text(_cols[i].name).width()+25;
            if(columns[i].hasOwnProperty('editor')) {
                columns[i].editor = Slick.Editors.ControlChars;
            }
            columns[i].minWidth = fldWidth;
            columns[i].width    = fldWidth;
            self._origcolumns[columns[i].field] = i;
        }

        //If we load new columns we can't keep the hidden columns information...
        if(self.hasOwnProperty('_hiddenColumns')) {
            delete self._hiddenColumns;
        }

        // load the column layout if its was saved
        if(self._clmlay !== null) {
            for(var i = 1; i < columns.length; ++i) {
                for(var j = 0; j < self._clmlay.length; ++j) {
                    if(columns[i].field === self._clmlay[j].name) {
                        columns[i].width = self._clmlay[j].width;
                        break;
                    }
                }
            }
        }
        self._grid.setColumns(columns);

        if(self._tbllay === null && !self._dlgResized) {
            dlg.width(Math.min(Math.max(self._footerWidth, self._getGridWidth() + 13), $(window).width()-dlg.offset().left-20));
        }
        self._dlg.dialog('open');
    },

    // public function for loading rows
    // used by ajaxCall but can also be used directly
    appendRows: function(_rows)
    {
        //console.time('appendRows');
        //console.profile('appendRows');

        var self = this;
        var redraw = false;
        var c = self._grid.getColumns();
        var firstChunk = (self._gdata.length === 0);

        // received response clear wait wheel
        this._setTitleHtml($(this._dlg.dialog('option', 'title')).removeClass('table-title-wait'));

        // system actions (beep and others)
        if(_rows.beep) beep();
        self._tbTxtBox.attr('title',_rows.toolTip);
        self._tbTxtBox.val(_rows.cnt+' ');
        var tbClass = (/tb_[^ ]+/g).exec(self._tbTxtBox.attr('class'));
        for (var i = 0; i < tbClass.length; ++i)
            self._tbTxtBox.removeClass(tbClass[i]);
        self._tbTxtBox.addClass('tb_'+_rows.state);
        if(_rows.message.length > 0) alert_jq(_rows.message);
        if(_rows.op !== "nop") {
            if(!$.isEmptyObject(_rows.disable) || !$.isEmptyObject(_rows.promote)) {
                for(var btn in self._toolbarButtons) {
                    var tbBtnObj = self._toolbarButtons[btn];
                    var btnElm = self[tbBtnObj.dom];
                    if (!$.isEmptyObject(_rows.disable) && _rows.disable.hasOwnProperty(btn)) {
                        btnElm
                            .button('disable')
                            .attr('title', _rows.disable[btn]);
                    }
                    else if (!$.isEmptyObject(_rows.promote) && _rows.promote.hasOwnProperty(btn)) {
                        btnElm
                            .button('enable')
                            .addClass('ui-state-error')
                            .attr('title', _rows.promote[btn]);
                    }
                    else { // enable the button
                        btnElm
                            .button('enable')
                            .removeClass('ui-state-error')
                            .attr('title', tbBtnObj.tip);
                    }
                }
            } else {
                for(var btn in self._toolbarButtons) {
                    self[self._toolbarButtons[btn].dom]
                    .button('enable')
                    .removeClass('ui-state-error')
                    .attr('title', self._toolbarButtons[btn].tip);
                }
            }
        }

        if (firstChunk && _rows.hasOwnProperty('max_width_vec') && !$.isEmptyObject(_rows.max_width_vec) && self._clmlay === null) {
            var fieldWidth = 0;
            for(var i=0;i<c.length; ++i) {
                fieldWidth = self._txtlen.text(_rows.max_width_vec[c[i].field]).width();
                fieldWidth = fieldWidth + 0.4 * fieldWidth;
                if(c[i].width < fieldWidth) {
                    c[i].width = fieldWidth;
                    if (c[i].width > self._MAX_ROW_WIDTH)
                        c[i].width = self._MAX_ROW_WIDTH;
                }
            }
            self._grid.setColumns(c);
            redraw = true;
        }

       // focus change on gresp receive
        var gvp = self._grid.getViewport();
        var gvpH = gvp.bottom - gvp.top;
        var computedFocus = _rows.focus;
        var needScroll = false;
        switch (_rows.op) {
            case "rpl": // replace
                self._grid.setData(_rows.rows);
                self._gdata = self._grid.getData();
                computedFocus = gvp.bottom + _rows.rows.length - 2 * gvpH;
                if(computedFocus < 0) computedFocus = 0;
                if(computedFocus > self._gdata.length - 1) computedFocus = self._gdata.length - 1;
                redraw = true;
                needScroll = true;
                break;
            case "app": // append
                for(var i=0; i < _rows.rows.length; ++i) {
                    self._gdata.push(_rows.rows[i])
                }
                self._gdata.splice(0, self._gdata.length - _rows.keep);
                computedFocus = gvp.top + _rows.rows.length;
                if(computedFocus > self._gdata.length - 1) computedFocus = self._gdata.length - 1;
                redraw = true;
                needScroll = true;
                break;
            case "prp": // prepend
                do {
                    self._gdata.splice(0, 0, _rows.rows.pop());
                } while (_rows.rows.length > 0)
                self._gdata.splice(_rows.keep, self._gdata.length - _rows.keep);
                computedFocus = gvp.bottom + _rows.rows.length - 2 * gvpH;
                if(computedFocus < 0) computedFocus = 0;
                if(computedFocus > self._gdata.length - 1) computedFocus = self._gdata.length - 1;
                redraw = true;
                needScroll = true;
                break;
            case "clr": // delete all rows
                self._gdata.splice(0, self._gdata.length);
                redraw = true;
                break;
            case "ins": // add rows to the end, keep all
                console.log('ins');
                for(var i=0; i < _rows.rows.length; ++i)
                    self._gdata.push(_rows.rows[i]);
                redraw = true;
                break;
            case "nop": // no operation
                console.log('nop');
                break;
            default:
                console.log("unknown operation "+_rows.op);
                break;
        }

        if(_rows.focus < 0) {
            computedFocus = self._gdata.length + _rows.focus;
            if(computedFocus < 0) computedFocus = 0;
        } else if(_rows.focus > 0) {
            computedFocus = _rows.focus - 1;
        }
        
        if(!redraw && needScroll) {
            self._grid.scrollRowIntoView(computedFocus);
        }

        if (redraw) {
            self._grid.updateRowCount();
            if(computedFocus > self._gdata.length || computedFocus < 0)
                self._grid.scrollRowIntoView(self._gdata.length-1);
            else if(needScroll)
                self._grid.scrollRowIntoView(computedFocus);

            self._grid.resizeCanvas();

            // only if the dialog don't have a predefined height/width
            if(self._tbllay === null) {
                // since columns' width doesn't change after the first block we can skip this
                if (firstChunk) {
                    var dlg = this._dlg.dialog('widget');

                    if (!self._dlgResized) {
                        var gWidth = self._getGridWidth() + 13;
                        var rWindowWidth = $(window).width()-dlg.offset().left-20; // available width for the window
                        
                        // Dialog width adjustment
                        if (self._footerWidth > gWidth) {
                            // table is smaller than the footer
                            dlg.width(self._footerWidth);
                        } else if (gWidth < rWindowWidth) {
                            // table is smaller than the remaining window
                            dlg.width(gWidth);
                        } else {
                            // table is bigger then the remaining window
                            var orig_top = dlg.offset().top;
                            var new_left = dlg.offset().left - gWidth + rWindowWidth;
                            if(new_left > 0) {
                                self._dlg.dialog("option", "position", [new_left, orig_top]);
                                dlg.width(gWidth);
                            } else {
                                self._dlg.dialog("option", "position", [0, orig_top]);
                                dlg.width($(window).width() - 20);
                            }
                        }

                        var oldDlgHeight = dlg.height();
                        var gHeight = self._getGridHeight() + 10;
                        var rWindowHeight = $(window).height()-dlg.offset().top-2*self.options.toolBarHeight-20; // available height for the window
                        if (dlg.height() > gHeight)       // if dialog is already bigger than height required by the table
                            self._dlg.height(gHeight);
                        else if (gHeight < rWindowHeight) // if table height is less then remaining window height
                            self._dlg.height(gHeight);
                        else                              // if table height is still bigger than the remaining window height
                            self._dlg.height(rWindowHeight);
                        if (oldDlgHeight != dlg.height())
                            self._grid.resizeCanvas();
                    }
                    // adjusting the column to fill the rest of the window
                    if((self._getGridWidth() + 18) < dlg.width()) {
                        c[c.length - 1].width += (dlg.width()-self._getGridWidth()-18);
                        self._grid.setColumns(c);
                    }
                }
            }
            // If we are tailing we need to keep the editor
            // TODO: Improve this design to avoid errors in edge cases.
            if(self._grid.getCellEditor() && _rows.loop == "tail") {
                self._grid.invalidate();
                if(self._grid.getActiveCell().row < self._gdata.length) {
                    self._grid.editActiveCell();
                }
            } else {
                self._grid.invalidate();
            }

            // 
            // loading of rows is the costliest of the operations
            // compared to computing and adjusting the table width/height
            // (so for now total time of function entry/exit is appromately equal to only row loading)
            //

            // update row styles
            self._applyStyle();
        }

        //console.timeEnd('appendRows');
        //console.profileEnd('appendRows');
    }

  });
}( jQuery ) );
