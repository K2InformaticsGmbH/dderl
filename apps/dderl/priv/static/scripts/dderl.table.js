(function( $ ) {

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
    _conn           : null,
    _session        : null,
    _adapter        : null,
    _cmd            : null,
    _clmlay         : null,
    _tbllay         : null,

    _fetchIsTail    : false,
    _fetchIsPush    : false,

    _dlgResized     : false,

    // sort and filter
    _sorts          : null,
    _filters        : null,
    _rftchExpBkOff  : 2,

    // start button
    _startBtn       : null,
    _cmdStrs        : [],

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
                        sortResult      : function(e, _result) { e.data._renderRows(_result); }
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
    _slkHdrCnxtMnu  : {'Browse Data'    : '_browseHeaderData',
                       'Hide'           : '_hide',
                       'UnHide'         : '_unhide',
                       'Filter...'      : '_filterColumn',
                       'Filter Clear'   : '_filterClear',
                       'Sort...'        : '_sort',
                       'Sort ASC'       : '_sortAsc',
                       'Sort DESC'      : '_sortDesc',
                       'Sort Clear'     : '_sortClear'},
    _slkCellCnxtMnu : {'Browse Data'    : '_browseCellData',
                       'Filter'         : '_filter'},

    // These options will be used as defaults
    options: {
        // dialog options default override
        toolBarHeight     : 22,
        height            : 500,
        width             : 500,
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
                              rowHeight: 20
                            },

        // dderl options
        dderlConn         : null,
        dderlAdapter      : null,
        dderlSession      : null,
        dderlStatement    : null,
        dderlCmd          : null,
        dderlClmlay       : null,
        dderlTbllay       : null,
        dderlStartBtn     : '>',
        dderlSortSpec     : null,
    },
 
    // Set up the widget
    _create: function() {
        var self = this;

        self._fnt = $(document.body).css('font-family');
        self._fntSz = $(document.body).css('font-size');

        // preserve some options
        if(self.options.dderlConn       !== self._conn)     self._conn      = self.options.dderlConn;
        if(self.options.dderlAdapter    !== self._adapter)  self._adapter   = self.options.dderlAdapter;
        if(self.options.dderlSession    !== self._session)  self._session   = self.options.dderlSession;
        if(self.options.dderlStatement  !== self._stmt)     self._stmt      = self.options.dderlStatement;
        if(self.options.dderlCmd        !== self._cmd)      self._cmd       = self.options.dderlCmd;
        if(self.options.dderlClmlay     !== self._clmlay)   self._clmlay    = self.options.dderlClmlay;
        if(self.options.dderlTbllay     !== self._tbllay)   self._tbllay    = self.options.dderlTbllay;
        if(self.options.dderlStartBtn   !== self._startBtn) self._startBtn  = self.options.dderlStartBtn;
        if(self.options.dderlSortSpec   !== self._sorts)    self._sorts     = self.options.dderlSortSpec;

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
        var hasCmd = false;
        for (var i = 0; i < this._cmdStrs.length; ++i)
            if (this._cmd === this._cmdStrs[i]) hasCmd = true;

        if(!hasCmd) this._cmdStrs.splice(0,0,this._cmd);

        $('<div>')
        .appendTo(document.body)
        .sql({autoOpen  : false,
              title     : this.options.title,
              cmdOwner  : this,
              history   : this._cmdStrs
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
        var self = this;
        var colnamesizes = new Array();
        var cols = this._grid.getColumns();

        // Column names and width
        for(var idx = 0; idx < cols.length; ++idx) {
            if(cols[idx].name.length > 0) {
                colnamesizes.push({
                        name: cols[idx].name,
                        width: cols[idx].width,
                        hidden: false
                });
            }
        }

        // Add the hidden columns to the column layout.
        if(self.hasOwnProperty('_hiddenColumns')) {
            for(var idx = self._hiddenColumns.length - 1; idx >= 0; --idx) {
                // The -1 in the insertPos is needed because the id column
                // is not saved, but was counted when calculating the index.
                var insertPos = self._hiddenColumns[idx].idxCol - 1;
                var hiddenCol = self._hiddenColumns[idx].colContent;
                colnamesizes.splice(insertPos, 0, {
                    name: hiddenCol.name,
                    width: hiddenCol.width,
                    hidden: true
                });
            }
        }

        // Table width/height/position
        var w = this._dlg.dialog('widget').width();
        var h = this._dlg.dialog('widget').height();
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
        this._ajax('/app/save_view', saveView, 'save_view', 'saveViewResult');
    },

    cmdReload: function(cmd, button) {
        /*if(this._cmd === cmd)
            console.log('command unchanged ['+cmd+']');
        else {*/
            console.log('command reloading ['+cmd+']');
            this._cmd = cmd;
            this.options.dderlStartBtn = this._startBtn = button;
            this._ajax('/app/query', {query: {connection: this._conn, qstr : this._cmd}}, 'query', 'queryResult');
        //}
    },

    // columns hide/unhide
    _hide: function(_ranges) {
        var self = this;
        var columns = self._grid.getColumns();
        var toHide = {};
        for (var i=0; i<_ranges.length; ++i) {
            toHide[_ranges[i].fromCell] = true;
            toHide[_ranges[i].toCell] = true;
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
    },

    // sorts
    _sort: function(_ranges) {
        var self = this;
        if(self._sorts === null)
            self._sorts = new Object();
        var cols = self._grid.getColumns();
        for (var i=0; i<_ranges.length; ++i)
            for(var c=_ranges[i].fromCell; c <= _ranges[i].toCell; ++c)
                if(cols[c].name.length > 0 && !self._sorts.hasOwnProperty(cols[c].name)) {
                    self._sorts[cols[c].name] =
                        { id : c,
                         asc : true
                        };
                }
        self._showSortGui();
    },
    _sortAsc: function(_ranges) {
        var self = this;
        if(self._sorts === null)
            self._sorts = new Object();
        var cols = this._grid.getColumns();
        for (var i=0; i<_ranges.length; ++i)
            for(var c=_ranges[i].fromCell; c <= _ranges[i].toCell; ++c)
                if(cols[c].name.length > 0)
                    self._sorts[cols[c].name] = {id : c, asc : true};
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
        for (var i=0; i<_ranges.length; ++i)
            for(var c=_ranges[i].fromCell; c <= _ranges[i].toCell; ++c)
                if(cols[c].name.length > 0)
                    self._sorts[cols[c].name] = {id : c, asc : false};
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
            for (var i=0; i<_ranges.length; ++i)
                for(var c=_ranges[i].fromCell; c <= _ranges[i].toCell; ++c)
                    if(cols[c].name.length > 0 && self._sorts.hasOwnProperty(cols[c].name))
                        delete self._sorts[cols[c].name];
        }
        if(self._sorts && !$.isEmptyObject(self._sorts))
            self._showSortGui();
        else {
            self._sorts = null;
            self._ajax('/app/sort', {sort: {spec: [], statement: self._stmt}}, 'sort', 'sortResult');
        }
    },
    _showSortGui: function() {
        var self = this;
        var data = new Array();
        for (var s in self._sorts)
            data.push({name: s, sort: (self._sorts[s].asc ? 'ASC' : 'DESC'), id: self._sorts[s].id});

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
            for (var i = 0; i <sd.length; ++i) {
                self._sorts[sd[i].name] = { id : sd[i].id,
                                           asc : (sd[i].sort === 'ASC' ? true : false) };
            }
            return self._sortSpec2Json();
        }
        
        sortDlg
            .dialog({
                width : 380,
                modal : true,
                title : 'Sorts',
                position : { my: "left top", at: "left bottom", of: this._dlg },
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
                /*resize: function(e, ui) {
                    var dH = $(this).height() / fCount - 30;
                    var dW = $(this).width() - 30;
                    for(var c in self._filters) {
                        self._filters[c].inp.width(dW);
                        self._filters[c].inp.height(dH);
                    }
                }*/
            });

    },

    _ajax: function(url, data, resp, callback) {
        this._dlg.dialog('option', 'title').addClass('table-title-wait');
        ajaxCall(this, url, data, resp, callback);
    },
    
    _sortSpec2Json: function() {
        var self = this;
        var sortspec = new Array();
        for (var s in self._sorts) {
            var t = new Object();
            t[self._sorts[s].id] = self._sorts[s].asc;
            sortspec.push(t);
        }
        return sortspec;
    },
    
    // filters
    _filterColumn: function(_ranges) {
        var self = this;
        var cols = self._grid.getColumns();
        if(self._filters === null)
            self._filters = new Object();
        for (var i=0; i<_ranges.length; ++i) {
            for(var c=_ranges[i].fromCell; c <= _ranges[i].toCell; ++c) {
                if(cols[c].name.length > 0) {
                    if(!self._filters.hasOwnProperty(cols[c].name)) {
                        self._filters[cols[c].name] = {inp : $('<textarea>')
                                                       .attr('type', "text")
                                                       .css('margin', 0)
                                                       .css('white-space','nowrap')
                                                       .css('overflow','auto')
                                                       .css('padding', 0),
                                                 vals: new Object(),
                                                 id: c};
                    }
                }
            }
        }
        self._showFilterGui();
    },
    _filterClear: function(_ranges) {
        var self = this;
        if(self._filters) {
            var cols = self._grid.getColumns();
            for (var i=0; i<_ranges.length; ++i)
                for(var c=_ranges[i].fromCell; c <= _ranges[i].toCell; ++c)
                    if(self._filters.hasOwnProperty(cols[c].name))
                        delete self._filters[cols[c].name];
        }
        if(self._filters && !$.isEmptyObject(self._filters))
            self._showFilterGui();
        else {
            self._filters = null;
            self._ajax('/app/filter', {filter: {spec: {'undefined':[]}, statement: self._stmt}}, 'filter', 'filterResult');
        }
    },
    _filter: function(_ranges) {
        var self = this;
        if(self._filters === null)
            self._filters = new Object();
        var cols = this._grid.getColumns();
        for (var i=0; i<_ranges.length; ++i) {
            for(var c=_ranges[i].fromCell; c <= _ranges[i].toCell; ++c) {
                if(cols[c].name.length > 0) {
                    if(!self._filters.hasOwnProperty(cols[c].name)) {
                        self._filters[cols[c].name] = {inp : $('<textarea>')
                                                       .attr('type', "text")
                                                       .css('margin', 0)
                                                       .css('white-space','nowrap')
                                                       .css('overflow','auto')
                                                       .css('padding', 0),
                                                 vals: new Object(),
                                                 id: c};
                    }
                    for(var r=_ranges[i].fromRow; r <= _ranges[i].toRow; ++r) {
                        self._filters[cols[c].name].vals[this._gdata[r][cols[c].name].replace(/\n/g,'\\n')] = true;
                    }
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
        }
        else
            self._showFilterGui();
    },
    _showFilterGui: function() {
        var self = this;
        // number of current filters
        var fCount = 0;
        for (var c in self._filters)
            fCount++;

        var fltrDlg =
            $('<div>')
            .css('width', 400)
            .appendTo(document.body);

        var fltrTbl =
            $('<table>')
            .css('height', '100%')
            .css('width', '100%')
            .attr('border', 0)
            .attr('cellpadding', 0)
            .attr('cellspacing', 0)   
            .appendTo(fltrDlg);
        for(var c in self._filters) {
            var strs = [];
            for(s in self._filters[c].vals) strs.push(s);
            self._filters[c].inp.val(strs.join('\n'));
            $('<tr>')
                .append($('<td>'))
                .append('<td>'+c+'</td>')
                .appendTo(fltrTbl);
            $('<tr>')
                .append('<td>in&nbsp;</td>')
                .append($('<td>').append(self._filters[c].inp))
                .appendTo(fltrTbl);
        }

        fltrDlg
            .dialog({
                width: fltrTbl.width()+60,
                modal: true,
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
        fltrDlg.dialog('option', 'buttons', buttons);

        var dH = fltrDlg.height() / fCount - 30;
        var dW = fltrDlg.width() - 30;
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
            fltr[self._filters[c].id] = vStrings;
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
            var data    = self._gdata[cell.fromRow];
            // console.log('browse_data @ '+column.name+' val '+JSON.stringify(data));
            self._ajax('/app/browse_data',
                           { browse_data: {connection : self._conn,
                                            statement : self._stmt,
                                                  row : data.id, //cell.fromRow,
                                                  col : cell.fromCell}},
                           'browse_data', 'browseData');
        }
    },

    _createSlickGrid: function() {
        var self = this;

        // building slickgrid
        // a dummy column needed to be added to enable slickgrid to enable column re-order
        self._grid = new Slick.Grid(self._tableDiv, [], [{id: "_"}], self.options.slickopts);
        self._grid.setSelectionModel(new Slick.CellRowColSelectionModel());
        self._grid.registerPlugin(new Slick.CellExternalCopyManager());

        self._grid.onContextMenu.subscribe($.proxy(self._gridContextMenu, self));
        self._grid.onHeaderContextMenu.subscribe($.proxy(self._gridHeaderContextMenu, self));
        self._grid.onCellChange.subscribe($.proxy(self._gridCellChange, self));
        self._grid.onAddNewRow.subscribe($.proxy(self._gridAddNewRow, self));
        self._grid.onKeyDown.subscribe($.proxy(self._delRow, self));
        self._grid.onScroll.subscribe(function(e, args) {
            var vp = args.grid.getViewport();
            //console.log('viewed '+JSON.stringify(vp));
        });

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
                if($.isFunction(f))
                    f(self);
                else
                    throw('['+self.options.title+'] toolbar '+_btn+' has unimplimented cb '+fName);
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
        self.buttonPress(">|...");
    },
    _toolBarSkipTl: function(self) {
        console.log('['+self.options.title+'] cb _toolBarSkipTl');
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
        if(_saveView === "ok")
            console.log('[AJAX] view saved!'),
            this._dlg.dialog('option', 'title').removeClass('table-title-wait');
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
        this._dlg.dialog('option', 'title', $('<span>')
                                                .text(_views.name)
                                                .addClass('table-title'));
        this.options.title = _views.name;
        console.log('>>>>> table '+_views.name+' '+_views.connection);
        if(_views.hasOwnProperty('error'))
            alert_jq(_views.error);
        else {
            if(_views.hasOwnProperty('sort_spec') && !$.isEmptyObject(_views.sort_spec)) {
                this._sorts = _views.sort_spec;
            }
            this.setColumns(_views.columns);
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
        if(_table.hasOwnProperty('sort_spec') && !$.isEmptyObject(_table.sort_spec)) {
            this._sorts = _table.sort_spec;
        }
        if(_table.hasOwnProperty('columns')) {
            this.setColumns(_table.columns);
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
        }

        var cl = null;
        if(_table.hasOwnProperty('column_layout') && _table.column_layout.length > 0)
            cl = _table.column_layout;
        var tl = null;
        if(_table.hasOwnProperty('table_layout') && _table.table_layout.length > 0)
            tl = _table.table_layout.length;

        this._dlg.dialog('option', 'title').removeClass('table-title-wait');
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

            // command back request
            if(_rows.loop.length > 0) {
                if (rowsCount > 0) {
                    console.log(rowsCount+' rows received, retrying '+_rows.loop);
                    this.buttonPress(_rows.loop);
                    this._rftchExpBkOff = 2;
                } else {
                    this._rftchExpBkOff = (this._rftchExpBkOff * 2);
                    console.log('no rows received, retrying '+_rows.loop+' after '+this._rftchExpBkOff+' ms');
                    setTimeout(function(){self.buttonPress(_rows.loop);}, this._rftchExpBkOff);
                }
            }
            else this._rftchExpBkOff = 2; // end of command looping received
        }
        else if(_rows.hasOwnProperty('error')) {
            alert_jq(_rows.error);
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
            // .on('dialogfocus', function (event, ui) {
            //     $(".ui-dialog").find(".ui-dialog-titlebar").removeClass("ui-state-error");
            //     $(this).parents(".ui-dialog:first").find(".ui-dialog-titlebar").addClass("ui-state-error");
            // }
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

        self._dlg.dialog("widget").draggable("option","containment","#main-body");
        if(self.options.position.length === undefined)
            self._dlg.dialog( "option", "position", {at : 'left top+'+$("#main-body").css('top'), my : 'left top', collision : 'flipfit'} );

        // converting the title text to a link
        self._dlg.dialog('option', 'title', $('<span>')
                                                .text(self.options.title)
                                                .addClass('table-title'));
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

        // right click on non-column zone
        if(args.column === undefined) return;

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
        var updateJson  = {update_data: {connection  : this._conn,
                                         statement   : this._stmt,
                                         rowid       : parseInt(modifiedRow.id),
                                         cellid      : args.cell,
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
                                        col         : args.grid.getColumnIndex(args.column.id),
                                        value       : args.item[args.column.id]}};
        //console.log('inserting '+JSON.stringify(args.item));
        this._ajax('/app/insert_data', insertJson, 'insert_data', 'insertData');
    },
    _delRow: function(e, args) {
        e.stopPropagation();
        if(e.keyCode == 46) {
            // Delete all rows from the selected range
            var selRanges = this._grid.getSelectionModel().getSelectedRanges();
            var rids = [];
            for(var i=0; i < selRanges.length; ++i)
                for(var ri = selRanges[i].fromRow; ri <= selRanges[i].toRow; ++ri) {
                    if(this._gdata[ri].op !== 'ins')
                        this._gdata[ri].op = 'del';
                    else
                        this._gdata.splice(ri, 1);
                    rids.push(this._gdata[ri].id);
                }

            // Delete args.row
            var deleteJson = {delete_row: {statement : this._stmt,
                                           rowids    : rids}};
            this._ajax('/app/delete_row', deleteJson, 'delete_row', 'deleteData');

            this._applyStyle();

            this._grid.updateRowCount();
            this._grid.invalidate();
        }
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
                    updStyle[j] = new Object();
                    for (var _i=0; _i<cols.length; ++_i)
                        updStyle[j][cols[_i].id] = 'slick-cell-upd';
                    break;
            }
        if(!$.isEmptyObject(delStyle)) self._grid.setCellCssStyles('delete', delStyle);
        if(!$.isEmptyObject(updStyle)) self._grid.setCellCssStyles('update', updStyle);
    },
    
    // loading the view table
    loadViews: function() { this._ajax('/app/views', null, 'views', 'loadViews'); },

    // loading rows
    buttonPress: function(button) {
        this._ajax('/app/button', {button: { connection: this._conn
                                               , statement: this._stmt
                                               , btn: button}}, 'button', 'loadRows');
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
        if(this._stmt) this.buttonPress("close");
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
        for (var i = 1; i < columns.length; ++i) {
            fldWidth = self._txtlen.text(_cols[i].name).width()+25;
            if(columns[i].hasOwnProperty('editor')) {
                columns[i].editor   = Slick.Editors.Text;
            }
            columns[i].minWidth = fldWidth;
            columns[i].width    = fldWidth;
        }

        // load the column layout if its was saved
        if(self._clmlay !== null) {
            var tmpColumns = new Array();
            self['_hiddenColumns'] = new Array();
            //Add the id column since it should be always the first one.
            tmpColumns[0] = columns[0];
            // The offset starts in 1 due to the id as first column.
            var offset = 1;
            for(var i = 0; i < self._clmlay.length; ++i) {
                for(var j = 1; j < columns.length; ++j) {
                    if(columns[j].name === self._clmlay[i].name) {
                        columns[j].width = self._clmlay[i].width;
                        if(self._clmlay[i].hidden) {
                            self._hiddenColumns.push(
                                {
                                    idxCol: i+offset,
                                    colContent: columns[j]
                                }
                            );
                            --offset;
                        } else {
                            tmpColumns[i+offset] = columns[j];
                        }
                        break;
                    }
                }
            }
            columns = tmpColumns;
            if(self._hiddenColumns.length == 0) {
                delete self._hiddenColumns;
            }
        }
        self._grid.setColumns(columns);

        if(self._tbllay === null && !self._dlgResized) {
            dlg.width(Math.min(Math.max(self._footerWidth, self._getGridWidth()), $(window).width()-dlg.offset().left-10));
        }
        self._dlg.dialog('open');
    },

    // public function for loading rows
    // used by ajaxCall but can also be used directly
    appendRows: function(_rows)
    {
        //console.time('appendRows');
        console.profile('appendRows');

        var self = this;
        var redraw = false;
        var c = self._grid.getColumns();
        var firstChunk = (self._gdata.length === 0);

        // received response clear wait wheel
        self._dlg.dialog('option', 'title').removeClass('table-title-wait');

        // system actions (beep and others)
        if(_rows.beep) beep();
        self._tbTxtBox.attr('title',_rows.toolTip);
        self._tbTxtBox.val(_rows.cnt+' ');
        var tbClass = (/tb_[^ ]+/g).exec(self._tbTxtBox.attr('class'));
        for (var i = 0; i < tbClass.length; ++i)
            self._tbTxtBox.removeClass(tbClass[i]);
        self._tbTxtBox.addClass('tb_'+_rows.state);
        if(_rows.message.length > 0) alert_jq(_rows.message);
        if(!$.isEmptyObject(_rows.disable) || !$.isEmptyObject(_rows.promote))
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
        else {
            for(var btn in self._toolbarButtons)
                self[self._toolbarButtons[btn].dom]
                    .button('enable')
                    .removeClass('ui-state-error')
                    .attr('title', self._toolbarButtons[btn].tip);
        }

        // if new cmd is different from the last one append
        if(_rows.sql.length > 0 && (self._cmdStrs.length === 0 || self._cmdStrs[self._cmdStrs.length-1] !== _rows.sql))
            self._cmdStrs.push(_rows.sql);

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


        switch (_rows.op) {
            case "rpl": // replace
                self._grid.setData(_rows.rows);
                self._gdata = self._grid.getData();
                redraw = true;
                break;
            case "app": // append
                for(var i=0; i < _rows.rows.length; ++i)
                    self._gdata.push(_rows.rows[i])
                self._gdata.splice(0, self._gdata.length - _rows.keep);
                redraw = true;
                break;
            case "prp": // prepend
                do {
                    self._gdata.splice(0, 0, _rows.rows.pop());
                } while (_rows.rows.length > 0)
                self._gdata.splice(_rows.keep, self._gdata.length - _rows.keep);
                redraw = true;
                break;
            case "clr": // delete all rows
                self._gdata.splice(0, self._gdata.length);
                redraw = true;
                break;
            case "ins": // no operation
                console.log('ins');
                for(var i=0; i<_rows.rows.length; ++i)
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

        // focus change on gresp receive
        var gvp = self._grid.getViewport();
        var gvpH = gvp.bottom - gvp.top;
        if(_rows.focus < 0) {
            _rows.focus = self._gdata.length + _rows.focus;
            self._grid.scrollRowIntoView(_rows.focus);
        } else if(_rows.focus > 0) {
            _rows.focus -= 1;
            self._grid.scrollRowIntoView(_rows.focus);
        } else {
            switch(_rows.op) {
                case 'rpl' :
                    _rows.focus = gvp.bottom + _rows.rows.length - 2 * gvpH;
                    if(_rows.focus < 0) _rows.focus = 0;
                    if(_rows.focus > self._gdata.length - 1) _rows.focus = self._gdata.length - 1;
                    break;
                case 'app' :
                    _rows.focus = gvp.top + _rows.rows.length;
                    if(_rows.focus > self._gdata.length - 1) _rows.focus = self._gdata.length - 1;
                    break;
                case 'prp' :
                    _rows.focus = gvp.bottom + _rows.rows.length - 2 * gvpH;
                    if(_rows.focus < 0) _rows.focus = 0;
                    if(_rows.focus > self._gdata.length - 1) _rows.focus = self._gdata.length - 1;
                    break;
                case "nop": // no operation
                    console.log('nop');
                    break;
                default:
                    console.log("unknown operation "+_rows.op);
                    break;
            }
        }
        
        // scroll to row always if focus > 0
        if(!redraw && _rows.focus >= 0)
            self._grid.scrollRowIntoView(_rows.focus);

        if (redraw) {
            self._grid.updateRowCount();
            //self._grid.invalidateRow(self._gdata.length-1);
            if(_rows.focus > self._gdata.length || _rows.focus < 0)
                self._grid.scrollRowIntoView(self._gdata.length-1);
            else if(_rows.focus >= 0)
                self._grid.scrollRowIntoView(_rows.focus);

            self._grid.resizeCanvas();

            // only if the dialog don't have a predefined height/width
            if(self._tbllay === null && self._clmlay === null) {
                // since columns' width doesn't change after the first block we can skip this
                if (firstChunk) {
                    var dlg = this._dlg.dialog('widget');

                    if (!self._dlgResized) {

                        var gWidth = self._getGridWidth();
                        var rWindowWidth = $(window).width()-dlg.offset().left-10; // available width for the window
                        
                        // Dialog width adjustment
                        if (self._footerWidth > gWidth) // table is smaller than the footer
                            dlg.width(self._footerWidth);
                        else if (gWidth < rWindowWidth) // table is smaller than the remaining window
                            dlg.width(gWidth);
                        else                            // table is bigger then the remaining window
                            dlg.width(rWindowWidth);

                        var oldDlgHeight = dlg.height();
                        var gHeight = self._getGridHeight();
                        var rWindowHeight = $(window).height()-dlg.offset().top-2*self.options.toolBarHeight; // available height for the window
                        if (dlg.height() > gHeight)       // if dialog is already bigger than height required by the table
                            this._dlg.height(gHeight);
                        else if (gHeight < rWindowHeight) // if table height is less then remaining window height
                            this._dlg.height(gHeight);
                        else                              // if table height is still bigger than the remaining window height
                            this._dlg.height(rWindowHeight);
                        if (oldDlgHeight != dlg.height())
                            self._grid.resizeCanvas();
                    }
                    // adjusting the column to fill the rest of the window
                    if(self._getGridWidth() < dlg.width()) {
                        c[c.length - 1].width += (dlg.width()-self._getGridWidth()-10);
                        self._grid.setColumns(c);
                    }
                }
            }
            self._grid.invalidate();

            // 
            // loading of rows is the costliest of the operations
            // compared to computing and adjusting the table width/height
            // (so for now total time of function entry/exit is appromately equal to only row loading)
            //

            // update row styles
            self._applyStyle();
        }

        //console.timeEnd('appendRows');
        console.profileEnd('appendRows');
    }

  });
}( jQuery ) );
