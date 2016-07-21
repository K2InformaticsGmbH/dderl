import jQuery from 'jquery';
import * as d3 from 'd3/build/d3.node';
import 'd3-selection-multi';
import 'jquery-dialogextend/build/jquery.dialogextend';
import {addWindowFinder, dderlState, updateWindowTitle, saveDashboardWithCounter,
    alert_jq, prompt_jq, confirm_jq, smartDialogPosition, addToCurrentViews,
    ajaxCall, beep} from './dderl';
import './dderl.termEditor.js';

(function( $ ) {
  $.widget( "dderl.table", $.ui.dialog, {

    _dlg            : null,
    _tableDiv       : null,
    _graphDivs      : null,
    _graphSpec      : null,
    _footerDiv      : null,
    _footerWidth    : 0,
    _grid           : null,
    _gdata          : null,
    _gridDataView   : null,
    _txtlen         : null,
    _fnt            : null,
    _fntSz          : null,
    _MAX_ROW_WIDTH  : 1000,

    _url            : null,
    _stmt           : null,
    _conn           : null,
    _adapter        : null,
    _cmd            : null,
    _optBinds       : null,
    _clmlay         : null,
    _tbllay         : null,
    _viewId         : null,
    _origcolumns    : null,

    _fetchIsTail    : false,
    _fetchIsPush    : false,

    _dlgResized     : false,

    // sort and filter
    _sorts          : null,
    _sortDlg        : null,
    _filters        : null,
    _fltrDlg        : null,
    _restoreAsDlg   : null,

    // start button
    _startBtn       : null,
    _cmdStrs        : null,
    _divSqlEditor   : null,
    _planeToShow    : 0,
    _planeSpecs     : null,
    
    // for edit erlang terms
    _erlangCellPos  : null,
    _divDisable     : null,

    // pause continue tail on edition of cells
    _editedText     : null,
    _editorEscaped  : false,
    _loop           : "",
    _spinCounter    : 0,
    _imagePreview   : null,

    // flag to avoid multiple calls to reorder
    _reorderCalled  : false,

    // pointer to the link element with our entry in the windows list
    _windowFinderTextLink: null,

    // private event handlers
    _handlers       : { loadViews       : function(e, _result) { e.data._renderViews            (_result); },
                        openView        : function(e, _result) { e.data._openView               (_result); },
                        browseData      : function(e, _result) { e.data._renderNewTable         (_result); },
                        queryResult     : function(e, _result) { e.data._renderTable            (_result); },
                        updateData      : function(e, _result) { e.data._checkUpdateResult      (_result); },
                        insertData      : function(e, _result) { e.data._insertResult           (_result); },
                        deleteData      : function(e, _result) { e.data._deleteResult           (_result); },
                        commitResult    : function(e, _result) { e.data._checkCommitResult      (_result); },
                        stmtCloseResult : function(e, _result) { e.data._checkStmtCloseResult   (_result); },
                        saveViewResult  : function(e, _result) { e.data._checkSaveViewResult    (_result); },
                        newViewResult   : function(e, _result) { e.data._checkNewViewResult     (_result); },
                        opViewResult    : function(e, _result) { e.data._operateViewResult      (_result); },
                        loadRows        : function(e, _result) { e.data._renderRows             (_result); },
                        filterResult    : function(e, _result) { e.data._renderRows             (_result); },
                        sortResult      : function(e, _result) { e.data._renderRows             (_result); },
                        reorderResult   : function(e, _result) { e.data._renderRows             (_result); },
                        truncateResult  : function(e, _result) { e.data._reloadOnSuccess        (_result); },
                        dropResult      : function(e, _result) { e.data._reloadOnSuccess        (_result); },
                        snapshotResult  : function(e, _result) { e.data._noReloadOnSuccess      (_result); },
                        restoreResult   : function(e, _result) { e.data._reloadOnSuccess        (_result); },
                        restoreAsResult : function(e, _result) { e.data._reloadOnSuccess        (_result); },
                        editTermOrView  : function(e, _result) { e.data._openTermOrViewEditor   (_result); },
                        getSqlResult    : function(e, _result) { e.data._openSqlEditor          (_result); },
                        activateSender  : function(e, _result) { e.data._activateSenderResult   (_result); },
                        activateReceiver: function(e, _result) { e.data._activateReceiverResult (_result); },
                        cacheResult     : function(e, _result) { e.data._cacheResult            (_result); }
                      },

    _toolbarButtons : {'restart'  : {tip: 'Reload',                typ : 'btn', icn : 'refresh',               clk : '_toolBarReload',   dom: '_tbReload' },
                       '|<'       : {tip: 'Move to first',         typ : 'btn', icn : 'step-backward',         clk : '_toolBarSkFrst',   dom: '_tbSkFrst' },
                       '<<'       : {tip: 'Jump to previous page', typ : 'btn', icn : 'backward',              clk : '_toolBarJmPrev',   dom: '_tbJmPrev' },
                       '<'        : {tip: 'Previous page',         typ : 'btn', icn : 'play previousPage',     clk : '_toolBarGo2Prv',   dom: '_tbGoPrev' },
                       'textBox'  : {tip: '',                      typ : 'txt',                                clk : '_toolBarTxtBox',   dom: '_tbTxtBox' },
                       '>'        : {tip: 'Next page',             typ : 'btn', icn : 'play',                  clk : '_toolBarGo2Nex',   dom: '_tbGoNext' },
                       '>>'       : {tip: 'Jump to next page',     typ : 'btn', icn : 'forward',               clk : '_toolBarJmNext',   dom: '_tbJmNext' },
                       '>|'       : {tip: 'Move to end',           typ : 'btn', icn : 'step-forward',          clk : '_toolBarSekEnd',   dom: '_tbSekEnd' },
                       '>|...'    : {tip: 'Move to end then Tail', typ : 'btn', icn : 'step-forward ellipsis', clk : '_toolBarSkTail',   dom: '_tbSkTail' },
                       '...'      : {tip: 'Skip to end and Tail',  typ : 'btn', icn : 'fetch-only ellipsis',   clk : '_toolBarSkipTl',   dom: '_tbSkipTl' },
                       'commit'   : {tip: 'Commit changes',        typ : 'btn', icn : 'check',                 clk : '_toolBarCommit',   dom: '_tbCommit' },
                       'rollback' : {tip: 'Discard changes',       typ : 'btn', icn : 'times',                 clk : '_toolBarDiscrd',   dom: '_tbDiscrd' }},

    // dialog context menus
    _dlgTtlCnxtMnu  : {'Edit SQL'       : '_editCmd',
                       'Save ddView'    : '_saveView',
                       'Save ddView As' : '_saveViewAs',
                       'Rename ddView'  : '_renameView',
                       'Delete ddView'  : '_deleteView',
                       'Export Csv'     : '_exportCsv',
                       'Send Data'      : '_activateSender',
                       'Receive Data'   : '_activateReceiver',
                       'Cache Data'     : '_cacheData'},

    // slick context menus
    _slkHdrCnxtMnu  : {'Hide'             : '_hide',
                       'UnHide'           : '_unhide',
                       'Filter...'        : '_filterColumn',
                       'Filter Clear'     : '_filterClear',
                       'Sort...'          : '_sort',
                       'Sort ASC'         : '_sortAsc',
                       'Sort DESC'        : '_sortDesc',
                       'Sort Clear'       : '_sortClear',
                       'Histogram'        : '_showHistogram',
                       'Statistics'       : '_showStatisticsFull',
                       'Toggle Grouping'  : '_toggleGrouping',
                       'Shrink'           : '_shrinkColumn',
                       'Fit to Data'      : '_fitColumnToData'},
    _slkCellCnxtMnu : {'Browse Data'      : '_browseCellData',
                       'Filter'           : '_filterCell',
                       'Filter...'        : '_filterCellDialog',
                       'Edit'             : '_editErlangTerm',
                       'Truncate Table'   : '_truncateTable',
                       'Statistics'       : '_showStatistics',
                       'Drop Table'       : '_dropTable',
                       'Update Sql'       : '_updateSql',
                       'Snapshot Table'   : '_snapshotTable',
                       'Insert Sql'       : '_insertSql',
                       'Restore Table'    : '_restoreTable',
                       'Restore Table As' : '_restoreTableAs'},

    // These options will be used as defaults
    options: {
        // dialog options default override
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
        position          : {at: "left top", my: "left top", of: "#main-body", collision : 'none'},
        appendTo          : "#main-body",
        focus             : function() {},
        open              : function() {
            console.log("the parent of this", this.parentNode);
            var titleNode = $(this).parent().children(".ui-dialog-titlebar");
            console.log("the title node", titleNode);
            $(this).table('showPlane');
        },
        close             : function() {
                              $(this).table('close_stmt');
                              $(this).table('closeGraphs');
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
        dderlStatement    : null,
        dderlCmd          : null,
        dderlCmdStrs      : null,
        dderlClmlay       : null,
        dderlTbllay       : null,
        dderlViewId       : null,
        dderlStartBtn     : '>',
        dderlSortSpec     : null,
        dderlSqlEditor    : null,
    },
 
    // Set up the widget
    _create: function() {
        var self = this;

        self._origcolumns = {};
        self._gdata = [];
        self._graphDivs = [];

        self._fnt = $(document.body).css('font-family');
        self._fntSz = $(document.body).css('font-size');

        // preserve some options
        if(self.options.dderlConn       !== self._conn)     self._conn      = self.options.dderlConn;
        if(self.options.dderlAdapter    !== self._adapter)  self._adapter   = self.options.dderlAdapter;
        if(self.options.dderlStatement  !== self._stmt)     self._stmt      = self.options.dderlStatement;
        if(self.options.dderlCmd        !== self._cmd)      self._cmd       = self.options.dderlCmd;
        if(self.options.dderlCmdStrs    !== self._cmdStrs)  self._cmdStrs   = self.options.dderlCmdStrs;
        if(self.options.dderlClmlay     !== self._clmlay)   self._clmlay    = self.options.dderlClmlay;
        if(self.options.dderlTbllay     !== self._tbllay)   self._tbllay    = self.options.dderlTbllay;
        if(self.options.dderlViewId     !== self._viewId)   self._viewId    = self.options.dderlViewId;
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
        if(!self._cmdStrs) {
            self._cmdStrs = [];
        }
        // add the initial sql query to the history
        if(self._cmd) {
            self._addToEditorHistory(self._cmd);
        }

        // dialog elements

        // field for text width measurement in pixels
        // added to document.body once
        if($('#txtlen').length === 0) {
            self._txtlen =
                $('<span>')
                .attr('id', 'txtlen')
                .css('visibility', 'hidden')
                .css('display', 'none')
                .css('font-family', self._fnt)
                .css('font-size', self._fntSz)
                .appendTo(document.body);
        } else {
            self._txtlen =
                $('#txtlen')
                .css('font-family', self._fnt)
                .css('font-size', self._fntSz);
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

        self._initPlanes(self._tbllay);
        // setting up the event handlers last to aid debuggin
        self._setupEventHandlers();
    },

    _init: function() {
        // default dialog open behavior
    	if ( this.options.autoOpen ) {
            this._dlg.dialog("open");
        }
    },

    _createContextMenus: function() {
        var self = this;

        self._cnxtMenu('_slkCellCnxtMnu'); // cell context menu
        self._cnxtMenu('_slkHdrCnxtMnu');  // header context menu
        self._cnxtMenu('_dlgTtlCnxtMnu');  // header context menu
    },
                    
    // create the context menu and add them to document.body
    // only if they do not exist
    // TODO: Create a context menu once per table instead of the global
    //       to allow dynamic menu options depending on the column content.
    _cnxtMenu: function(_menu) {

        function executeMenuAction() {
            var self = $('#' + _menu).data('cnxt');
            if (self) {
                var columnId = null;
                console.log('self title _cnxtMenu ' + self.options.title);
                if (_menu === '_slkHdrCnxtMnu') {
                    columnId = $('#' + _menu).data('column-id');
                }
                self[_menu].dom.hide();
                self._cnxtMenuAction(_menu, $(this).attr("action"), columnId);
            }
        }

        if($('#'+_menu).length === 0) {
            var mnu = $('<ul>')
                .attr('id', _menu)
                .addClass("context_menu")
                .hide()
                .mouseleave(function(e) {
                    var self = $('#'+_menu).data('cnxt');
                    e.preventDefault();
                    if($(this).is(':visible')) {
                        $(this).hide();
                        self._grid.focus();
                    }
                })
                .appendTo(document.body);

            for(var m in this[_menu]) {
                if($.type(this[_menu][m]) === "string") {
                    $('<li>')
                        .attr("action", m)
                        .click(executeMenuAction)
                        .text(m)
                        .appendTo(mnu);
                }
            }
            this[_menu].dom = mnu;
        }
    },

    // delegate actions of context menu
    _cnxtMenuAction: function(_menu, _action, _columnId) {
        var funName = this[_menu][_action];
        var fun = $.proxy(this[funName], this);
        if($.isFunction(fun)) {
            var data = null;
            switch(_menu) {
                case '_slkHdrCnxtMnu':
                    if(_action === "Histogram") {
                        let _ranges = this._grid.getSelectionModel().getSelectedRanges();
                        var _columnIds = [];
                        for (var i = 0; i < _ranges.length; i++) {
                            _columnIds[i] = _ranges[i].fromCell;
                        }
                        data = {ranges: this._grid.getSelectionModel().getSelectedRanges(),
                                columnIds: _columnIds};
                    } else if(_action === "Toggle Grouping") {
                        data = {ranges: this._grid.getSelectionModel().getSelectedRanges(),
                                columnId: _columnId};
                    } else {
                        data = this._grid.getSelectionModel().getSelectedRanges();
                    }
                break;
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
        } else {
            throw('unimplimented fun '+funName+' for \''+_action+ '\' in '+_menu);
        }
    },

    _editCmd: function() {
        var self = this;
        if(self._divSqlEditor && self._divSqlEditor.hasClass('ui-dialog-content')) {
            self._divSqlEditor.dialog("moveToTop");
        } else {
            var script = "";
            if(self._planeSpecs && self._planeSpecs.length) {
                script = self._planeSpecs[0].script;
            }

            self._divSqlEditor = $('<div>')
                .appendTo(document.body)
                .sql({autoOpen  : false,
                      title     : this.options.title,
                      cmdOwner  : this._dlg,
                      history   : this._cmdStrs,
                      cmdFlat   : this._cmd,
                      optBinds  : this._optBinds,
                      script    : script // TODO: This should be multiple specs...
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
        if(this._viewId) {
            this._updateView(this._viewId, this.options.title);
        } else {
            this._saveNewView(this.options.title, false);
        }
    },

    /*
     * Renaming a view
     */
    _renameView: function() {
        if(("All ddViews" === this.options.title) || ("Remote Tables" === this.options.title)) {
            alert_jq("Error: The ddView '" + this.options.title + "' may not be renamed");
        } else {
            var self = this;
            prompt_jq({label: "ddView new name", value: self.options.title, content: ''},
                function(viewName) {
                    if (viewName) {
                        if(self._viewId) {
                            console.log("saving "+self._viewId+" with name "+viewName);
                            var renameView = {view_op : {operation : "rename", view_id : self._viewId, newname : viewName}};
                            self._ajax('view_op', renameView, 'view_op', 'opViewResult');
                        } else {
                            self._setTitleHtml($('<span>').text(viewName).addClass('table-title'));
                        }
                        self.options.title = viewName;
                    }
                });
        }
    },

    /*
     * Delete a view
     */
    _deleteView: function() {
        if(("All ddViews" === this.options.title) || ("Remote Tables" === this.options.title)) {
            alert_jq("Error: The ddView '" + this.options.title + "' may not be deleted");
        } else if(this._viewId) {
            var self = this;
            var viewName = self.options.title;
            confirm_jq({title: "Confirm delete view " + viewName, content: ''},
                function() {
                    console.log("deleting a view " + self._viewId + " with name " + viewName);
                    var delView = {view_op : {operation : "delete", view_id : self._viewId, newname : ""}};
                    self._ajax('view_op', delView, 'view_op', 'opViewResult');
                });
        } else {
            alert_jq("Error: \"" + this.options.title + "\" is not a view and therefore may not be deleted here!");
        }
    },

    /*
     * Exported save view doesn't allow updates.
     */

    saveView: function() {
        if(!this._viewId) {
            this._saveNewView(this.options.title, false);
        }
    },

    getViewId: function() {
        return this._viewId;
    },

    _saveViewAs: function() {
        var self = this;
        var args = {label: "ddView name", content: '', value: self.options.title};
        prompt_jq(args, function (viewName) {
            if (viewName) {
                self._saveViewWithName(viewName, false);
            }
        });
    },

    _exportCsv: function() {        
        var filename = this.options.title;
        var csv_ext = /\.csv$/g;
        if(!csv_ext.test(filename))
            filename += '.csv';
        var cmd_str = this._cmd;

        var adapter = this._adapter;
        var connection = dderlState.connection;
        var dderl_sess = dderlState.session ? '' + dderlState.session : '';
        var binds = JSON.stringify(this._optBinds && this._optBinds.hasOwnProperty('pars') ?
            this._optBinds.pars : null);

        prompt_jq(
            {label: "Download CSV", value:filename, content: ''},
            function(fileNewName) {
                $('<iframe>')
                .on('load',function() {
                    var iframe = $(this);
                    var form = $('<form method="post" action="app/download_query">')
                        .append($('<input type="hidden" name="dderl-session">').val(dderl_sess))
                        .append($('<input type="hidden" name="connection">').val(connection))
                        .append($('<input type="hidden" name="dderl-adapter">').val(adapter))
                        .append($('<input type="hidden" name="fileToDownload">').val(fileNewName))
                        .append($('<input type="hidden" name="queryToDownload">').val(cmd_str))
                        .append($('<input type="hidden" name="binds">').val(binds));
                    $(this).contents().find('body').append(form);
                    form.submit();
                    setTimeout(function() {iframe.remove();}, 500);
                })
                .appendTo(document.body);
            });
    },

    _getTableLayout: function(_viewName) {
        // Column names and width.
        // Index starting at 1 to skip the id column.
        var colnamesizes = [];
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
        return {
            save_view: {
                table_layout: {
                    width: w,
                    height: h,
                    y: y,
                    x: x,
                    plane_to_show: this._planeToShow,
                    plane_specs: this._planeSpecs
                },
                conn_id: dderlState.connectionSelected.connection,
                column_layout: colnamesizes,
                name: _viewName,
                content: this._cmd
            }
        };
    },

    // Wrapper function used to get the layout from the sql editor. 
    getTableLayout: function() {
        return this._getTableLayout().save_view.table_layout;
    },

    _updateView: function(viewId, viewName) {
        var self = this;
        var saveView = self._getTableLayout(viewName);
        var updateView = {update_view : saveView.save_view};
        updateView.update_view.view_id = viewId;
        self._ajax('update_view', updateView, 'update_view', 'saveViewResult');
    },

    _saveViewWithName: function(_viewName, replace) {
        var self = this;

        var saveView = self._getTableLayout(_viewName);
        saveView.save_view.replace = replace;

        console.log('saving view '+JSON.stringify(saveView));
        self._ajax('save_view', saveView, 'save_view', 'saveViewResult');
    },

    _saveNewView: function(viewName, replace) {
        var self = this;

        var saveView = self._getTableLayout(viewName);
        saveView.save_view.replace = replace;

        console.log('saving view '+JSON.stringify(saveView));
        self._ajax('save_view', saveView, 'save_view', 'newViewResult');
    },

    _showHistogram: function(data) {
        var self = this;
        var columnIds = data.columnIds;
        console.log('show histogram ' + JSON.stringify(data));

        if (0 === columnIds) {
            alert_jq('Error: No appropriate column for the menu item "Histogram" selected!');
            return;
        }

        // Considering hidden columns
        var columnIdsEff = [];
        for(var i = 0; i < columnIds.length; i++) {
            columnIdsEff.push(self._origcolumns[self._grid.getColumns()[columnIds[i]].field]);
        }

        $('<div>').appendTo(document.body)
            .statsTable({
                title          : "Histogram",
                initialQuery   : self._cmd,
                columnIds      : columnIdsEff,
                dderlStatement : self._stmt,
                parent         : self._dlg
            })
            .statsTable('load', 'histogram');
    },

    _showStatistics: function(_ranges) {
        var self = this;
        var cellmin = _ranges[0].fromCell;
        var cellmax = _ranges[0].toCell;
        var rowmin = _ranges[0].fromRow;
        var rowmax = _ranges[0].toRow;
        for (var i = 0; i < _ranges.length; ++i) {
            if (cellmin > _ranges[i].fromCell) cellmin = _ranges[i].fromCell;
            if (cellmax < _ranges[i].toCell) cellmax = _ranges[i].toCell;
            if (rowmin > _ranges[i].fromRow) rowmin = _ranges[i].fromRow;
            if (rowmax < _ranges[i].toRow) rowmax = _ranges[i].toRow;
        }
        var cols = this._grid.getColumns();
        var selcols = [];
        for(var c = cellmin; c <= cellmax; ++c) {
            selcols[selcols.length] = self._origcolumns[cols[c].id];
        }
        var selrows = [];
        for(var r = rowmin; r <= rowmax; ++r) {
            selrows[selrows.length] = self._gdata[r].id;
        }

        var title = self.options.title + " statistics (selected range)";
        $('<div>').appendTo(document.body)
            .statsTable({
                title          : title,
                initialQuery   : self._cmd,
                columnIds      : selcols,
                rowIds         : selrows,
                dderlStatement : self._stmt,
                parent         : self._dlg
            })
            .statsTable('load', 'statistics');

        console.log('show statistics ' + JSON.stringify(_ranges));
    },

    _showStatisticsFull: function(_ranges) {
        if ((1 === _ranges.length) && (_ranges[0].fromCell === 0) &&
            (_ranges[0].toCell === 0) && (_ranges[0].fullCol === true)) {
            alert_jq('Error: No appropriate column for the menu item "Statistics" selected!');
            return;
        }

        var self = this;

        var cellmin = _ranges[0].fromCell;
        var cellmax = _ranges[0].toCell;
        for (var i = 0; i < _ranges.length; ++i) {
            if (cellmin > _ranges[i].fromCell) cellmin = _ranges[i].fromCell;
            if (cellmax < _ranges[i].toCell) cellmax = _ranges[i].toCell;
        }
        var cols = this._grid.getColumns();
        var selcols = [];
        for(var c = cellmin; c <= cellmax; ++c) {
            selcols[selcols.length] = self._origcolumns[cols[c].id];
        }

        var title = self.options.title + " statistics (full column)";
        $('<div>').appendTo(document.body)
            .statsTable({
                title          : title,
                initialQuery   : self._cmd,
                columnIds      : selcols,
                dderlStatement : self._stmt,
                parent         : self._dlg
            })
            .statsTable('load', 'statistics_full');

        console.log('show statistics ' + JSON.stringify(_ranges));
    },

    _getSelectedArea: function(_ranges) {
        var self = this;
        var cellmin = _ranges[0].fromCell;
        var cellmax = _ranges[0].toCell;
        var rowmin = _ranges[0].fromRow;
        var rowmax = _ranges[0].toRow;

        for (var i = 0; i < _ranges.length; ++i) {
            if (cellmin > _ranges[i].fromCell) cellmin = _ranges[i].fromCell;
            if (cellmax < _ranges[i].toCell) cellmax = _ranges[i].toCell;
            if (rowmin > _ranges[i].fromRow) rowmin = _ranges[i].fromRow;
            if (rowmax < _ranges[i].toRow) rowmax = _ranges[i].toRow;
        }

        cellmin = Math.max(cellmin, 1);

        var cols = this._grid.getColumns();

        var selcols = [];
        for(var c = cellmin; c <= cellmax; ++c) {
            selcols[selcols.length] = self._origcolumns[cols[c].id];
        }

        var selrows = [];
        for(var r = rowmin; r <= rowmax; ++r) {
            selrows[selrows.length] = self._gdata[r].id;
        }

        return {selrows: selrows, selcols: selcols};
    },

    _updateSql: function(_ranges) {
        this._getSql(_ranges, 'upd');
    },

    _insertSql: function(_ranges) {
        this._getSql(_ranges, 'ins');
    },

    _getSql: function(_ranges, Op) {
        var self = this;
        var selectedArea = self._getSelectedArea(_ranges);

        var getSql = {
            get_sql: {
                connection: dderlState.connection,
                statement: self._stmt,
                columnIds: selectedArea.selcols,
                rowIds: selectedArea.selrows,
                op: Op
            }
        };

        self._ajax('get_sql', getSql, 'get_sql', 'getSqlResult');
    },

    _openSqlEditor: function(sqlResult) {
        var self = this;
        var width = 500;
        // received response clear wait wheel
        self.removeWheel();

        if(sqlResult.hasOwnProperty('error')) {
            alert_jq(sqlResult.error);
        } else {
            var split = sqlResult.sql.split("\n");
            if((split.length > 0) && ((split[0].length * 8) > width)) {
                width = split[0].length * 8;
                if(width > 1000) {
                    width = 1000;
                }
            }
            $('<div>')
                .appendTo(document.body)
                .sql({autoOpen  : false,
                      title     : sqlResult.title,
                      width     : width,
                      cmdOwner  : null,
                      history   : this._cmdStrs
                     })
                .sql('open').sql('setFlatCmd', sqlResult.sql);
        }
    },

    _activateSender: function() {
        var self = this;
        var columnPos = self._getColumnPositions();
        self._ajax('activate_sender', {
            activate_sender: {
                connection: dderlState.connection,
                statement: self._stmt,
                column_positions: columnPos
            }
        }, 'activate_sender', 'activateSender');
    },

    _activateSenderResult: function(activationResult) {
        if(activationResult.hasOwnProperty('error')) {
            alert_jq(activationResult.error);
        } else {
            alert_jq("sender activated");
        }
    },

    _activateReceiver: function() {
        var self = this;
        var columnPos = self._getColumnPositions();
        self._ajax('activate_receiver', {
            activate_receiver: {
                connection: dderlState.connection,
                statement: self._stmt,
                column_positions: columnPos
            }
        }, 'activate_receiver', 'activateReceiver');
    },

    _activateReceiverResult: function(activationResult) {
        if(activationResult.hasOwnProperty('error')) {
            alert_jq(activationResult.error);
        } else {
            var msg = "Receiver activated: <br><br>";
            msg += "Use tail to see the data in real time. <br><br>";
            msg += "Available rows: " + activationResult.available_rows + "<br>";
            msg += "Sender columns: " + activationResult.sender_columns[0];
            for(var i = 1; i < activationResult.sender_columns.length; ++i) {
                msg += ",<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
                msg += activationResult.sender_columns[i];
            }

            alert_jq(msg);
        }
    },

    _cacheData: function() {
        var self = this;
        self._ajax('cache_data', {
            cache_data: {
                connection: dderlState.connection,
                statement: self._stmt,
            }
        }, 'cache_data', 'cacheResult');
    },

    _cacheResult: function(cacheResult) {
        if(cacheResult === "ok") {
            console.log('cache success!');
        } else if(cacheResult.hasOwnProperty('error')) {
            alert_jq('cache failed!\n' + cacheResult.error);
        }
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
    cmdReload: function(cmd, optBinds, button, planeData) {
        console.log('command reloading ['+cmd+']');
        // Close the stmt if we had one to avoid fsm leak independent of the result of the query.
        this.close_stmt();
        this._clmlay = null;
        this._cmd = cmd;
        this._optBinds = optBinds;
        this.options.dderlStartBtn = this._startBtn = button;
        this._filters = null;
        this._closeGraphs();
        this._initPlanes(planeData);
        this._ajax('query', {query: {
            connection: dderlState.connection,
            conn_id: dderlState.connectionSelected.connection,
            qstr : this._cmd,
            binds: (this._optBinds && this._optBinds.hasOwnProperty('pars')? this._optBinds.pars : null)
        }}, 'query', 'queryResult');
        this._dlg.dialog("moveToTop");
    },

    // columns hide/unhide
    _hide: function(_ranges) {
        if ((1 === _ranges.length) && (_ranges[0].fromCell === 0) &&
            (_ranges[0].toCell === 0) && (_ranges[0].fullCol === true)) {
            alert_jq('Error: No appropriate column for the menu item "Hide" selected!');
            return;
        }

        var self = this;
        var columns = self._grid.getColumns();
        var toHide = {};
        var j;
        for (var i=0; i<_ranges.length; ++i) {
            //Validate that id column is not on the selection.
            if(_ranges[i].fromCell !== 0 && _ranges[i].toCell !== 0)  {
                for(j = _ranges[i].fromCell; j <= _ranges[i].toCell; ++j) {
                    toHide[j] = true;
                }
            }
        }
        var toHideArray = [];
        for(j in toHide) {
            toHideArray[toHideArray.length] = parseInt(j);
        }
        toHideArray.sort(function(a,b) {return (a < b ? 1 : (a === b ? 0 : -1));});

        if(!self.hasOwnProperty('_hiddenColumns')) {
            self._hiddenColumns = [];
        }

        for(j=0; j < toHideArray.length; ++j) {
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

    _unhide: function() {
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
            self._sorts = {};
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
        if(self._sorts === null) { self._sorts = {}; }
        var cols = this._grid.getColumns();
        for (var i=0; i<_ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                self._sorts[cols[c].field] = {name : cols[c].name, asc : true};
            }
        }
        if(Object.keys(self._sorts).length === 1) {
            self._ajax('sort', {sort: {spec: self._sortSpec2Json(), statement: self._stmt}}, 'sort', 'sortResult');
        } else {
            self._showSortGui();
        }
    },
    _sortDesc: function(_ranges) {
        var self = this;
        if(self._sorts === null) {
            self._sorts = {};
        }
        var cols = this._grid.getColumns();
        for (var i = 0; i < _ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                self._sorts[cols[c].field] = {name : cols[c].name, asc : false};
            }
        }
        if(Object.keys(self._sorts).length === 1) {
            self._ajax('sort', {sort: {spec: self._sortSpec2Json(), statement: self._stmt}}, 'sort', 'sortResult');
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
            self._ajax('sort', {sort: {spec: [], statement: self._stmt}}, 'sort', 'sortResult');
        }
    },
    _showSortGui: function() {
        var self = this;

        // first check if we have a sort dialog open and close it.
        if(self._sortDlg && self._sortDlg.hasClass('ui-dialog-content')) {
            self._sortDlg.dialog("close");
        }

        var data = [];
        for (var s in self._sorts) {
            data.push({id: s, name: self._sorts[s].name, sort: (self._sorts[s].asc ? 'ASC' : 'DESC')});
        }

        self._sortDlg =
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
            .appendTo(self._sortDlg);

        // building slickgrid
        var sgrid = new Slick.Grid(sortDiv, data, [ // columns
            {
                id: "#",
                name: "",
                width: 40,
                behavior: "selectAndMove",
                selectable: true,
                resizable: false,
                formatter: Slick.Formatters.DragArrows,
                cssClass: "center"
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
                cannotTriggerInsert: true,
                formatter: Slick.Formatters.Sort,
                cssClass: "center"
            },
            {
                id: "select",
                name: "",
                field: "select",
                width: 40,
                selectable: true,
                formatter: Slick.Formatters.Trashcan,
                cssClass: "center"
            }
        ], {
            editable: true,
            enableAddRow: false,
            enableCellNavigation: true,
            autoEdit: false
        });

        sgrid.setSelectionModel(new Slick.RowSelectionModel());
        sgrid.onClick.subscribe(function(e, args) {
            var col = sgrid.getColumns()[args.cell].id;
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

            rows.sort(function (a, b) { return a - b; });

            for (let i = 0; i < rows.length; i++) {
                extractedRows.push(data[rows[i]]);
            }

            rows.reverse();

            for (let i = 0; i < rows.length; i++) {
                if (rows[i] < insertBefore) {
                    left.splice(rows[i], 1);
                } else {
                    right.splice(rows[i] - insertBefore, 1);
                }
            }

            data = left.concat(extractedRows.concat(right));

            var selectedRows = [];
            for (var i = 0; i < rows.length; i++) {
                selectedRows.push(left.length + i);
            }

            sgrid.resetActiveCell();
            sgrid.setData(data);
            sgrid.setSelectedRows(selectedRows);
            sgrid.render();
        });

        sgrid.registerPlugin(moveRowsPlugin);

        var saveChange = function() {
            var sd = sgrid.getData();
            self._sorts = {};
            for (var i = 0; i < sd.length; ++i) {
                self._sorts[sd[i].id] = {name : sd[i].name,
                                         asc : (sd[i].sort === 'ASC' ? true : false) };
            }
            return self._sortSpec2Json();
        };
        
        self._sortDlg.dialog({
            width : 336,
            modal : false,
            title : 'Sorts',
            dialogClass: 'btnSortClass',
            appendTo: "#main-body",
            rowHeight : self.options.slickopts.rowHeight,
            close : function() {
                $(this).dialog('close');
                $(this).remove();
            },
            buttons: [
                {
                    text: 'Sort',
                    click: function() {
                        var sortspec = saveChange();
                        self._ajax('sort', {sort: {spec: sortspec, statement: self._stmt}}, 'sort', 'sortResult');
                        $(this).dialog('close');
                        $(this).remove();
                    }
                }
            ]
        });

        self._sortDlg.dialog("widget").draggable("option", "containment", "#main-body");
        //Set the height of the sort dialog depending on the number of rows...
        var sortGridHeight = (data.length + 2) * self.options.slickopts.rowHeight;
        self._sortDlg.height(sortGridHeight);
        //Lets put it where we have space...
        smartDialogPosition($("#main-body"), self._dlg, self._sortDlg, ['center']);
        setTimeout(function() {
            var theSortButton = self._sortDlg.dialog("widget").find('button:contains("Sort")');
            theSortButton.focus();
            console.log(theSortButton);
        }, 50);
    },

    _ajax: function(url, data, resp, callback) {
        this.addWheel();
        ajaxCall(this, url, data, resp, callback);
    },
    
    _sortSpec2Json: function() {
        var self = this;
        var sortspec = [];
        for (var s in self._sorts) {
            let t = {};
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
            self._sorts = {};
        }

        var cols = self._grid.getColumns();
        for (let colpos in origJson) {
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
        if ((1 === _ranges.length) && (_ranges[0].fromCell === 0) &&
            (_ranges[0].toCell === 0) && (_ranges[0].fullCol === true)) {
            alert_jq('Error: No appropriate column for the menu item "Filter..." selected!');
            return;
        }

        var self = this;
        var cols = self._grid.getColumns();
        if(self._filters === null) {
            self._filters = {};
        }
        for (var i=0; i<_ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                if(!self._filters.hasOwnProperty(cols[c].field)) {
                    var filterColumnName = cols[c].name;
                    if (filterColumnName.length > 30) {
                        filterColumnName = filterColumnName.substring(1, 27) + "...";
                    }
                    self._filters[cols[c].field] =
                        {
                            inp : $('<textarea>')
                                .attr('type', "text")
                                .css('margin', 0)
                                .css('overflow','auto')
                                .css('padding', 0),
                            typeSelect : self._createFilterOptions($('<select>').css('width', '90px')),
                            vals: {},
                            name: filterColumnName
                        };
                }
            }
        }
        self._showFilterGui();
    },

    _createFilterOptions: function(select) {
        var filterTypes = [{value: "$in$", text: "In"},
                           {value: "$not_in$", text: "Not in"},
                           {value: "$like$", text: "Like"},
                           {value: "$not_like$", text: "Not like"}];

        for(var i = 0; i < filterTypes.length; ++i) {
            var option = "<option value='" + filterTypes[i].value + "'>" + filterTypes[i].text + "</option>";
            select.append($(option));
        }
        return select;
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
            self._ajax('filter', {filter: {spec: {'undefined':[]}, statement: self._stmt}}, 'filter', 'filterResult');
        }
    },

    _filterCellDialog: function(_ranges) {
        this._filter(_ranges, true);
    },

    _filterCell: function(_ranges) {
        this._filter(_ranges, false);
    },

    _filter: function(_ranges, showDialog) {
        var self = this;
        if(self._filters === null) {
            self._filters = {};
        }
        var cols = this._grid.getColumns();
        for (var i = 0; i < _ranges.length; ++i) {
            var fromCell = Math.max(_ranges[i].fromCell, 1);
            for(let c = fromCell; c <= _ranges[i].toCell; ++c) {
                if(!self._filters.hasOwnProperty(cols[c].field)) {
                    var filterColumnName = cols[c].name;
                    if (filterColumnName.length > 30) {
                        filterColumnName = filterColumnName.substring(1, 27) + "...";
                    }
                    self._filters[cols[c].field] =
                        {
                            inp : $('<textarea>')
                                .attr('type', "text")
                                .css('margin', 0)
                                .css('overflow','auto')
                                .css('padding', 0),
                            typeSelect : self._createFilterOptions($('<select>').css('width', '90px')),
                            vals: {},
                            name: filterColumnName
                        };
                }
                for(var r=_ranges[i].fromRow; r <= _ranges[i].toRow; ++r) {
                    self._filters[cols[c].field].vals[this._gdata[r][cols[c].field].replace(/\n/g,'\\n')] = true;
                }
            }
        }
        if(Object.keys(self._filters).length === 1 && !showDialog) {
            for(let c in self._filters) {
                var strs = [];
                for(let s in self._filters[c].vals) {
                    strs.push(s);
                }
                self._filters[c].inp.val(strs.join('\n'));
            }
            var filterspec = self._filterSpec2Json('and');
            self._ajax('filter', {filter: {spec: filterspec, statement: self._stmt}}, 'filter', 'filterResult');
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
        var fCount = Object.keys(self._filters).length;

        self._fltrDlg =
            $('<div>')
            .css('width', 370)
            .appendTo(document.body);

        var fltrTbl =
            $('<table>')
            .css('height', '100%')
            .css('width', '100%')
            .attr('border', 0)
            .attr('cellpadding', 0)
            .attr('cellspacing', 0)
            .appendTo(self._fltrDlg);

        for(let c in self._filters) {
            var strs = [];
            for(let s in self._filters[c].vals) {
                strs.push(s);
            }
            self._filters[c].inp.val(strs.join('\n'));
            $('<tr>')
                .append('<td>'+ self._filters[c].name +'</td>')
                .append($('<td>').append(self._filters[c].typeSelect))
                .appendTo(fltrTbl);
            $('<tr>')
                .append($('<td colspan = "2">').append(self._filters[c].inp))
                .appendTo(fltrTbl);
        }

        self._fltrDlg
            .dialog({
                width: fltrTbl.width(),
                modal: false,
                title:'Filter',
                position: { my: "left top", at: "left bottom", of: this._dlg },
                close: function () {
                    $(this).dialog('close');
                    $(this).remove();
                },
                resize: function() {
                    var dH = $(this).height() / fCount - 30;
                    var dW = $(this).width() - 30;
                    for(let c in self._filters) {
                        self._filters[c].inp.width(dW);
                        self._filters[c].inp.height(dH);
                    }
                },
                appendTo: "#main-body"
            });

        self._fltrDlg.dialog("widget").draggable("option","containment", "#main-body");
        //Lets put it where we have space...
        smartDialogPosition($("#main-body"), this._dlg, self._fltrDlg, ['center']);

        var applyFiltersFn = function(type) {
            var filterspec = self._filterSpec2Json(type);
            self._ajax('filter', {filter: {spec: filterspec, statement: self._stmt}}, 'filter', 'filterResult');
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

        var dH = self._fltrDlg.height() / fCount - 21;
        var dW = self._fltrDlg.width() - 30;
        for(let c in self._filters) {
            self._filters[c].inp.width(dW);
            self._filters[c].inp.height(dH);
        }
        for(let c in self._filters) {
            self._filters[c].inp.focus();
            break;
        }
    },

    _filterSpec2Json: function(type) {
        var self = this;
        var filterspec = {};
        filterspec[type] = [];
        for(var c in self._filters) {
            var _vStrings = self._filters[c].inp.val().split('\n');
            if (_vStrings.length === 1 && _vStrings[0].length === 0) _vStrings = [];
            var vStrings = [];
            self._filters[c].vals = {};
            for (var i=0; i < _vStrings.length; ++i) {
                vStrings[i] = _vStrings[i].replace(/\\n/g,'\n');
                self._filters[c].vals[_vStrings[i]] = true;
            }
            var fltr = {};
            fltr[self._origcolumns[c]] = vStrings;
            if(vStrings.length > 0) {
                var filterType = self._filters[c].typeSelect.val();
                fltr[self._origcolumns[c]].unshift(filterType);
                console.log("Here we add the filter type " + filterType  + " at the front");
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
    
    // table actions
    _browseCellData: function(ranges) {
        var self = this;
        console.log('_browseCellData for '+ ranges.length + ' slick range(s)');
        
        var cells = [];
        for(let i = 0; i < ranges.length; ++i) {
            // For complete rows use the first column only
            if(ranges[i].fromCell <= 1 &&
                ranges[i].toCell === self._grid.getColumns().length - 1) {
                for (let j = ranges[i].fromRow; j <= ranges[i].toRow; ++j) {
                    let data = self._gridDataView.getItem(j);
                    let column = self._grid.getColumns()[1];
                    cells.push({
                        row: data.id,
                        col: self._origcolumns[column.field]
                    });
                }
            } else {
                for (let j = ranges[i].fromRow; j <= ranges[i].toRow; ++j) {
                    for(let k = Math.max(ranges[i].fromCell, 1); k <= ranges[i].toCell; ++k) {
                        let data = self._gridDataView.getItem(j);
                        let column = self._grid.getColumns()[k];
                        cells.push({
                            row: data.id,
                            col: self._origcolumns[column.field]
                        });
                    }
                }
            }
        }
        console.log("the cells", cells);
        if(cells.length > 10) {
            alert_jq("A maximum of 10 tables can be open simultaneously");
        } else {
            for(let i = 0; i < cells.length; ++i) {
                self._ajax('browse_data',
                       { browse_data: {connection : dderlState.connection,
                                           conn_id : dderlState.connectionSelected.connection,
                                           statement : self._stmt,
                                           row : cells[i].row,
                                           col : cells[i].col}},
                       'browse_data', 'browseData');
            }
        }
    },

    _runTableCmd: function(tableCmd, callback, tables) {
        var self = this;

        console.log(tableCmd + ' for '+ tables);

        var context = {};
        context[tableCmd] = {connection  : dderlState.connection,
                             statement   : self._stmt,
                             table_names : tables};
        self._ajax(tableCmd, context, tableCmd, callback);
        self._grid.resetActiveCell();
        self._grid.setSelectedRows([]);
    },
    _get_range_values: function(ranges) {
        var self = this;
        var columns = self._grid.getColumns();
        var rangeValues = [];
        console.log('select ranges ' + ranges.length + ' slick range(s)');

        for(var i = 0; i < ranges.length; ++i) {
            for(var j = ranges[i].fromCell; j <= ranges[i].toCell; ++j) {
                var cell = columns[j];
                for(var k = ranges[i].fromRow; k <= ranges[i].toRow; ++k) {
                    var row    = self._gdata[k];
                    rangeValues.push(row[cell.field]);
                }
            }
        }

        return rangeValues;
    },
    _truncateTable: function(ranges) {
        var self = this;
        var truncateTables = self._get_range_values(ranges);
        confirm_jq({title: "Confirm truncate", content:truncateTables}, 
                function() {
                    self._runTableCmd.apply(self, ['truncate_table', 'truncateResult', truncateTables]);
                });
    },
    _dropTable: function (ranges) {
        var self = this;
        var dropTables = self._get_range_values(ranges);
        confirm_jq({title: "Confirm delete", content:dropTables}, 
                function() {
                    self._runTableCmd.apply(self, ['drop_table', 'dropResult', dropTables]);
                });
    },
    _snapshotTable: function(ranges) {
        var self = this;
        var snapshotTables = self._get_range_values(ranges);
        self._runTableCmd.apply(self, ['snapshot_table', 'snapshotResult', snapshotTables]);
    },
    _restoreTable: function(ranges) {
        var self = this;
        var restoreTables = self._get_range_values(ranges);
        confirm_jq({title: "Confirm restore from snapshot", content:restoreTables}, 
                function() {
                    self._runTableCmd.apply(self, ['restore_table', 'restoreResult', restoreTables]);
                });
    },
    _restoreTableAs: function(ranges) {
        var self = this;
        var tables = [];
        var columns = self._grid.getColumns();

        for(var i = 0; i < ranges.length; ++i) {
            for(var j = ranges[i].fromCell; j <= ranges[i].toCell; ++j) {
                var cell = columns[j];
                for(var k = ranges[i].fromRow; k <= ranges[i].toRow; ++k) {
                    var row    = self._gdata[k];
                    tables.push(row[cell.field]);
                }
            }
        }

        this._showRestoreAs(tables);
    },

    _showRestoreAs: function(tables) {
        var self = this;
        if(self._restoreAsDlg && self._restoreAsDlg.hasClass('ui-dialog-content')) {
            self._restoreAsDlg.dialog("close");
        }

        self._restoreAsDlg =
            $('<div>')
            .css('width', 336)
            .appendTo(document.body);

        var restorAsTbl =
            $('<table>')
            .css('height', '100%')
            .css('width', '100%')
            .attr('border', 0)
            .attr('cellpadding', 0)
            .attr('cellspacing', 0)
            .appendTo(self._restoreAsDlg);

        var restoreAsData = [];
        var fCount = 0;
        var maxFieldLen = 0; 
        for(fCount = 0; fCount < tables.length; ++fCount) {
            if (maxFieldLen < tables[fCount].length)
                maxFieldLen = tables[fCount].length;
            restoreAsData[fCount] = {field : tables[fCount],
                                inp : $('<input>')
                                    .attr('type', 'text')
                                    .css('display', 'table-cell')
                                    .css('width', '100%')
                                    .val(tables[fCount])};
            $('<tr>')
                .append($('<td>')
                            .css('width','1%')
                            .css('white-space','nowrap')
                            .append(restoreAsData[fCount].field)
                            .append('&nbsp;'))
                .append($('<td>').append(restoreAsData[fCount].inp))
                .appendTo(restorAsTbl);
        }

        self._restoreAsDlg
            .dialog({
                width :   100 + 12 * maxFieldLen,
                modal :   false,
                title :   'Restore As',
                appendTo: "#main-body",
                position : {my: "left top", at: "left bottom", of: this._dlg},
                close : function() {
                        $(this).dialog('close');
                        $(this).remove();
                    },
                resize: function() {}
            });

        self._restoreAsDlg.dialog("widget").draggable("option","containment", "#main-body");
        smartDialogPosition($("#main-body"), this._dlg, self._restoreAsDlg, ['center']);

        var applyRestoreAsFn = function() {
            var restoreAsJson = {};
            for(var t = 0; t < restoreAsData.length; ++t) {
                restoreAsJson[restoreAsData[t].field] = restoreAsData[t].inp.val();
            }
            self._ajax('restore_tables_as', {restore_tables_as: restoreAsJson, connection : dderlState.connection},
                    'restore_tables_as', 'restoreAsResult');
            $(this).dialog('close');
            $(this).remove();
        };

        self._restoreAsDlg.dialog('option', 'buttons', [
                { text: 'Restore', click: function() { applyRestoreAsFn.apply(this); } }
            ]);
    },

    _editErlangTerm: function (_ranges) {
        var self = this;
        if(!self._singleCellSelected(_ranges)) {
            throw('cell level \'Edit\' don\'t support multiples and ranges');
        } else {
            var cell = _ranges[0];
            var columnField = self._grid.getColumns()[cell.fromCell].field;
            var stringToFormat = self._gdata[cell.fromRow][columnField];
            if(stringToFormat.lastIndexOf("data:image", 0) === 0) {// Check if it is a base64 image.
                self._openImageEditor(stringToFormat);
            } else {
                var data = self._gridDataView.getItem(cell.fromRow);
                self._erlangCellPos = {row: cell.fromRow, cell: cell.fromCell};
                self._ajax('edit_term_or_view', {
                    edit_term_or_view: {
                       connection : dderlState.connection,
                       statement : self._stmt,
                       row : data.id,
                       erlang_term: stringToFormat,
                       expansion_level: "auto",
                       force: false
                   }
               }, 'edit_term_or_view', 'editTermOrView');
            }
        }
    },

    _createSlickGrid: function() {
        var self = this;

        // building slickgrid
        //
        self._gridDataView = new Slick.Data.DataView();

        // a dummy column needed to be added to enable slickgrid to enable column re-order
        self._grid = new Slick.Grid(self._tableDiv, self._gridDataView, [{id: "_"}], self.options.slickopts);
        self._grid.setSelectionModel(new Slick.CellRowColSelectionModel());
        var copyManager = new Slick.CellExternalCopyManager();
        self._grid.registerPlugin(copyManager);
        self._grid.registerPlugin(new Slick.Data.GroupItemMetadataProvider());
        copyManager.onPasteCells.subscribe($.proxy(self._gridPasteCells, self));

        self._grid.onContextMenu.subscribe($.proxy(self._gridContextMenu, self));
        self._grid.onHeaderContextMenu.subscribe($.proxy(self._gridHeaderContextMenu, self));
        self._grid.onCellChange.subscribe($.proxy(self._gridCellChange, self));
        self._grid.onBeforeEditCell.subscribe($.proxy(self._gridBeforeEdit, self));
        self._grid.onBeforeCellEditorDestroy.subscribe($.proxy(self._gridAfterEdit, self));
        self._grid.onAddNewRow.subscribe($.proxy(self._gridAddNewRow, self));
        self._grid.onColumnsReordered.subscribe($.proxy(self._gridColumnsReorder, self));
        self._grid.onColumnsResized.subscribe($.proxy(self._gridColumnsResize, self));
        self._grid.onKeyDown.subscribe($.proxy(self._handleKeyDown, self));
        self._grid.onClick.subscribe($.proxy(self._handleClick, self));
        self._grid.onMouseDown.subscribe($.proxy(self._handleMouseDown, self));
        self._grid.onDragInit.subscribe($.proxy(self._handleDragInit, self));
        self._grid.onMouseEnter.subscribe($.proxy(self._handleMouseEnter, self));
        self._grid.onMouseLeave.subscribe($.proxy(self._handleMouseLeave, self));
        self._grid.onSelectedRowsChanged.subscribe($.proxy(self._handleSelectionChanged, self));

        // wire up model events to drive the grid
        self._gridDataView.onRowCountChanged.subscribe(function () {
            self._grid.updateRowCount();
            self._grid.render();
        });
        self._gridDataView.onRowsChanged.subscribe(function (e, args) {
            self._grid.invalidateRows(args.rows);
            self._grid.render();
        });
        self._gdata = self._gridDataView.getItems();

        // to be accessed from external class context
        self._grid.gridowner = self;
    },

    _setupEventHandlers: function() {
        // make this as context to private event handler functions
        // and register for named events
        for(var fun in this._handlers) {
            this.element.on(fun, null, this, this._handlers[fun]);
        }
    },

    _shrinkColumn: function(selectedRange) {
        var self = this;
        var columns = self._grid.getColumns();
        for(var i = 0; i < selectedRange.length; ++i) {
            for(var j = selectedRange[i].fromCell; j <= selectedRange[i].toCell; ++j) {
                columns[j].width = 35;
            }
        }
        self._grid.setColumns(columns);
    },

    _fitColumnToData: function(selectedRange) {
        var self = this;
        var columns = self._grid.getColumns();
        for(var i = 0; i < selectedRange.length; ++i) {
            for(var j = selectedRange[i].fromCell; j <= selectedRange[i].toCell; ++j) {
                columns[j].width = 35;
                var maxLength = 0;
                for(var k = 0; k < self._gdata.length; ++k) {
                    var row = self._gdata[k];
                    var field = columns[j].field;
                    if(row[field].length > maxLength) {
                        maxLength = row[field].length;
                        var fieldWidth = self._txtlen.text(row[field]).width();
                        fieldWidth = fieldWidth + 0.4 * fieldWidth;
                        if(columns[j].width < fieldWidth) {
                            columns[j].width = Math.min(fieldWidth, self._MAX_ROW_WIDTH);
                        }
                    }
                }
            }
        }
        self._grid.setColumns(columns);
    },

    getSelf: function() {
        return this;
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

        var toolElmFn = function (e) {
            var self = e.data;
            var _btn = $(this).data('tag');
            var fName = self._toolbarButtons[_btn].clk;
            //var f = $.proxy(self[fName], self);
            var f = self[fName];
            if ($.isFunction(f)) {
                f(self);
                self._grid.focus();
            } else {
                throw ('[' + self.options.title + '] toolbar ' + _btn + ' has unimplimented cb ' + fName);
            }
        };

        var footerTxtKeypressHandler = function (evt) {
            if (evt.which == 13) {
                var rownum = parseInt($(this).val());
                if (!isNaN(rownum)) {
                    self._toolBarTxtBoxVal = rownum;
                    evt.data = self;
                    toolElmFn.call(this, evt);
                }
            }
            return true;
        };

        // footer items
        for(var btn in self._toolbarButtons) {
            var btnTxt = self._toolbarButtons[btn].tip;
            var elm = self._toolbarButtons[btn];
            var inph = self.options.toolBarHeight;

            if(elm.typ === 'btn') {
                self[elm.dom] =
                    $('<button>')
                    .text(btnTxt)
                    .data('tag', btn)
                    .button({icons: {primary: 'fa fa-' + elm.icn}, text: false})
                    .css('height', inph+'px')
                    .addClass('colorIcon')
                    .click(self, toolElmFn)
                    .appendTo(self._footerDiv);
            } else if(elm.typ === 'txt') {
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
                    .keypress(footerTxtKeypressHandler)
                    .appendTo(self._footerDiv);
            }
        }
        self._footerDiv
            .buttonset()
            .css('height', (self.options.toolBarHeight)+'px');

        // footer total width
        var childs = self._footerDiv.children();
        var totWidth = 0;
        for(var i=0; i<childs.length; ++i) {
            totWidth += $(childs[i]).width();
        }

        self._footerWidth = totWidth;
    },

    /*
     * Toolbar callbak functions
     */
    // NOTE: self is 'this' and 'this' is dom ;)
    _toolBarReload: function(self) {
        console.log('['+self.options.title+']'+' reloading '+self._cmd);
        // Do not keep the layout if the table is empty.
        if(self._grid.getDataLength() !== 0) {
            var viewInfo = self._getTableLayout("");
            self._clmlay = viewInfo.save_view.column_layout;
            self._tbllay = viewInfo.save_view.table_layout;
        }
        self._gridDataView.setGrouping([]);
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
        if(!self._atTop()) {
            self._scrollBack();
        } else {
            self.buttonPress("<");
        }
    },
    _toolBarTxtBox: function(self) {
        if(self.hasOwnProperty('_toolBarTxtBoxVal')) {
            console.log('['+self.options.title+'] cb _toolBarTxtBox '+self._toolBarTxtBoxVal);
            self.buttonPress(self._toolBarTxtBoxVal);
        }
    },
    _toolBarGo2Nex: function(self) {
        console.log('['+self.options.title+'] cb _toolBarGo2Nex');
        if(!self._atBottom()) {
            self._scrollNext();
        } else {
            self.buttonPress(">");
        }
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
    _toolBarClearG: function(evt) {
        var self = evt.data;
        console.log("clear graph");
        if(self._graphSpec && $.isFunction(self._graphSpec.on_reset)) {
            try {
                self._graphSpec.on_reset();
            } catch(e) {
                alert_js_error(e);
            }
        }
    },
    ////////////////////////////
    
    /*
     * ajaxCall success callbacks
     */
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
        var self = this;
        self.removeWheel();
        if(_saveView === "ok") {
             console.log('[AJAX] view saved!');
        } else if(_saveView.hasOwnProperty('view_id')) {
            console.log('[AJAX] view saved, id = ' + _saveView.view_id + ' and name ' + _saveView.name);
            self.options.title = _saveView.name;
            if(!self._viewId) {
                self._viewId = _saveView.view_id;
                addToCurrentViews(self);
            } else {
                self._viewId = _saveView.view_id;
            }
            updateWindowTitle(self._windowFinderTextLink, self.options.title);
            self._setTitleHtml($('<span>').text(self.options.title).addClass('table-title'));
        } else if(_saveView.hasOwnProperty('need_replace')) {
            $('<div><p><span class="ui-icon ui-icon-alert" style="float: left; margin: 0 7px 20px 0;"></span>A view with that name already exists. Are you sure you want to replace it?</p></div>').appendTo(document.body).dialog({
                resizable: false,
                height:180,
                modal: true,
                appendTo: "#main-body",
                buttons: {
                    "Replace the view": function() {
                        $( this ).dialog( "close" );
                        self._saveViewWithName(_saveView.need_replace, true);
                    },
                    Cancel: function() {
                        $( this ).dialog( "close" );
                    }
                },
                open: function() {
                    $(this)
                        .dialog("widget")
                        .draggable("option","containment","#main-body");
                },
                close : function() {
                    $(this).dialog('destroy');
                    $(this).remove();
                }
            });
        } else if(_saveView.hasOwnProperty('error')) {
            alert_jq('failed to save view!\n'+_saveView.error);
        }
    },

    _operateViewResult: function(opView) {
        if(opView.hasOwnProperty('error')) {
            alert_jq('Failed to modify view!\n'+ opView.error);
        } else if(opView  === "ok") {
             console.log('[AJAX] view saved!');
        } else if(opView.hasOwnProperty('op')) {
            if(opView.op === "rename") {
                this._setTitleHtml($('<span>').text(opView.newname).addClass('table-title'));
                console.log('[AJAX] view renamed!');
            } else if(opView.op === "delete") {
                this._dlg.dialog('close');
                console.log('[AJAX] view deleted!');
            } else {
                alert_jq('Invalid response from server, trying to modify the view');
            }
        } else {
            alert_jq('Invalid response from server, trying to modify the view');
        }
    },

    _checkNewViewResult: function(_saveView) {
        var self = this;
        self.removeWheel();
        if(_saveView === "ok") {
             console.log('[AJAX] view saved!');
        } else if(_saveView.hasOwnProperty('view_id')) {
            self._viewId = _saveView.view_id;
            addToCurrentViews(self);
            console.log('[AJAX] view saved, id = ' + _saveView.view_id);
            saveDashboardWithCounter();
        } else if(_saveView.hasOwnProperty('need_replace')) {
            $('<div><p><span class="ui-icon ui-icon-alert" style="float: left; margin: 0 7px 20px 0;"></span>A view with that name already exists. Are you sure you want to replace it?</p></div>').appendTo(document.body).dialog({
                resizable: false,
                height:180,
                modal: true,
                appendTo: "#main-body",
                buttons: {
                    "Replace the view": function() {
                        $( this ).dialog( "close" );
                        self._saveNewView(_saveView.need_replace, true);
                    },
                    Cancel: function() {
                        $( this ).dialog( "close" );
                    }
                },
                open: function() {
                    $(this)
                        .dialog("widget")
                        .draggable("option","containment","#main-body");
                },
                close : function() {
                    $(this).dialog('destroy');
                    $(this).remove();
                }
            });
        } else if(_saveView.hasOwnProperty('error')) {
            alert_jq('failed to save view!\n'+_saveView.error);
        }
    },
    _insertResult: function(_insert) {
        this.appendRows(_insert);
    },

    _deleteResult: function(_delete) {
        this.appendRows(_delete);
        console.log('deleted '+JSON.stringify(_delete));
    },

    _openView: function(viewResult) {
        this._cmd    = viewResult.content;
        this._stmt   = viewResult.statement;
        this._conn   = viewResult.connection;
        this._viewId = viewResult.view_id;
        if(viewResult.hasOwnProperty('column_layout') && viewResult.column_layout.length > 0) {
            this._clmlay = viewResult.column_layout;
        }
        this._setTitleHtml($('<span>').text(viewResult.name).addClass('table-title'));
        this.options.title = viewResult.name;
        updateWindowTitle(this._windowFinderTextLink, this.options.title);
        console.log('>>>>> table '+viewResult.name+' '+viewResult.connection);
        if(viewResult.hasOwnProperty('error')) {
            alert_jq(viewResult.error);
        } else {
            this.setColumns(viewResult.columns);
            if(viewResult.hasOwnProperty('sort_spec') && !$.isEmptyObject(viewResult.sort_spec)) {
                this._setSortSpecFromJson(this, viewResult.sort_spec);
            }
            this.buttonPress(this._startBtn);
        }
        // If this is a view we add it to the current views
        addToCurrentViews(this);
    },

    _renderViews: function(_views) {
        var self = this;
        self._cmd    = _views.content;
        self._stmt   = _views.statement;
        self._conn   = _views.connection;
        self._viewId = _views.view_id;
        if(_views.hasOwnProperty('column_layout') && _views.column_layout.length > 0) {
            self._clmlay = _views.column_layout;
        }
        if(_views.hasOwnProperty('table_layout')  && _views.table_layout.hasOwnProperty('x')) {
            self._tbllay = _views.table_layout;
            // Set the options.
            self.options.width = self._tbllay.width;
            self.options.height = self._tbllay.height;
            self.options.position = {
                my: "left top",
                at: "left+" + self._tbllay.x + " top+" + self._tbllay.y,
                of: "#main-body",
                collision : 'none'
            };

            // Override default dialog options.
            self._dlg.dialog("option", "position", self.options.position);
            self._dlg.dialog("option", "width", self.options.width);
            self._dlg.dialog("option", "height", self.options.height);
        }
        self._setTitleHtml($('<span>').text(_views.name).addClass('table-title'));
        self.options.title = _views.name;
        console.log('>>>>> table '+_views.name+' '+_views.connection);
        if(_views.hasOwnProperty('error')) {
            alert_jq(_views.error);
        } else {
            self.setColumns(_views.columns);
            if(_views.hasOwnProperty('sort_spec') && !$.isEmptyObject(_views.sort_spec)) {
                self._setSortSpecFromJson(self, _views.sort_spec);
            }
            self._gridColumnsReorder();
            self.buttonPress(self._startBtn);
        }
        // If this is a view we add it to the current views
        addToCurrentViews(this);
    },

    // Exported function to be called from sql query editor
    renderTable: function(_table) {
        this._dlg.dialog("moveToTop");
        this._cmd = _table.qstr;
        this._optBinds = _table.qparams;
        this._renderTable(_table);
    },

    _renderTable: function(_table) {
        if(_table.hasOwnProperty('result') && _table.result === 'ok') {
            console.log('[_renderTable] no row query, closing dialog');
            this._dlg.dialog('close');
            return;
        }
        if(_table.hasOwnProperty('error')) {
            this._dlg.dialog('close');
            alert_jq(_table.error);
            return;
        }
        if(!_table.hasOwnProperty('statement')) {
            this._dlg.dialog('close');
            alert_jq('missing statement handle');
            return;
        }
        this._stmt = _table.statement;
        this._conn = _table.connection;
        if(_table.hasOwnProperty('view_id')) {
            this._viewId = _table.view_id;
        }
        if(_table.hasOwnProperty('column_layout') && _table.column_layout.length > 0) {
            this._clmlay = _table.column_layout;
        }

        if(_table.hasOwnProperty('table_layout')) {
            if(_table.table_layout.hasOwnProperty('x')) {
                this._tbllay = _table.table_layout;
            }

            this._initPlanes(_table.table_layout);
        }
        
        if(!_table.hasOwnProperty('columns')) {
            console.log('[_renderTable] missing columns - '+_table);
            alert_jq('missing columns');
            return;
        }

        this.setColumns(_table.columns);
        if(_table.hasOwnProperty('sort_spec') && !$.isEmptyObject(_table.sort_spec)) {
            this._setSortSpecFromJson(this, _table.sort_spec);
        }
        this._gridDataView.beginUpdate();
        this._gridDataView.setItems([]);
        this._gridDataView.endUpdate();
        this._gdata = this._gridDataView.getItems();
        this._gridColumnsReorder();
        
        this.showPlane();
        this.buttonPress(this._startBtn);
        
        console.log('>>>>> table ' + _table.name + ' ' + _table.connection);
    },

    _renderNewTable: function(_table) {
        var tl = _table.table_layout;
        var cl = null;
        var viewId = null;
        var left = 0;
        var top = 0;

        if(_table.hasOwnProperty('error')) {
            alert_jq(_table.error);
            this._openFailedSql(_table.name, _table.content, null, null);
            return;
        } else if(_table.hasOwnProperty('binds')) {
            console.log("the tl cha", tl);
            this._openFailedSql(_table.name, _table.content, _table.binds, _table.view_id, tl);
            return;
        }

        if(!_table.hasOwnProperty('table_layout') || !_table.table_layout.hasOwnProperty('x')) {
            var dlg = this._dlg.dialog('widget');
            var titleBarHeight = dlg.find('.ui-dialog-titlebar').height();
            left = (dlg.position().left + titleBarHeight + 10);
            top = (dlg.position().top + titleBarHeight + 10);
        } else {
            left = _table.table_layout.x;
            top = _table.table_layout.y;
        }
        
        var pos = {
            my: "left top",
            at: "left+" + left + " top+" + top,
            of: "#main-body",
            collision : 'none'
        };
            
        if(_table.hasOwnProperty('view_id')) {
            viewId = _table.view_id;
        }

        if(_table.hasOwnProperty('column_layout') && _table.column_layout.length > 0) {
            cl = _table.column_layout;
        }


        this.removeWheel();
        var baseOptions = {
            autoOpen        : false,
            title           : _table.name,
            position        : pos,

            dderlAdapter    : this._adapter,
            dderlConn       : this._conn,
            dderlStatement  : _table.statement,
            dderlCmd        : _table.content,
            dderlClmlay     : cl,
            dderlTbllay     : tl,
            dderlViewId     : viewId,
            dderlSortSpec   : ((_table.hasOwnProperty('sort_spec') && _table.sort_spec.length > 0
                                ) ?_table.sort_spec : null)
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
        .table(baseOptions)
        .table('setColumns', _table.columns)
        .table('callReorder')
        .table('buttonPress', '>');
    },

    callReorder: function() {
        var self = this;
        self._gridColumnsReorder();
    },

    _renderRows: function(_rows) {
        var self = this;
        if(_rows.hasOwnProperty('op') && _rows.op === "close") {
            self._stmt = null;
        } else if(_rows.hasOwnProperty('rows')) {
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
            if(_rows.loop.length > 0 && !self._divDisable) {
                if (self._grid.getCellEditor()) {
                    self._loop = _rows.loop;
                } else {
                    self.buttonPress(_rows.loop);
                }
            }
        } else if(_rows.hasOwnProperty('error')) {
            alert_jq(_rows.error);
        }
    },

    _noReloadOnSuccess: function(result) {
        // received response clear wait wheel
        this.removeWheel();
        if(result.hasOwnProperty('error')) {
            alert_jq(result.error);
        }
    },

    _reloadOnSuccess: function(result) {
        // received response clear wait wheel
        this.removeWheel();
        if(result.hasOwnProperty('error')) {
            alert_jq(result.error);
        } else {
            this._toolBarReload(this);
        }
    },

    _openTermOrViewEditor: function(cmdOrString) {
        var self =  this;
        if(cmdOrString.isView === true) {
            self._openFailedSql(
                cmdOrString.title,
                cmdOrString.cmd,
                null,
                cmdOrString.view_id,
                cmdOrString.table_layout);
        } else {
            self._openErlangTermEditor(cmdOrString);
        }
    },

    _openFailedSql: function(title, cmd, optBinds, viewId, tbllay) {
        var script = "";

        if(tbllay && tbllay.hasOwnProperty('plane_specs')) {
            if($.isArray(tbllay.plane_specs) &&
               tbllay.hasOwnProperty('plane_to_show') &&
               tbllay.plane_to_show > 0 &&
               tbllay.plane_to_show <= tbllay.plane_specs.length) {
                script = tbllay.plane_specs[tbllay.plane_to_show-1].script;
            }
        }
        $('<div>')
            .appendTo(document.body)
            .sql({autoOpen  : false,
                  title     : title,
                  cmdOwner  : null,
                  history   : [],
                  cmdFlat   : cmd,
                  optBinds  : optBinds,
                  viewId    : viewId,
                  script    : script // TODO: This should be multiple specs...
                 })
            .sql('open');
    },

    _openImageEditor: function(dataImg) { // Data image encoded as base64 string
        var title = "Image editor (read only)";

        var dlgHeight = $("#main-body").height()-50;
        var dlgWidth = $("#main-body").width()-50;
        $('<div><img src="'+dataImg+'"></div>')
            .dialog(
                { // dialog options default override
                    minHeight       : 250,
                    minWidth        : 250,
                    height          : dlgHeight,
                    width           : dlgWidth,
                    resizable       : true,
                    modal           : false,
                    title           : title,
                    clear           : null,
                    appendTo        : "#main-body",
                    position        : {my: "top left", at: "top left", of: "#main-body", collision: 'none'},
                    focus           : function() {},
                    open: function() {
                        $(this)
                            .dialog("widget")
                            .draggable("option","containment","#main-body");
                    },
                    close           : function() {
                        $(this).dialog('destroy');
                        $(this).remove();
                    }
                });
    },

    _openErlangTermEditor: function(data) {
        var self = this;
        var title = "Erlang term editor";
        var content = data;
        var isJson = false;

        // received response clear wait wheel
        self.removeWheel();

        if(data.hasOwnProperty('error')) {
            title = "Text editor";
            data.isFormatted = true;
            content = data;
        } else if(data.isJson === true) {
            isJson = true;
            title = "Json editor";
            content = data.stringToFormat;
        }
        self.disableDialog();
        $('<div>')
            .appendTo(document.body)
            .termEditor(
            {
                autoOpen: false,
                title: title,
                termOwner: self,
                readOnly: false,
                container: $("#main-body"),
                appendTo: "#main-body",
                term: content,
                isJson: isJson
            }).termEditor('open');
    },
    ////////////////////////////

    _createDlg: function() {
        var self = this;

        if(self._tbllay !== null) {
            self.options.width = self._tbllay.width;
            self.options.height = self._tbllay.height;
            self.options.position = {
                my: "left top",
                at: "left+" + self._tbllay.x + " top+" + self._tbllay.y,
                of: "#main-body",
                collision : 'none'
            };
        } else if(!self.options.position || !self.options.position.my) {
            self.options.position = {
                at : "left top",
                my : "left top",
                of : "#main-body",
                collision : 'none'
            };
        }

        // dlg width can't be less than footer width
        self.options.minWidth = self._footerWidth;

        var last = Date.now();
        var deferTimer;
        self._dlg = self.element
            .dialog(self.options)
            .bind("dialogresize", function() {
                self._grid.resizeCanvas();
                self._dlgResized = true;
                if(self._graphSpec && $.isFunction(self._graphSpec.on_resize)) {
                    var planeIdx = self._planeToShow - 1;
                    var divElement = self._graphDivs[planeIdx].node();
                    // We need to execute the script.
                    if(divElement) {
                        let now = Date.now();
                        // Only call the function once every 200 miliseconds.
                        if(now > last + 200) {
                            last = now;
                            try {
                                self._graphSpec.on_resize(divElement.clientWidth, divElement.clientHeight);
                            } catch(e) {
                                alert_js_error(e);
                            }
                        } else {
                            clearTimeout(deferTimer);
                            deferTimer = setTimeout(function() {
                                last = now;
                                try {
                                    self._graphSpec.on_resize(divElement.clientWidth, divElement.clientHeight);
                                } catch(e) {
                                    alert_js_error(e);
                                }
                            }, 200);
                        }
                    }
                }
            })
            .bind("dialogfocus", function() {
                // If the table is disabled do not set the focus.
                if(self._divDisable) {
                    return;
                }
                var cellEditor = self._grid.getCellEditor();
                self._grid.focus();
                if(cellEditor && !cellEditor.isFocused()) {
                    cellEditor.focus();
                }
            })
            .bind("dialogbeforeclose", function() {
                self._grid.resetHeaderScroll();
            })
            .dialogExtend({
                "minimizable" : true,
                "icons" : {
                    "minimize": "ui-icon-minus"
                },
                "minimizeLocation" : "left",
                "load" : function() {
                },
                "minimize" : function() {
                },
                "restore" : function() {
                    self._dlg.dialog("moveToTop");
                }
            });

        self._dlg.dialog("widget").draggable("option","containment","#main-body");
        

        // converting the title text to a link
        self._setTitleHtml($('<span>').text(self.options.title).addClass('table-title'));

        // If this is a view we add it to the current views
        if(self._viewId) {
            addToCurrentViews(self);
        }

        // add this dialog to the window finder.
        self._windowFinderTextLink = addWindowFinder(self, self.options.title);
    },

    _initPlanes: function(planeData) {
        if(planeData && planeData.hasOwnProperty('plane_specs')) {
            this._planeSpecs = planeData.plane_specs;
        }

        if($.isArray(this._planeSpecs) &&
           planeData.hasOwnProperty('plane_to_show') &&
           planeData.plane_to_show > 0 &&
           planeData.plane_to_show <= this._planeSpecs.length) {
            this._planeToShow = planeData.plane_to_show;
        } else {
            this._planeToShow = 0;
        }
    },

    // plane to show 1 based, 0 represents the grid
    showPlane: function(newPlaneToShow) {
        if(newPlaneToShow !== undefined) {
            if(this._graphDivs[this._planeToShow - 1]) {
                $(this._graphDivs[this._planeToShow - 1].node()).hide();
            }
            this._planeToShow = newPlaneToShow;
        }

        this._refreshButtons();
        if(!this._planeToShow) {
            this._tableDiv.show();
            return;
        }
        
        var planeIdx = this._planeToShow - 1;
        this._tableDiv.hide();
        // We need to execute the script.
        if(!this._graphDivs[planeIdx]) {
            var planeFunc = evalD3Script(this._planeSpecs[planeIdx].script);
            if(planeFunc) {
                let d = document.createElement("div");
                d.classList.add("d3-container");
                d.style.bottom = this.options.toolBarHeight + 'px';
                this._dlg.append(d);
                let container = d3.select(d);
                try {
                    this._graphSpec = planeFunc(container, d.clientWidth, d.clientHeight);
                    this._graphDivs[this._planeToShow - 1] = container;
                } catch(e) {
                    container.remove();
                    alert_js_error(e);
                }
            }
        } else {
            $(this._graphDivs[planeIdx].node()).show();
        }
    },

    _refreshButtons: function() {
        var self = this;
        if(!self._planeToShow) {
            self._tbCommit.show();
            self._tbDiscrd.show();
            if(self._tbClearG) {
                self._tbClearG.hide();
            }
        } else {
            // Create the dom element once,
            // then show or hide as required
            if(!self._tbClearG) {
                self._tbClearG =
                    $('<button>')
                    .text('Clear graph')
                    .data('tag', 'clear')
                    .button({icons: {primary: 'fa fa-undo'}, text: false})
                    .css('height', self.options.toolBarHeight + 'px')
                    .addClass('colorIcon')
                    .click(self, self._toolBarClearG)
                    .appendTo(self._footerDiv);

                self._footerDiv.buttonset('refresh');
            }

            self._tbCommit.hide();
            self._tbDiscrd.hide();
            self._tbClearG.show();
        }
    },
      
    // context menus invocation for slickgrid
    _gridContextMenu: function(e, args) {
        e.preventDefault();

        var g           = args.grid;
        g.getData().syncGridSelection(g, true);
        var cell        = g.getCellFromEvent(e);

        //Check if we are in a new row.
        if(!g.getData().getItem(cell.row)) {
            return;
        }
        
        var column      = g.getColumns()[cell.cell];

        //Check if we are in the id column
        if(column.field == "id") {
            return;
        }

        var gSelMdl     = g.getSelectionModel();
        var gSelecteds  = gSelMdl.getSelectedRanges();
        var activeCell  = g.getActiveCell();

        var missing = true;
        for(var i=0; i < gSelecteds.length; ++i) {
            if(gSelecteds[i].contains(cell.row, cell.cell)) {
                missing = false;
                break;
            }
        }

        if(missing) {
            g.setActiveCell(cell.row, cell.cell);
            gSelMdl.setSelectedRanges([new Slick.Range(cell.row, cell.cell, cell.row, cell.cell)]);
        } else if(!activeCell) {
            g.setActiveCell(cell.row, cell.cell);
        }

        this._slkHdrCnxtMnu.dom
            .removeData('cnxt')
            .hide();
        this._slkCellCnxtMnu.dom
            .css("top", e.clientY - 20)
            .css("left", e.clientX - 10)
            .data('cnxt', this)
            .show();

        var maxLeft = $("#main-body").width() - this._slkCellCnxtMnu.dom.width();
        var maxTop = $("#main-body").height() - this._slkCellCnxtMnu.dom.height();
        var newLeft = Math.min(this._slkCellCnxtMnu.dom.position().left, maxLeft);
        var newTop = Math.min(this._slkCellCnxtMnu.dom.position().top, maxTop);
        this._slkCellCnxtMnu.dom.css("top", newTop).css("left", newLeft);
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
        for(let i = 0; i < gSelecteds.length; ++i) {
            if(gSelecteds[i].fullCol) {
                fullCols.push(gSelecteds[i]);
            }
        }

        var found = false;
        for(let i = 0; i < fullCols.length; ++i) {
            if(fullCols[i].contains(0, col)) {
                found = true;
                break;
            }
        }

        if(found) {
            gSelMdl.setSelectedRanges(fullCols);
        } else {
            g.setActiveCell(0, col);
            var lastRow = (g.getDataLength() === 0? 0 : g.getDataLength() -1);
            var newSelection = new Slick.Range(0, col, lastRow, col);
            newSelection.fullCol = true;
            gSelMdl.setSelectedRanges([newSelection]);
        }

        this._slkCellCnxtMnu.dom
            .removeData('cnxt')
            .removeData('column-id')
            .hide();
        this._slkHdrCnxtMnu.dom
            .css("top", e.clientY - 20)
            .css("left", e.clientX - 10)
            .data('cnxt', this)
            .data('column-id', args.column.id)
            .show();
    },

    _gridCellChange: function(e, args) {
        if(e) {
            e.stopPropagation();
        }

        var g           = args.grid;
        var modifiedRow = g.getData().getItems()[args.row];
        var cols        = g.getColumns();
        var updateJson  = {update_data: {connection  : this._conn,
                                         statement   : this._stmt,
                                         rowid       : parseInt(modifiedRow.id),
                                         cellid      : this._origcolumns[cols[args.cell].field],
                                         value       : modifiedRow[cols[args.cell].field]}};
        this._ajax('update_data', updateJson, 'update_data', 'updateData');
        console.log('changed '+JSON.stringify(updateJson));

        // Update all rows from the selected range
        var selRanges = this._grid.getSelectionModel().getSelectedRanges();
        for(var i=0; i < selRanges.length; ++i)
            for(var ri = selRanges[i].fromRow; ri <= selRanges[i].toRow; ++ri) {
                if(this._gdata[ri].op !== 'ins') {
                    this._gdata[ri].op = 'upd';
                }
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
        this._ajax('insert_data', insertJson, 'insert_data', 'insertData');
    },
    _gridPasteCells: function(e, args) {
        e.stopPropagation();
        var range = args.ranges[0];
        var modifiedRows = [];
        var gridData = this._gdata;
        var cols = this._grid.getColumns();
        var toCellSafe = Math.min(range.toCell, cols.length - 1);
        var fromCellSafe = Math.max(range.fromCell, 1);
        var rowsToRemove = [];
        for(let i = range.fromRow; i <= range.toRow; ++i) {
            var modifiedCells = [];
            var gridData_i = gridData[i];
            if (gridData_i !== undefined && gridData_i !== null) {
                for(let j = fromCellSafe; j <= toCellSafe; ++j) {
                    var cellValue = gridData_i[cols[j].field];
                    if(cellValue !== undefined && cellValue !== null) {
                        modifiedCells.push({cellid: this._origcolumns[cols[j].field], value : cellValue});
                    }
                }
                var rowId = parseInt(gridData_i.id);
                if(rowId > 0) {
                    modifiedRows.push({rowid: rowId, cells: modifiedCells});
                } else {
                    rowsToRemove.push(i);
                    modifiedRows.push({cells: modifiedCells});
                }
            }
        }
        var pasteJson = {paste_data: {connection : this._conn,
                                      statement  : this._stmt,
                                      rows       : modifiedRows}};
        this._ajax('paste_data', pasteJson, 'paste_data', 'updateData');

        // Update all rows from the selected range
        for(let i = range.fromRow; i <= range.toRow; ++i) {
            if (gridData[i] !== undefined && gridData[i] !== null) {
                gridData[i].op = 'upd';
            }
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
        var currentSelectedRanges = this._grid.getSelectionModel().getSelectedRanges();
        if(!(currentSelectedRanges.length === 0 ||
             this._singleCellSelected(currentSelectedRanges))) {
            return false;
        }
        if(args.item) {
            this._editedText = args.item[args.column.field];
        }
        return true;
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
        if(!cellChanged) {
            console.log("deleting pending editor");
            delete self._pendingEditorCell;
            if (loop) {
                self.buttonPress(loop);
            }
        }
    },

    _handleKeyDown: function(e) {
        var keyCode = $.ui.keyCode;
        var col;
        var activeCell;
        var oldActiveCell;
        if(e.keyCode === 27 && this._loop) { // Esc
            this._editorEscaped = true;
        }

        // TODO: Review this, maybe it can be simplified.
        if(this._grid.getCellEditor()) {
            if(e.keyCode === keyCode.ENTER) {
                e.stopImmediatePropagation();
                do {
                    oldActiveCell = this._grid.getActiveCell();
                    this._grid.navigateNext();
                    activeCell = this._grid.getActiveCell();
                    if(activeCell && oldActiveCell &&
                       activeCell.row == oldActiveCell.row &&
                       activeCell.cell == oldActiveCell.cell &&
                       this._gdata.length == activeCell.row) {
                        this._pendingEditorCell = activeCell;
                        console.log("saving pending editor!");
                        break;
                    }
                    if(activeCell) {
                        col = activeCell.cell;
                    }
                } while (activeCell && !this._grid.getColumns()[col].editor);
            } else if(!this._grid.getCellEditor().isFocused()) {
                this._grid.getCellEditor().focus();
            }
            // If we are in edit mode already.
            return;
        } else if(e.keyCode === 46 || e.keyCode === 8) { // Del
            e.stopImmediatePropagation();
            // Delete all rows from the selected range
            var selRanges = this._grid.getSelectionModel().getSelectedRanges();
            var rids, modifiedRows, rowsToRemove;
            rids = [];
            modifiedRows = [];
            rowsToRemove = [];
            for(let i = 0; i < selRanges.length; ++i) {
                for(let ri = selRanges[i].fromRow; ri <= selRanges[i].toRow; ++ri) {
                    if(selRanges[i].fromCell === 0) {
                        rids.push(this._gdata[ri].id);
                        if(this._gdata[ri].op !== 'ins') {
                            this._gdata[ri].op = 'del';
                        } else {
                            if(rowsToRemove.indexOf(ri) == -1) {
                                rowsToRemove.push(ri);
                            }
                        }
                    } else {
                        var cols = this._grid.getColumns();
                        var modifiedCells = [];
                        for(var j = selRanges[i].fromCell; j <= selRanges[i].toCell; ++j) {
                            modifiedCells.push({cellid: this._origcolumns[cols[j].field], value : ""});
                            this._gdata[ri][cols[j].field] = "";
                        }
                        var rowId = parseInt(this._gdata[ri].id);
                        if(rowId) {
                            modifiedRows.push({rowid: rowId, cells: modifiedCells});
                            if(this._gdata[ri].op !== 'ins') {
                                this._gdata[ri].op = 'upd';
                            }
                        }
                    }
                }
            }

            rowsToRemove.sort(function(a, b) {
                return b - a;
            });
            for(let i = 0; i < rowsToRemove.length; ++i) {
                this._gdata.splice(rowsToRemove[i], 1);
            }

            if(rids.length !== 0) {
                var deleteJson = {delete_row: {statement : this._stmt,
                                               rowids    : rids}};
                this._ajax('delete_row', deleteJson, 'delete_row', 'deleteData');
            }

            if(modifiedRows.length !== 0) {
                var pasteJson = {paste_data: {connection : this._conn,
                                              statement  : this._stmt,
                                              rows       : modifiedRows}};
                this._ajax('paste_data', pasteJson, 'paste_data', 'updateData');
            }

            this._applyStyle();

            this._grid.updateRowCount();
            this._grid.invalidate();
        } else if(e.keyCode === keyCode.ENTER || e.keyCode === 113) {
            e.stopImmediatePropagation();
            if(!this._singleCellSelected(this._grid.getSelectionModel().getSelectedRanges())) {
                return;
            }
            this._grid.editActiveCell();
            if(this._grid.getCellEditor()) {
                this._grid.getCellEditor().moveCaretToEnd();
            }
        } else if(e.ctrlKey || e.metaKey) {
            if(e.keyCode === 72) {
                e.stopImmediatePropagation();
                dderlState.copyMode = "header";
                console.log("header copy activated");
            } else if(e.keyCode === 74) {
                e.stopImmediatePropagation();
                dderlState.copyMode = "json";
                console.log("json copy activated");
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

            activeCell = this._grid.getActiveCell();
            if (activeCell && !isInEditMode) {
                col = activeCell.cell;

                // would be nice to have xor in javascript to avoid confusing ternary operator.
                if (this._grid.getColumns()[col].editor && !(e.ctrlKey? !e.altKey : e.altKey)) {
                    // If we have an active cell but no range means that we are in a new cell, 
                    // so we open the editor.
                    var currentSelectedRanges = this._grid.getSelectionModel().getSelectedRanges();
                    if(currentSelectedRanges.length === 0 ||
                       this._singleCellSelected(currentSelectedRanges)) {
                        this._grid.editActiveCell();
                    }
                }
            }
        }
    },

    _handleClick: function() {
        var self = this;
        self._dlg.dialog("moveToTop");
    },

    // Recover the focus if the vieport gets a mouse event.
    _handleMouseDown: function() {
        console.log("handle mouse down");
        var self = this;
        // If the tale is disabled do not set the focus.
        if(self._divDisable) {
            return;
        }

        self._dlg.dialog("moveToTop");
        //TODO: Why is this duplicated ?... find a replacement.
        /*if($.browser.msie) {
            //Ie steals the focus to the scrollbar even after preventDefaults.
            //Added the timer to get the focus back.
            setTimeout(function() {
                self._grid.focus();
                var cellEditor = self._grid.getCellEditor();
                if(cellEditor && !cellEditor.isFocused()) {
                    cellEditor.focus();
                }
                console.log("Focus set");
            }, 50);
        } else {
        */
        self._grid.focus();
        var cellEditor = self._grid.getCellEditor();
        if(cellEditor && !cellEditor.isFocused()) {
            cellEditor.focus();
        }
        console.log("Focus set");
        //}
    },

    _handleDragInit: function(e) {
        e.stopImmediatePropagation();
        var self = this;
        self._dlg.dialog("moveToTop");
        self._grid.focus();
        console.log("Focus set");
    },

    _handleMouseEnter: function(e, args) {
        var self = this;
        var g = args.grid;
        var cell = g.getCellFromEvent(e);
        var row = g.getData().getItem(cell.row);

        //Check if we are in a new row.
        if(!row) {
            return;
        }
        var value = row[g.getColumns()[cell.cell].field];
        if(value && typeof(value.lastIndexOf) === "function" && value.lastIndexOf("data:image", 0) === 0) {
            self._removeImgPreview();
            self._addImgPreview(value, e.clientY + 5, e.clientX + 5);
        }
    },

    _handleMouseLeave: function() {
        var self = this;
        self._removeImgPreview();
    },

    _handleSelectionChanged: function(e, args) {
        var self = this;
        var columns = self._grid.getColumns();
        self._grid.updateColumnHeader(columns[0].id, args.rows.length.toString());
    },

    _addImgPreview: function(value, top, left) {
        var self = this;
        self._imagePreview = $('<div><img src="'+value+'"></div>')
            .css("position", "absolute")
            .css("z-index", 99999)
            .css("top", top)
            .css("left", left)
            .appendTo("#main-body")
            .show();
    },

    _removeImgPreview: function() {
        var self = this;
        if(self._imagePreview) {
            self._imagePreview.remove();
            self._imagePreview = null;
        }
    },

    _gridColumnsReorder: function() {
        var self = this;
        var columnsPos = self._getColumnPositions();

        // Workaround to fix the selection display.
        var g           = self._grid;
        var gSelMdl    = g.getSelectionModel();
        var gSelected  = gSelMdl.getSelectedRanges();
        gSelMdl.setSelectedRanges(gSelected);

        var reorderData = {reorder: {statement   : self._stmt,
                                     column_order: columnsPos}};
        self._ajax('reorder', reorderData, 'reorder', 'reorderResult');
    },

    _getColumnPositions: function() {
        var self = this;
        var columns = self._grid.getColumns();
        var columnPos = [];
        //id is the first column, it is not part of the sql
        for(var i = 1; i < columns.length; ++i) {
            columnPos.push(self._origcolumns[columns[i].field]);
        }
        console.log(columnPos);
        return columnPos;
    },

    _gridColumnsResize: function() {
        var self = this;
        self._grid.invalidate();
    },

    _applyStyle: function() {
        var self = this;
        self._grid.removeCellCssStyles('delete');
        self._grid.removeCellCssStyles('update');
        var delStyle = {};
        var updStyle = {};
        var cols = self._grid.getColumns();
        for (let j = 0; j < self._gdata.length; ++j) {
            switch (self._gdata[j].op) {
                case 'del':
                    delStyle[j] = {};
                    for (let i = 0; i < cols.length; ++i) {
                        delStyle[j][cols[i].id] = 'slick-cell-del';
                    }
                    break;
                case 'upd':
                case 'ins':
                    updStyle[j] = {};
                    for (let i = 0; i < cols.length; ++i) {
                        updStyle[j][cols[i].id] = 'slick-cell-upd';
                    }
                    break;
            }
        }
        if(!$.isEmptyObject(delStyle)) self._grid.setCellCssStyles('delete', delStyle);
        if(!$.isEmptyObject(updStyle)) self._grid.setCellCssStyles('update', updStyle);
    },

    close_stmt: function() {
        if(this._stmt && dderlState.session && dderlState.connection) {
            this.buttonPress("close");
        }
    },

    closeGraphs: function () {
        this._closeGraphs();
    },

    _closeGraphs: function() {
        console.log("closing the graphs");
        this._graphSpec = null;
        for(var i = 0; i < this._graphDivs.length; ++i) {
            if(this._graphDivs[i]) {
                this._graphDivs[i].remove();
                delete this._graphDivs[i];
            }
        }
        this._graphDivs.length = 0;
    },
      
    // loading the views table
    loadViews: function(useSystem) {
        if(useSystem){
            this._ajax('system_views', {system_views: { conn_id: dderlState.connectionSelected.connection}}, 'system_views', 'loadViews');
        } else {
            this._ajax('views', {views: { conn_id: dderlState.connectionSelected.connection}}, 'views', 'loadViews');
        }
    },

    // loading the required view
    openView: function(viewResult) {
        this._openView(viewResult);
    },

    // loading rows
    buttonPress: function (button) {
        this._ajax('button', {
            button: {
                connection: dderlState.connection,
                statement: this._stmt,
                binds: (this._optBinds && this._optBinds.hasOwnProperty('pars')) ? this._optBinds.pars : null,
                btn: button
            }
        }, 'button', 'loadRows');
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
        self._divDisable = null;
        self._grid.focus();
    },

    moveAllToTop: function() {
        var self = this;
        self._dlg.dialog("moveToTop");
        if(self._divSqlEditor && self._divSqlEditor.hasClass('ui-dialog-content')) {
            self._divSqlEditor.dialog("moveToTop");
        }
        if(self._fltrDlg && self._fltrDlg.hasClass('ui-dialog-content')) {
            self._fltrDlg.dialog("moveToTop");
        }
        if(self._sortDlg && self._sortDlg.hasClass('ui-dialog-content')) {
            self._sortDlg.dialog("moveToTop");
        }
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
        }
 
        // In jQuery UI 1.9 and above, you use the _super method instead
        if (save) this._super( "_setOption", key, value );
    },

    _setTitleHtml: function(newTitle) {
        var self = this;
        self._dlg.dialog('option', 'title', newTitle[0].outerHTML);
        self._dlg.dialog("widget").find(".table-title").on("contextmenu click", function(e) {
            self._dlgTtlCnxtMnu.dom
                .css("top", e.clientY - 10)
                .css("left", e.clientX)
                .data('cnxt', self)
                .show();
            return false;
        });
    },

    _moveSelection : function(nRowsToMove) {
        var self = this;

        var g           = self._grid;
        var gSelMdl     = g.getSelectionModel();
        var gSelecteds  = gSelMdl.getSelectedRanges();
        var activeCell  = g.getActiveCell();

        for(var i=0; i < gSelecteds.length; ++i) {
            gSelecteds[i].fromRow -= nRowsToMove;
            if(gSelecteds[i].fromRow < 0) {
                gSelecteds[i].fromRow = 0;
            }
            gSelecteds[i].toRow -= nRowsToMove;
            if(gSelecteds[i].toRow < 0) {
                gSelecteds[i].toRow = 0;
            }
        }

        if(activeCell) {
            if(activeCell.row - nRowsToMove < 0) {
                g.resetActiveCell();
            } else if (g.getCellEditor()) {
                self._grid.setActiveCell(activeCell.row - nRowsToMove, activeCell.cell);
                self._grid.editActiveCell();
            } else {
                self._grid.setActiveCell(activeCell.row - nRowsToMove, activeCell.cell);
            }
        }

        gSelMdl.setSelectedRanges(gSelecteds);
    },

    _atBottom : function() {
        return this._grid.getViewport().bottom >= this._gdata.length;
    },

    _scrollNext : function() {
        var currentViewPort = this._grid.getViewport();
        //The viewport reports 2 more rows than the visible rows...
        var numberOfRows = currentViewPort.bottom - currentViewPort.top - 4;
        var rowScrollTarget = Math.min(this._gdata.length, currentViewPort.bottom + numberOfRows);
        this._grid.scrollRowIntoView(rowScrollTarget);
    },

    _atTop : function() {
        return this._grid.getViewport().top <= 0;
    },

    _scrollBack : function() {
        var currentViewPort = this._grid.getViewport();
        var numberOfRows = currentViewPort.bottom - currentViewPort.top - 4;
        var rowScrollTarget = Math.max(0, currentViewPort.top - numberOfRows);
        this._grid.scrollRowIntoView(rowScrollTarget);
    },

    _singleCellSelected: function(selRanges) {
        if(selRanges.length > 2) {
            return false;
        } else if(selRanges.length == 2) {
            return (selRanges[0].fromRow  === selRanges[0].toRow  && // single cell
                    selRanges[0].fromCell === selRanges[0].toCell &&
                    selRanges[1].fromRow  === selRanges[1].toRow  && // single cell
                    selRanges[1].fromCell === selRanges[1].toCell &&
                    selRanges[1].fromCell === selRanges[0].toCell && // same cell
                    selRanges[1].fromRow  === selRanges[0].toRow);
        } else if(selRanges.length == 1) {
            return (selRanges[0].fromRow  === selRanges[0].toRow &&
                    selRanges[0].fromCell === selRanges[0].toCell);
        }
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
        var rows = this._gridDataView.getItems().length + 2;
        return (rows * this.options.slickopts.rowHeight) + this.options.toolBarHeight;
    },

    setColumns: function(_cols) {
        var self = this;
        var dlg = self._dlg.dialog('widget');

        // Column Data
        var columns = _cols;
        var fldWidth = 0;
        self._origcolumns = {};
        columns[0].formatter = Slick.Formatters.IdFormatter;
        columns[0].headerCssClass = "numeric";
        for (let i = 1; i < columns.length; ++i) {
            if(columns[i].type == "numeric") {
                columns[i].cssClass = "numeric";
                columns[i].headerCssClass = "numeric";
            }
            columns[i].formatter = Slick.Formatters.BinStringText;
            fldWidth = self._txtlen.text(_cols[i].name).width()+25;
            if(columns[i].hasOwnProperty('editor')) {
                columns[i].editor = Slick.Editors.ControlChars;
            }
            columns[i].minWidth = 40;
            columns[i].width    = fldWidth;
            self._origcolumns[columns[i].field] = i;
        }

        //If we load new columns we can't keep the hidden columns information...
        if(self.hasOwnProperty('_hiddenColumns')) {
            delete self._hiddenColumns;
        }

        // load the column layout if its was saved
        if(self._clmlay !== null) {
            for(let i = 1; i < columns.length; ++i) {
                for(let j = 0; j < self._clmlay.length; ++j) {
                    if(columns[i].field === self._clmlay[j].name) {
                        columns[i].width = self._clmlay[j].width;
                        break;
                    }
                }
            }
        }
        self._grid.setColumns(columns);

        if(self._tbllay === null && !self._dlgResized) {
            var necessaryWidth = Math.max(self._footerWidth, self._getGridWidth() + 13);
            var availableSpace = $(window).width() - dlg.offset().left - 20;
            self._dlg.dialog("option", "width", Math.min(necessaryWidth, availableSpace));
        }
        self._dlg.dialog('open');
    },

    endPaste : function() { this.removeWheel(); },
    removeWheel : function()
    {
        this._spinCounter--;
        var $dlgTitleObj = $(this._dlg.dialog('option', 'title'));
        if(this._spinCounter <= 0 && this._dlg.hasClass('ui-dialog-content') &&
            $dlgTitleObj.hasClass('table-title-wait')) {
            this._setTitleHtml($dlgTitleObj.removeClass('table-title-wait'));
            this._spinCounter = 0;
        }
    },

    startPaste : function() { this.addWheel(); },
    addWheel : function()
    {
        if(this._spinCounter < 0)
            this._spinCounter = 0;
        this._spinCounter++;
        var $dlgTitleObj = $(this._dlg.dialog('option', 'title'));
        if(this._spinCounter > 0 && this._dlg.hasClass('ui-dialog-content') &&
            !($dlgTitleObj.hasClass('table-title-wait'))) {
            this._setTitleHtml($dlgTitleObj.addClass('table-title-wait'));
        }
    },

    // public function for loading rows
    // used by ajaxCall but can also be used directly
    appendRows: function(_rows)
    {
        //console.time('appendRows');
        //console.profile();
        var self = this;
        var redraw = false;


        // received response clear wait wheel
        self.removeWheel();

        // system actions (beep and others)
        if(_rows.beep) { beep(); }
        self._tbTxtBox.attr('title',_rows.toolTip);
        self._tbTxtBox.val(_rows.cnt+' ');
        var tbClass = (/tb_[^ ]+/g).exec(self._tbTxtBox.attr('class'));
        for (let i = 0; i < tbClass.length; ++i) {
            self._tbTxtBox.removeClass(tbClass[i]);
        }
        self._tbTxtBox.addClass('tb_'+_rows.state);
        if(_rows.message.length > 0) alert_jq(_rows.message);

        if(_rows.op !== "nop") {
            if(!$.isEmptyObject(_rows.disable) || !$.isEmptyObject(_rows.promote)) {
                for(let btn in self._toolbarButtons) {
                    var tbBtnObj = self._toolbarButtons[btn];
                    var btnElm = self[tbBtnObj.dom];
                    if ((!$.isEmptyObject(_rows.disable) && _rows.disable.hasOwnProperty(btn)) ||
                        (dderlState.adapter === "oci" && (btn === ">|..." || btn === "..."))) {
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
                for(let btn in self._toolbarButtons) {
                    self[self._toolbarButtons[btn].dom]
                    .button('enable')
                    .removeClass('ui-state-error')
                    .attr('title', self._toolbarButtons[btn].tip);
                }
            }
        }


        // If we are in a graph we use the data callback.
        if(self._graphSpec && $.isFunction(self._graphSpec.on_data)) {
            if(_rows.op != "nop") {
                try {
                    self._graphSpec.on_data(_rows.rows);
                } catch(e) {
                    alert_js_error(e);
                }
            }

            // TODO: do we need to have data in slickgrid updated too ?
            return;
        }

        var c = self._grid.getColumns();
        var firstChunk = (self._grid.getDataLength() === 0);
        
        if (firstChunk && _rows.hasOwnProperty('max_width_vec') && !$.isEmptyObject(_rows.max_width_vec) && !self._clmlay) {
            var fieldWidth = 0;
            for (let i = 0; i < c.length; ++i) {
                fieldWidth = self._txtlen.text(_rows.max_width_vec[c[i].field]).width();
                fieldWidth = fieldWidth + 0.4 * fieldWidth;
                if(c[i].width < fieldWidth) {
                    c[i].width = fieldWidth;
                    if (c[i].width > self._MAX_ROW_WIDTH) {
                        c[i].width = self._MAX_ROW_WIDTH;
                    }
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
        self._gdata = self._gridDataView.getItems();
        self._gridDataView.beginUpdate();
        switch (_rows.op) {
            case "rpl": // replace
                self._gridDataView.setItems(_rows.rows);
                self._gdata = self._gridDataView.getItems();
                computedFocus = gvp.bottom + _rows.rows.length - 2 * gvpH;
                if(computedFocus < 0) computedFocus = 0;
                if(computedFocus > self._gdata.length - 1) computedFocus = self._gdata.length - 1;
                if(_rows.focus === 0) {
                    computedFocus = self._gdata.length - 1;
                }
                redraw = true;
                needScroll = true;
                break;
            case "app": // append
                for (let i = 0; i < _rows.rows.length; ++i) {
                    self._gridDataView.addItem(_rows.rows[i]);
                }
                var nRowsMoved = self._gdata.length - _rows.keep;
                if(nRowsMoved > 0) {
                    self._moveSelection(nRowsMoved);
                    for(let j = 0; j < nRowsMoved; ++j) {
                        self._gridDataView.deleteItem(self._gdata[0].id);
                    }
                    computedFocus = gvp.bottom - 4 - nRowsMoved;
                } else {
                    computedFocus = gvp.bottom + Math.min(_rows.rows.length, gvpH - 4);
                }
                if(computedFocus > self._gdata.length - 1) computedFocus = self._gdata.length - 1;
                if(computedFocus < 1) computedFocus = 1;
                redraw = true;
                needScroll = true;
                break;
            case "prp": // prepend
                var nRowsDelete = self._gdata.length +  _rows.rows.length - _rows.keep;
                if(nRowsDelete > 0) {
                    var lastIndex = self._gdata.length - 1;
                    for(let j = lastIndex; j > lastIndex - nRowsDelete; --j) {
                        self._gridDataView.deleteItem(self._gdata[j].id);
                    }
                }
                do {
                    self._gridDataView.insertItem(0, _rows.rows.pop());
                } while (_rows.rows.length > 0);

                computedFocus = gvp.top - Math.min(_rows.rows.length, gvpH - 4) + nRowsDelete;
                if(computedFocus < 0) {
                    computedFocus = 0;
                } else if(computedFocus > self._gdata.length - 1) {
                    computedFocus = self._gdata.length - 1;
                }
                redraw = true;
                needScroll = true;
                break;
            case "clr": // delete all rows
                self._gridDataView.setItems([]);
                self._gdata = self._gridDataView.getItems();
                redraw = true;
                break;
            case "ins": // add rows to the end, keep all
                console.log('ins');
                for (let i = 0; i < _rows.rows.length; ++i) {
                    self._gridDataView.addItem(_rows.rows[i]);
                }
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
            if(!self._tbllay) {
                // since columns' width doesn't change after the first block we can skip this
                if (firstChunk) {
                    var dlg = this._dlg.dialog('widget');

                    if (!self._dlgResized) {
                        var gWidth = self._getGridWidth() + 13;
                        var rWindowWidth = $(window).width()-dlg.offset().left-20; // available width for the window
                        
                        // Dialog width adjustment
                        if (self._footerWidth > gWidth) {
                            // table is smaller than the footer
                            self._dlg.dialog("option", "width", self._footerWidth);
                        } else if (gWidth < rWindowWidth) {
                            // table is smaller than the remaining window
                            self._dlg.dialog("option", "width", gWidth);
                        } else {
                            // table is bigger then the remaining window
                            var orig_top = dlg.offset().top;
                            var new_left = dlg.offset().left - gWidth + rWindowWidth;
                            if(new_left > 0) {
                                self._dlg.dialog("option", "position", {
                                    my: "left top",
                                    at: "left+" + new_left + " top+" + orig_top,
                                    of: "#main-body",
                                    collision : 'none'
                                });
                                self._dlg.dialog("option", "width", gWidth);
                            } else {
                                self._dlg.dialog("option", "position", {
                                    my: "left top",
                                    at: "left top+" + orig_top,
                                    of: "#main-body",
                                    collision : 'none'
                                });
                                self._dlg.dialog("option", "width", $(window).width() - 40);
                            }
                        }

                        var oldDlgHeight = self._dlg.dialog("option", "height");
                        var gHeight = self._getGridHeight();
                        var rWindowHeight = $(window).height()-dlg.offset().top-2*self.options.toolBarHeight-40; // available height for the window
                        // Height used for header and footer (total - content).
                        var extraHeight = oldDlgHeight - self._dlg.height();
                        if (self._dlg.height() > gHeight || gHeight < rWindowHeight) {
                            // if content of dialog is already bigger than height required by the table
                            // or if table height is less then remaining window height
                            self._dlg.dialog("option", "height", gHeight + extraHeight);
                        } else {
                            // if table height is still bigger than the remaining window height
                            self._dlg.dialog("option", "height", rWindowHeight + extraHeight);
                        }

                        if (oldDlgHeight != self._dlg.dialog("option", "height")) {
                            self._grid.resizeCanvas();
                        }
                    }
                    // adjusting the column to fill the rest of the window
                    if((self._getGridWidth() + 18) < dlg.width()) {
                        c[c.length - 1].width += (dlg.width()-self._getGridWidth()-18);
                        self._grid.setColumns(c);
                    }
                }
            }

            // 
            // loading of rows is the costliest of the operations
            // compared to computing and adjusting the table width/height
            // (so for now total time of function entry/exit is appromately equal to only row loading)
            //

        }
        // If we are tailing we need to keep the editor
        // TODO: Improve this design to avoid errors in edge cases.
        // Note: it is not posible to call endupdate before checking for
        //       the cell editor, since calling endupdate will destroy it.
        var cellEditor = self._grid.getCellEditor();
        self._gridDataView.endUpdate();
        self._grid.invalidate();
        if(redraw) {
            // update row styles
            self._applyStyle();
        }
        if(cellEditor && _rows.loop == "tail") {
            if(self._grid.getActiveCell().row < self._gdata.length) {
                self._grid.editActiveCell();
            }
        } else if(self._pendingEditorCell && _rows.op == "ins") {
            self._grid.setActiveCell(self._pendingEditorCell.row + 1,
                                     self._pendingEditorCell.cell);
            delete self._pendingEditorCell;
        }
        
        //console.timeEnd('appendRows');
        //console.profileEnd();
    },

    _toggleGrouping: function (data) {
        var self = this;
        var separator = /[#/.\\]/; //TODO: Check if the correct value is not: /[#\/.\\]/
        var columnId = data.columnId;
        var columns = self._grid.getColumns();
        var columnIndex = self._grid.getColumnIndex(columnId);

        if ('sel' == columnId) {
            alert_jq('Error: No appropriate column for the menu item "Toggle Grouping" selected!');
            return;
        }

        console.log('toggle grouping ' + JSON.stringify(data));
        if (self._gridDataView.getGrouping().length === 0) {
            columns[columnIndex].oldformatter = columns[columnIndex].formatter;
            columns[columnIndex].formatter = function (row, cell, value) {
                if (value !== null && value !== undefined) {
                    return value.split(separator).pop();
                }
                return "";                
            };
            groupByColumn(self._gridDataView, columnId, separator);
        }
        else {
            columns[columnIndex].formatter = columns[columnIndex].oldformatter;
            self._gridDataView.setGrouping([]);
        }
    }
  });
}( jQuery ) );

function groupByColumn(dataView, col, seperator) {
    var getters = [];
    var ldata = dataView.getItems();
    var level = 0;

    for (let r = 0; r < ldata.length; ++r) {
        let curlength = ldata[r][col].split(seperator).length - 1;
        if (level < curlength) {
            level = curlength;
        }
    }

    for (let r = 0; r < ldata.length; ++r) {
        let curlength = ldata[r][col].split(seperator).length - 1;
        let parts = ldata[r][col].split(seperator);
        for (let li = 0; li < level - curlength; ++li) {
            parts.splice(-1, 0, "");
        }
        ldata[r][col+'_grp'] = parts.join('/');
    }

    function createGetterFun(idx) {
        return function (row) {
            return row[col + '_grp'].split('/')[idx];
        };
    }

    function formatter(g) {
        return "" + g.value + "<span class='slick-group-count'>(" + g.count + ")</span>";
    }

    for(var i = 0; i < level; ++i) {
        getters.push({
            getter: createGetterFun(i),
            formatter: formatter,
            collapsed: true
        });
    }
    dataView.setGrouping(getters);
}

function evalD3Script(script) {
    /* jshint evil:true */
    // Here we can inject libraries we would like to make available to d3 scripts.
    var f = new Function('script', 'd3', "return eval('(' + script + ')')");
    var result = null;
    try {
        result = f(script, d3);
    } catch(e) {
        alert_js_error(e);
    }
    return result;
}

// TODO: Move this with the other alerts
function alert_js_error(e) {
    var message = e.message;
    if(e.stack) {
        message += "\n" + e.stack;
    }
    alert_jq(message);
}
