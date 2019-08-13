import jQuery from 'jquery';
import {alert_jq} from '../dialogs/dialogs';
import {addWindowFinder, ajaxCall, beep, dderlState} from './dderl';
import {smartDialogPosition} from './dderl';
import {controlgroup_options} from '../jquery-ui-helper/helper.js';
import * as tableSelection from './table-selection';

(function( $ ) {
    $.widget("dderl.statsTable", $.ui.dialog, {

        _dlg            : null,
        _footerWidth    : 0,
        _tableDiv       : null,
        _footerDiv      : null,
        _grid           : null,
        _gdata          : null,
        _txtlen         : null,
        _cmd            : "",
        _colIds         : [],
        _rowIds         : [],
        _stmt           : "",
        _columns        : null,
        _parent         : null,
        _type           : 'statistics',
        _fnt            : null,
        _fntSz          : null,
        _MAX_ROW_WIDTH  : 1000,

        _handlers : {queryResult        : function(e, _result) { e.data._createDistinctCount(_result); },
                     updateData         : function(e, _result) { e.data._updatePlot(_result); },
                     statsResult        : function(e, _result) { e.data._reload(_result); },
                     activateSender     : function(e, _result) { e.data._activateSenderResult(_result); },
                     statsLoadResult    : function(e, _result) { e.data.open(_result); }
                    },

        _toolbarButtons : {'restart'  : {tip: 'Reload', typ : 'btn', icn : 'refresh', clk : '_toolBarReload',   dom: '_tbReload' },
                           'textBox'  : {tip: '',       typ : 'txt',                  clk : '_toolBarTxtBox',   dom: '_tbTxtBox' }},

                           // dialog context menus
        _statsDlgTtlCnxtMnu  : {'Send Data' : '_activateSender'},

        // slick context menus
        _statsSlkHdrCnxtMnu  : {'Hide'       : '_hide',
                                'UnHide'     : '_unhide',
                                'Sort...'    : '_sort',
                                'Sort ASC'   : '_sortAsc',
                                'Sort DESC'  : '_sortDesc',
                                'Sort Clear' : '_sortClear'},

        // These options will be used as defaults
        options: {
            // dialog options default override
            autoOpen        : false,
            height          : 50,
            width           : 100,
            minHeight       : 50,
            minWidth        : 100,
            resizable       : true,
            modal           : false,
            title           : "",
            toolTip         : "",
            appendTo        : "#main-body",
            canMinimize     : true,
            canMaximize     : true,
            closeOnEscape   : false,
            clear           : null,
            toolBarHeight   : 20,
            initialQuery    : "",
            columnIds       : [1],
            columnRows      : [1],
            columns         : [],
            dderlStatement  : null,
            parent          : null,
            open            : function() {
                var self = this;
                var titleNode = $(self).parent().children(".ui-dialog-titlebar");
                console.log("the title node", titleNode);
                titleNode.click(function() {
                    $(self).statsTable('setGridFocus');
                });
            },
            close           : function() {
                $(this).dialog('destroy');
                $(this).remove();
            },

            // slickgrid options default override
            slickopts       : { enableColumnReorder: true,
                                enableCellNavigation: true,
                                asyncEditorLoading: false,
                                autoEdit: false,
                                rowHeight: 20,
                                editorLock: null
                              },
        },

        _create : function() {
            var self = this;

            self._fnt = $(document.body).css('font-family');
            self._fntSz = $(document.body).css('font-size');

            if(self.options.title !== self._title) {self._title = self.options.title;}
            if(self.options.dderlStatement  !== self._stmt) {self._stmt = self.options.dderlStatement;}
            if(self.options.initialQuery != self._cmd) {self._cmd = self.options.initialQuery;}
            if(self.options.columnIds != self._colIds) {self._colIds = self.options.columnIds;}
            if(self.options.rowIds != self._rowIds) {self._rowIds = self.options.rowIds;}
            if(self.options.parent != self._parent) {self._parent = self.options.parent;}

            self._plotDiv = $('<div>').appendTo(self.element);

            // editor lock private to this table.
            if(!self.options.slickopts.editorLock) {
                self.options.slickopts.editorLock = new Slick.EditorLock();
            }

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
                    .css('font-size', self._fntSz);
            }

            // toolbar container
            self._footerDiv = $('<div>').appendTo(self.element);
            self._createDlgFooter();

            // create the dialog
            self.options.minWidth = self._footerWidth;
            self._dlg = self.element.dialog(self.options)
                .bind("dialogresize", function() {
                    self._grid.resizeCanvas();
                    self._dlgResized = true;
                })
                .bind("dialogfocus", function() {
                    self._grid.focus();
                    tableSelection.select(self);
                })
                .bind("dialogbeforeclose", function() {
                    self._grid.resetHeaderScroll();
                });

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

            // convert title into html
            self._setTitleHtml($('<span>').text(self.options.title).addClass('table-title'));

            self._createSlickGrid();
            self._cnxtMenu('_statsSlkHdrCnxtMnu');
            self._cnxtMenu('_statsDlgTtlCnxtMnu');

            // setting up the event handlers last to aid debuggin
            for(var fun in self._handlers) {
                self.element.on(fun, null, self, self._handlers[fun]);
            }
        },

        setGridFocus: function() {
            var self = this;
            console.log("Focus set via titlebar click");
            self._grid.focus();
            tableSelection.select(self);
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

            this._statsSlkHdrCnxtMnu.dom
                .css("top", e.clientY - 20)
                .css("left", e.clientX - 10)
                .data('cnxt', this)
                .data('column-id', args.column.id)
                .show();
        },

        _createDlgFooter : function() {
            var self = this;

            // footer for the toolbar
            self._footerDiv
                .css('height', '20px')
                .css('position', 'absolute')
                .css('left', '0')
                .css('right', '0')
                .css('bottom', '0')
                .css('overflow', 'hidden');

            var toolElmFn = function(e) {
                var self = e.data;
                var btn = $(this).data('tag');
                var fName = self._toolbarButtons[btn].clk;
                var f = self[fName];
                if($.isFunction(f)) {
                    f(self);
                } else {
                    throw ('[' + self.options.title + '] toolbar ' + btn + ' has unimplimented cb ' + fName);
                }
            };

            var identKeyPressHandler = function(evt) {
                if(evt.which == 13) {
                    var rownum = parseInt($(this).val());
                    if(!isNaN(rownum)) {
                        self._toolBarTxtBoxVal = rownum;
                        evt.data = self;
                        toolElmFn.call(this, evt);
                    }
                }
                return true;
            };

            // footer items
            for(let btn in self._toolbarButtons) {
                var btnTxt = self._toolbarButtons[btn].tip;
                var elm = self._toolbarButtons[btn];
                var inph = self.options.toolBarHeight;
                //if($.browser.msie) inph -= 2;

                if(elm.typ === 'btn')
                    self[elm.dom] =
                    $('<button>')
                    .text(btnTxt)
                    .data('tag', btn)
                    .button({icon: 'fa fa-' + elm.icn, showLabel: false})
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
                    .keypress(identKeyPressHandler)
                    .appendTo(self._footerDiv);
            }
            self._footerDiv
                .controlgroup(controlgroup_options())
                .css('height', (self.options.toolBarHeight)+'px');

            // footer total width
            var childs = self._footerDiv.children();
            var totWidth = 0;
            for(var i = 0; i < childs.length; ++i) {
                totWidth += Math.max(childs[i].scrollWidth, childs[i].offsetWidth, childs[i].clientWidth);
            }

            // 10 pixels for resize handler
            self._footerWidth = totWidth + 10;
        },

        _init: function() {
            var self = this;

            // default dialog open behavior
            if (self.options.autoOpen) {
                self._dlg.dialog("open");
            }
        },

        // create the context menu and add them to document.body
        // only if they do not exist
        // TODO: Create a context menu once per table instead of the global
        //       to allow dynamic menu options depending on the column content.
        _cnxtMenu: function(_menu) {
            function leaveMenuHandler(e) {
                var self = $('#' + _menu).data('cnxt');
                e.preventDefault();
                $(this).hide();
                self._grid.focus();
                tableSelection.select(self);
            }

            function clickMenuHandler() {
                var self = $('#' + _menu).data('cnxt');
                if(self) {
                    var columnId = null;
                    console.log('self title _cnxtMenu ' + self.options.title);
                    if(_menu === '_statsSlkHdrCnxtMnu') {
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
                    .mouseleave(leaveMenuHandler)
                    .appendTo(document.body);
                for(var m in this[_menu]) {
                    if($.type(this[_menu][m]) === "string") {
                        $('<li>')
                            .attr("action", m)
                            .click(clickMenuHandler)
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
            if($.isFunction(fun)) {
                var data = this._grid.getSelectionModel().getSelectedRanges();
                fun(data);
            } else {
                throw('unimplimented fun '+funName+' for \''+_action+ '\' in '+_menu);
            }
        },

        // columns hide/unhide
        _hide: function(_ranges) {
            var self = this;
            var columns = self._grid.getColumns();
            var toHide = {};
            for(let i = 0; i < _ranges.length; ++i) {
                //Validate that id column is not on the selection.
                if(_ranges[i].fromCell !== 0 && _ranges[i].toCell !== 0) {
                    for(var j = _ranges[i].fromCell; j <= _ranges[i].toCell; ++j) {
                        toHide[j] = true;
                    }
                }
            }
            var toHideArray = [];
            for(let j in toHide) {
                toHideArray[toHideArray.length] = parseInt(j);
            }
            toHideArray.sort(function(a,b) {return (a < b ? 1 : (a === b ? 0 : -1));});

            if(!self.hasOwnProperty('_hiddenColumns')) {
                self._hiddenColumns = [];
            }

            for(let j = 0; j < toHideArray.length; ++j) {
                self._hiddenColumns.push(
                    {
                        idxCol: toHideArray[j],
                        colContent: columns[toHideArray[j]]
                    }
                );
                columns.splice(toHideArray[j], 1);
            }
            self._grid.setColumns(columns);
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
        },

        // sorts (This maybe can be moved to a plugin but somehow differs
        // as it is local sort instead of fsm sorted  )
        _sort: function(_ranges) {
            var self = this;
            if(!self._sorts) { self._sorts = {}; }
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
            if(!self._sorts) { self._sorts = {}; }
            var cols = self._grid.getColumns();
            for (var i=0; i<_ranges.length; ++i) {
                var fromCell = Math.max(_ranges[i].fromCell, 1);
                for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                    self._sorts[cols[c].field] = {name : cols[c].name, asc : true};
                }
            }
            self._sortApply();
        },

        _sortDesc: function(_ranges) {
            var self = this;
            if(!self._sorts) { self._sorts = {}; }
            var cols = this._grid.getColumns();
            for (var i = 0; i < _ranges.length; ++i) {
                var fromCell = Math.max(_ranges[i].fromCell, 1);
                for(var c = fromCell; c <= _ranges[i].toCell; ++c) {
                    self._sorts[cols[c].field] = {name : cols[c].name, asc : false};
                }
            }
            self._sortApply();
        },

        _sortApply: function() {
            var self = this;

            var sortCols = Object.keys(self._sorts);
            if(sortCols.length === 1) {
                var colId = sortCols[0];
                self._sortSingleCol(colId, self._sorts[colId].asc);
            } else {
                self._showSortGui();
            }
        },

        _sortSingleCol: function(colId, asc) {
            var self = this;
            var dir = 1;
            if(!asc) { dir = -1; }

            self._gdata.sort(function(dataRow1, dataRow2) {
                var result = dataRow1[colId] - dataRow2[colId];
                if(Number.isNaN(result)) {
                    return dir * dataRow1[colId].localeCompare(dataRow2[colId], undefined, {sensitivity: 'base'});
                } else {
                    return dir * Math.sign(result);
                }
            });

            self._grid.invalidate();
            self._grid.render();
        },

        _sortClear: function(_ranges) {
            // Sort by id as it ASC as it is the default given by the server.
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
                self._gdata.sort(function(dataRow1, dataRow2) {
                    if(dataRow1.id === dataRow2.id) { return 0; }
                    if(dataRow1.id > dataRow2.id) { return 1; }
                    else { return -1; }
                });
                self._grid.invalidate();
                self._grid.render();
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
                        sgrid.invalidateAllRows();
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
            };

            self._sortDlg.dialog({
                width : 336,
                modal : false,
                title : 'Sorts',
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
                            saveChange();
                            self._sortMultipleCols(sgrid.getData());
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

        _sortMultipleCols: function(sd) {
            var self = this;

            self._gdata.sort(function(dataRow1, dataRow2) {

                // No sort selected then sort using id asc.
                if (sd.length === 0) {
                    if(dataRow1.id === dataRow2.id) { return 0; }
                    if(dataRow1.id > dataRow2.id) { return 1; }
                    else { return -1; }
                }

                for (let i = 0; i < sd.length; ++i) {
                    let colId = sd[i].id;
                    let cmpResult = dataRow1[colId].localeCompare(dataRow2[colId], undefined, {numeric: true, sensitivity: 'base'});
                    if(cmpResult !== 0) {
                        let dir = (sd[i].sort === 'ASC')? 1 : -1;
                        return cmpResult * dir;
                    }
                }
                return 0;
            });

            self._grid.invalidate();
            self._grid.render();
        },

        load: function(type)
        {
            var self = this;
            var reqObj = {};

            self._type = type;

            reqObj[type] = {
                connection  : dderlState.connection,
                statement   : self._stmt,
                column_ids  : self._colIds,
                row_ids     : self._rowIds
            };

            self._ajax(type, reqObj, type, 'statsLoadResult');
        },

        open: function(stats) {
            var self = this;
            if (stats.hasOwnProperty('error')) {
                alert_jq(stats.error);
                return;
            }

            if(self._parent) {
                self._dlg.dialog("option", "position", {at : 'left top+60', my : 'left top', of : self._parent.dialog("widget"), collision : 'none'});
            } else {
                self._dlg.dialog("option", "position", {at : 'left top+60', my : 'left top', of : $("#main-body"), collision : 'none'});
            }
            self._dlg.dialog("widget").draggable("option", "containment", "#main-body");
            self.setColumns(stats.cols, stats.type != "stats");
            self.appendRows(stats.gres);
            addWindowFinder(self, self.options.title);
        },

        moveAllToTop: function() {
            var self = this;
            self._dlg.dialog("moveToTop");
        },

        _reload: function(stats) {
            var self = this;
            if(stats.hasOwnProperty('gres')) {
                self.appendRows(stats.gres);
            }
        },

        _ajax: function(url, data, resp, callback) {
            ajaxCall(this, url, data, resp, callback);
        },

        _getSenderData: function() {
            let self = this;
            console.log("raw:", self._gdata);
            let cols = self._grid.getColumns();
            console.log("The cols", cols);
            let result = [];
            self._gdata.forEach((row) => {
                let dataRow = [];
                // Starts as 1 as id colums is the index 0.
                for(let i = 1; i < cols.length; ++i) {
                    dataRow.push(row[cols[i].field]);
                }
                result.push(dataRow);
            });
            console.log("result data", result);
            return result;
        },

        _activateSender: function() {
            var self = this;
            self._ajax('activate_sender', {
                activate_sender: {
                    connection: dderlState.connection,
                    statement: self._stmt,
                    data: self._getSenderData(),
                    sender_type: 'stats'
                }
            }, 'activate_sender', 'activateSender');
        },

        _activateSenderResult: function(activationResult) {
            if(activationResult.hasOwnProperty('error')) {
                alert_jq(activationResult.error);
            } else {
                let dlg = alert_jq("Sender activated, it will wait for up to 100 seconds for a receiver");
                setTimeout(() => {
                    dlg.dialog("close");
                }, 5000);
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

            self._grid.onHeaderContextMenu.subscribe($.proxy(self._gridHeaderContextMenu, self));
            self._grid.onClick.subscribe($.proxy(self._handleClick, self));
            self._grid.onKeyDown.subscribe($.proxy(self._handleKeyDown, self));
            self._grid.onMouseDown.subscribe($.proxy(self._handleMouseDown, self));
            self._grid.onDragInit.subscribe($.proxy(self._handleDragInit, self));
            self._grid.onSort.subscribe($.proxy(self._handleSort, self));
            self._grid.onSelectedRowsChanged.subscribe($.proxy(self._handleSelectionChanged, self));
            self._gdata = self._grid.getData();
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

        _getToolTip: function(columnName)
        {
            switch (columnName) {
            case "avg":
                return "Shows the arithmetic mean of the rows with numeric values. If no \n" +
                       "numerical values are available, then 'undefined' is displayed as a result.";
            case "column":
                return "Shows the header of the selected column.";
            case "count":
                return "Shows the number of selected rows and how many of them are relevant for the statistical ratios.";
            case "hash":
                return "Shows the portable hash of a concatenation of all selected values.\n" +
                       "Portable means regardless of machine architecture and ERTS version.";
            case "max":
                return "Shows the largest value of the rows. If no values are available,\n" +
                       "then 'undefined' is displayed as a result.";
            case "median":
                return "Shows the median of the rows with numeric values. The median is the middle value or \n" +
                       "an interpolated value that would be the middle value once the values are sorted.\n" +
                       "If no numerical values are available, then 'undefined' is displayed as a result.";
            case "min":
                return "Shows the smallest value of the rows. If no values are available,\n" +
                       "then 'undefined' is displayed as a result.";
            case "std_dev":
                return "Shows the standard deviation of the rows with numeric values.\n" +
                       "The standard deviation is calculated as the square root of the variance.\n" +
                       "If no numerical values are available, then 'undefined' is displayed as a result.";
            case "sum":
                return "Shows the sum of the rows with numeric values. If no numerical \n" +
                       "values are available, then 'undefined' is displayed as a result.";
            case "variance":
                return "Shows the variance of the rows with numeric values. If only one row \n" +
                       "with a numeric value has been selected, then the variance is zero.\n" +
                       "If no numerical values are available, then 'undefined' is displayed as a result.";
            default:
                return "";
            }
        },

        setColumns: function(columns, hasPercent) {
            var self = this;

            // Column Data
            self._origcolumns = {};
            columns[0].formatter = Slick.Formatters.IdFormatter;
            columns[0].headerCssClass = "numeric";
            columns[0].name = "0";
            columns[0].cannotTriggerInsert = true;
            columns[0].resizable = false;
            columns[0].sortable = false;
            columns[0].selectable = false;
            for (var i = 1; i < columns.length; ++i) {
                columns[i].toolTip = self._getToolTip(columns[i].name);
                columns[i].resizable = true;
                columns[i].sortable = false;
                columns[i].selectable = true;
                if(columns[i].type == "numeric") {
                    columns[i].cssClass = "numeric";
                    columns[i].headerCssClass = "numeric";
                }
                if(i == columns.length - 1 && hasPercent) {
                    columns[i].formatter = Slick.Formatters.Percent;
                } else {
                    columns[i].formatter = Slick.Formatters.BinStringText;
                }
                if(columns[i].hasOwnProperty('editor')) {
                    columns[i].editor = Slick.Editors.ControlChars;
                }
                columns[i].minWidth = 50;
                self._origcolumns[columns[i].field] = i;
            }

            self._grid.setColumns(columns);
            self._columns = self._grid.getColumns();
            self._dlg.dialog('open');
        },

        // public function for loading rows
        // used by ajaxCall but can also be used directly
        appendRows: function(_rows) {
            //console.time('appendRows');
            //console.profile('appendRows');

            var self = this;
            var redraw = false;
            var c = self._grid.getColumns();
            var firstChunk = (self._gdata.length === 0);

            if (_rows.hasOwnProperty('toolTip')) self._tbTxtBox.attr('title',_rows.toolTip);
            if (_rows.hasOwnProperty('cnt')) self._tbTxtBox.val(_rows.cnt+' ');
            if (_rows.hasOwnProperty('state')) self._tbTxtBox.addClass('tb_'+_rows.state);

            if (firstChunk && _rows.hasOwnProperty('max_width_vec') && !$.isEmptyObject(_rows.max_width_vec)) {
                var fieldWidth = 0;
                for(let i=0;i<c.length; ++i) {
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
                    self._gdata.push(_rows.rows[i]);
                }
                var nRowsMoved = self._gdata.length - _rows.keep;
                if(nRowsMoved > 0) {
                    self._moveSelection(nRowsMoved);
                    self._gdata.splice(0, nRowsMoved);
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
                do {
                    self._gdata.splice(0, 0, _rows.rows.pop());
                } while (_rows.rows.length > 0);
                var nRowsUp = self._gdata.length - _rows.keep;
                self._gdata.splice(_rows.keep, nRowsUp);
                computedFocus = gvp.top - Math.min(_rows.rows.length, gvpH - 4) + nRowsUp;
                if(computedFocus < 0) {
                    computedFocus = 0;
                } else if(computedFocus > self._gdata.length - 1) {
                    computedFocus = self._gdata.length - 1;
                }
                redraw = true;
                needScroll = true;
                break;
            case "clr": // delete all rows
                self._gdata.splice(0, self._gdata.length);
                redraw = true;
                break;
            case "ins": // add rows to the end, keep all
                console.log('ins');
                for(let i=0; i < _rows.rows.length; ++i) {
                    self._gdata.push(_rows.rows[i]);
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

                // since columns' width doesn't change after the first block we can skip this
                if (firstChunk) {
                    var dlg = this._dlg.dialog('widget');

                    if (!self._dlgResized) {
                        var gWidth = self._getGridWidth() + 15;
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

                        var oldDlgHeight = dlg.height();
                        var gHeight = self._getGridHeight();

                        // available height for the window
                        var rWindowHeight = $(window).height()-dlg.offset().top-2*self.options.toolBarHeight-20;
                        if (dlg.height() > gHeight || gHeight < rWindowHeight) {
                            // if dialog is already bigger than height required by the table or
                            // if table height is less then remaining window height
                            self._dlg.height(gHeight);
                        } else {
                            // if table height is still bigger than the remaining window height
                            self._dlg.height(rWindowHeight);
                        }

                        if (oldDlgHeight != dlg.height()) {
                            self._grid.resizeCanvas();
                        }
                    }
                    // adjusting the last column to fill the rest of the window
                    if((self._getGridWidth() + 18) < dlg.width()) {
                        c[c.length - 1].width += (dlg.width()-self._getGridWidth()-18);
                        self._grid.setColumns(c);
                    }
                }
                self._grid.invalidate();
                self._grid.scrollRowIntoView(1);
            }

            //console.timeEnd('appendRows');
            //console.profileEnd('appendRows');
        },

        _createDistinctCount: function(_data) {
            if(_data.hasOwnProperty('result') && _data.result === 'ok') {
                this._dlg.dialog('close');
            } else if(_data.hasOwnProperty('error')) {
                this._dlg.dialog('close');
                alert_jq(_data.error);
            } else if(!_data.hasOwnProperty('statement')) {
                console.error('[_createPlot] missing statement handle - '+_data);
                this._dlg.dialog('close');
                alert_jq('missing statement handle');
            } else if(!_data.hasOwnProperty('columns')) {
                console.log('[_createPlot] missing columns - '+_data);
                alert_jq('missing columns');
            } else {
                this.setColumns(_data.columns);
                if(_data.hasOwnProperty('sort_spec') && !$.isEmptyObject(_data.sort_spec)) {
                    this._setSortSpecFromJson(this, _data.sort_spec);
                }

                this._grid.setData([]);
                this._gdata = this._grid.getData();
                this.buttonPress(this._startBtn);

                this._columns = [];
                for (var i = 1; i < _data.columns.length; ++i) {
                    this._columns[i - 1] = _data.columns[i].field;
                }

                this._stmt = _data.statement;
                this.buttonPress(">");
            }
        },

        _updatePlot: function(data) {
            var self = this;
            if(data.hasOwnProperty('rows')) {
                var rows = data.rows;
                //TODO: Remove duplicated code!
                //TODO: make it generic, not only for monitor table...

                switch (data.op) {
                case "rpl": // replace
                    self._series = [];
                    self._fillSeries(rows);
                    break;
                case "app": // append
                    self._fillSeries(rows);
                    for(let i = 0; i < self._series.length; ++i) {
                        self._series[i].splice(0, self._series[i].length - data.keep);
                    }
                    break;
                case "prp": // prepend
                    while(rows.length > 0) {
                        var row = rows.pop();
                        for(let j = 1; j < self._columns.length; ++j) {
                            // The first column is the date.
                            var time = self._parseDate(row[self._columns[0]]);
                            var value = row[self._columns[j]];
                            self._series[j-1].splice(0, 0, [time, value]);
                        }
                    }
                    for(let i = 0; i < self._series.length; ++i) {
                        self._series[i].splice(data.keep, self._series[i].length - data.keep);
                    }
                    break;
                case "clr": // delete all rows
                    self._series = [];
                    break;
                case "ins": // add rows to the end, keep all
                    self._fillSeries(rows);
                    break;
                case "nop": // no operation
                    console.log('nop');
                    break;
                default:
                    console.log("unknown operation "+data.op);
                    break;
                }

                // if new cmd is not in the list
                /*if(data.sql.length > 0) {
                    self._reorderCalled = false;
                    self._cmd = data.sql;
                    self._addToEditorHistory(data.sql);
                } else if(rowsCount > 0 && !self._reorderCalled && data.loop.length === 0) {
                    self._reorderCalled = true;
                    self._gridColumnsReorder();
                }*/

                if(data.beep) beep();
                self._tbTxtBox.attr('title',data.toolTip);
                self._tbTxtBox.val(data.cnt+' ');
                var tbClass = (/tb_[^ ]+/g).exec(self._tbTxtBox.attr('class'));
                for(let i = 0; i < tbClass.length; ++i) {
                    self._tbTxtBox.removeClass(tbClass[i]);
                }
                self._tbTxtBox.addClass('tb_' + data.state);
                if(data.message.length > 0) {
                    alert_jq(data.message);
                }

                $.plot(self._divPlaceHolder, self._series, {
                    xaxis: {
                        mode: "time",
                        timezone: "browser",
                        ticks: 4
                    }
                });

                // command back request (TODO: always 100 ms, improve self!).
                if(data.loop.length > 0) {
                    setTimeout(function() {self.buttonPress(data.loop);}, 100);
                }
            } else if(data.hasOwnProperty('error')) {
                alert_jq(data.error);
            }
        },

        // loading rows
        buttonPress: function(button) {
            ajaxCall(this, 'button', {
                button: {
                    connection: dderlState.connection,
                    statement: this._stmt,
                    btn: button
                }
            }, 'button', 'updateData');
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
            // TODO: test a workaround for this...
            /*
            if($.browser.msie) {
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
            tableSelection.select(self);
        },

        _handleDragInit: function(e) {
            e.stopImmediatePropagation();
            var self = this;
            self._dlg.dialog("moveToTop");
            self._grid.focus();
            console.log("Focus set");
            tableSelection.select(self);
        },

        _handleKeyDown: function(e) {
            var keyCode = $.ui.keyCode;

            // TODO: Review this, maybe it can be simplified.
            if(e.ctrlKey || e.metaKey) {
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
            }
        },

        _handleSelectionChanged: function(e, args) {
            var self = this;
            var columns = self._grid.getColumns();
            self._grid.updateColumnHeader(columns[0].id, args.rows.length.toString());
        },

        _handleSort: function(e, args) {
            var self = this;
            var field = args.sortCol.field;

            self._gdata.sort(function(a, b){
                var result = a[field] - b[field];
                if(isNaN(result)) {
                    result = (a[field] > b[field]) ? 1 : ((a[field] < b[field]) ? -1 : 0);
                }
                return args.sortAsc ? result : -result;
            });

            self._grid.invalidate();
            self._grid.scrollRowIntoView(1);
        },

        _setTitleHtml: function(newTitle) {
            var self = this;
            self._dlg.dialog('option', 'title', newTitle[0].outerHTML);
            self._dlg.dialog("widget").find(".table-title").on("contextmenu click", function(e) {
                if(self._dlgMinimized) {
                    self._dlg.dialogExtend("restore");
                } else {
                    self._statsDlgTtlCnxtMnu.dom
                        .css("top", e.clientY - 10)
                        .css("left", e.clientX)
                        .data('cnxt', self)
                        .show();
                }
                return false;
            });
        },

        /*
         * Toolbar callbak functions
         */
        // NOTE: self is 'this' and 'this' is dom ;)
        _toolBarReload: function(self) {
            var cmd = self._type;

            var reqObj = {};
            reqObj[cmd] = {
                connection  : dderlState.connection,
                statement   : self._stmt,
                column_ids  : self._colIds,
                row_ids     : self._rowIds
            };

            self._ajax(cmd, reqObj, cmd, 'statsResult');
        },

        _toolBarTxtBox: function(self) {
            if(self.hasOwnProperty('_toolBarTxtBoxVal')) {
                self.buttonPress(self._toolBarTxtBoxVal);
            }
        },

        hideSelection: function() {
            var self = this;
            console.log("Hiding selection for", self.options.title);
            self._tableDiv.addClass(tableSelection.hiddenSelectionClass);
        },
    
        enableSelection: function() {
            var self = this;
            console.log("Enabling selection for", self.options.title);
            self._tableDiv.removeClass(tableSelection.hiddenSelectionClass);
        }
    });
}( jQuery ) );
