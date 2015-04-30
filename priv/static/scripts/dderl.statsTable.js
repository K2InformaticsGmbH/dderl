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

        _handlers : {queryResult        : function(e, _result) { e.data._createHisto(_result); },
                     updateData         : function(e, _result) { e.data._updatePlot(_result); },
                     statsResult        : function(e, _result) { e.data._reload(_result); },
                     statsLoadResult    : function(e, _result) { e.data.open(_result); }
                    },

        _toolbarButtons : {'restart'  : {tip: 'Reload', typ : 'btn', icn : 'arrowrefresh-1-e', clk : '_toolBarReload',   dom: '_tbReload' },
                           'textBox'  : {tip: '',       typ : 'txt',                           clk : '_toolBarTxtBox',   dom: '_tbTxtBox' }},

        // slick context menus
        _statsSlkHdrCnxtMnu  : {'Hide'      : '_hide',
                                'UnHide'    : '_unhide'},

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

            if(self.options.title !== self._title) {self._title = self.options.title;}
            if(self.options.dderlStatement  !== self._stmt) {self._stmt = self.options.dderlStatement};
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
                    .css('font-size', self._fntSz)
            }

            // toolbar container
            self._footerDiv = $('<div>').appendTo(self.element);
            self._createDlgFooter();

            // create the dialog
            self.options.minWidth = self._footerWidth;
            self._dlg = self.element.dialog(self.options)
                .bind("dialogresize", function(event, ui) {
                    self._grid.resizeCanvas();
                    self._dlgResized = true;
                })
                .bind("dialogfocus", function(event, ui) {
                    self._grid.focus();
                })
                .bind("dialogbeforeclose", function(event, ui) {
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

            self._createSlickGrid();
            self._cnxtMenu('_statsSlkHdrCnxtMnu');

            // setting up the event handlers last to aid debuggin
            for(var fun in self._handlers) {
                self.element.on(fun, null, self, self._handlers[fun]);
            }
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

            // footer items
            for(btn in self._toolbarButtons) {
                var btnTxt = self._toolbarButtons[btn].tip;
                var elm = self._toolbarButtons[btn];

                var toolElmFn = function(e) {
                    var self = e.data;
                    var _btn = $(this).data('tag');
                    var fName = self._toolbarButtons[_btn].clk;
                    var f = self[fName];
                    if($.isFunction(f)) {
                        f(self);
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
            for(var i = 0; i < childs.length; ++i) {
                totWidth += $(childs[i]).width();
            }

            self._footerWidth = totWidth;
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
            if($('#'+_menu).length === 0) {
                var mnu = $('<ul>')
                    .attr('id', _menu)
                    .addClass("context_menu")
                    .hide()
                    .mouseleave(function(e) {
                        var self = $('#'+_menu).data('cnxt');
                        e.preventDefault();
                        $(this).hide();
                        self._grid.focus();
                    })
                    .appendTo(document.body);
                for(var m in this[_menu]) {
                    if($.type(this[_menu][m]) === "string") {
                        $('<li>')
                            .attr("action", m)
                            .click(function(e) {
                                var self = $('#'+_menu).data('cnxt');
                                if(undefined != self) {
                                    var columnId = null;
                                    console.log('self title _cnxtMenu ' + self.options.title);
                                    if(_menu === '_statsSlkHdrCnxtMnu') {
                                        columnId = $('#'+_menu).data('column-id');
                                    }
                                    self[_menu].dom.hide();
                                    self._cnxtMenuAction(_menu, $(this).attr("action"), columnId);
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
        _cnxtMenuAction: function(_menu, _action, _columnId) {
            var funName = this[_menu][_action];
            var fun = $.proxy(this[funName], this);
            if($.isFunction(fun)) {
                var data = this._grid.getSelectionModel().getSelectedRanges();
                //console.log('applying fun '+funName+' for \''+_action+ '\' in '+_menu+' for '+data);
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
            self._dlg.dialog("option", "position", {at : 'left top', my : 'left top', collision : 'flipfit'});
            self._dlg.dialog("widget").draggable("option", "containment", "#main-body");
            self._dlg.dialog("widget").appendTo("#main-body");
            if(self._parent) {
                smartDialogPosition($("#main-body"), self._parent, self._dlg, ['center']);
            } else {
                smartDialogPosition($("#main-body"), $("#main-body"), self._dlg, ['center']);
            }
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

        setColumns: function(columns, hasPercent) {
            var self = this;
            var dlg = self._dlg.dialog('widget');

            // Column Data
            var fldWidth = 0;
            self._origcolumns = {};
            columns[0].formatter = Slick.Formatters.IdFormatter;
            columns[0]['cannotTriggerInsert'] = true;
            columns[0]['resizable']     = false;
            columns[0]['sortable']      = false;
            columns[0]['selectable']    = false;
            for (var i = 1; i < columns.length; ++i) {
                columns[i]['resizable']     = true;
                columns[i]['sortable']      = true;
                columns[i]['selectable']    = true;
                if(columns[i].type == "numeric") {
                    columns[i].cssClass = "numeric";
                    columns[i].headerCssClass = "numeric";
                }
                if(i == columns.length - 1 && hasPercent) {
                    columns[i].formatter = Slick.Formatters.Percent;
                } else {
                    columns[i].formatter = Slick.Formatters.BinStringText;
                }
                fldWidth = self._txtlen.text(columns[i].name).width()+45;
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
        appendRows: function(_rows)
        {
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
                } while (_rows.rows.length > 0)
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

                // since columns' width doesn't change after the first block we can skip this
                if (firstChunk) {
                    var dlg = this._dlg.dialog('widget');

                    if (!self._dlgResized) {
                        var gWidth = self._getGridWidth() + 15;
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
            }

            //console.timeEnd('appendRows');
            //console.profileEnd('appendRows');
        },

        _createHisto: function(_data) {
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
                    for(var i = 0; i < self._series.length; ++i) {
                        self._series[i].splice(0, self._series[i].length - data.keep);
                    }
                    break;
                case "prp": // prepend
                    while(rows.length > 0) {
                        var row = rows.pop();
                        for(var j = 1; j < self._columns.length; ++j) {
                            // The first column is the date.
                            var time = self._parseDate(row[self._columns[0]]);
                            var value = row[self._columns[j]];
                            self._series[j-1].splice(0, 0, [time, value]);
                        }
                    }
                    for(var i = 0; i < self._series.length; ++i) {
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
                for (var i = 0; i < tbClass.length; ++i)
                    self._tbTxtBox.removeClass(tbClass[i]);
                self._tbTxtBox.addClass('tb_'+data.state);
                if(data.message.length > 0) alert_jq(data.message);

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
            ajaxCall(this, 'button', {button: {connection: dderlState.connection,
                                                    statement : this._stmt,
                                                    btn       : button
                                                   }}, 'button', 'updateData');
        },

        _handleClick: function(e, args) {
            var self = this;
            self._dlg.dialog("moveToTop");
        },

        // Recover the focus if the vieport gets a mouse event.
        _handleMouseDown: function(e, args) {
            console.log("handle mouse down");
            var self = this;
            // If the tale is disabled do not set the focus.
            if(self._divDisable) {
                return;
            }

            self._dlg.dialog("moveToTop");
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
                self._grid.focus();
                var cellEditor = self._grid.getCellEditor();
                if(cellEditor && !cellEditor.isFocused()) {
                    cellEditor.focus();
                }
                console.log("Focus set");
            }
        },

        _handleDragInit: function(e, args) {
            e.stopImmediatePropagation();
            var self = this;
            self._dlg.dialog("moveToTop");
            self._grid.focus();
            console.log("Focus set");
        },

        _handleKeyDown: function(e, args) {
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
            self._grid.render();
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
        }
    });
}( jQuery ) );
