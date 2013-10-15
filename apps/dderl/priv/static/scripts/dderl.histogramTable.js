(function( $ ) {
    $.widget("dderl.histogramTable", $.ui.dialog, {
        _dlg      : null,
        _tableDiv : null,
        _footerDiv: null,
        _grid     : null,
        _gdata    : null,
        _txtlen   : null,
        _cmd      : "",
        _stmt     : "",
        _columns  : null,

        _handlers : {queryResult : function(e, _result) { e.data._createHisto(_result); },
                     updateData  : function(e, _result) { e.data._updatePlot(_result); }
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
                           '...'      : {tip: 'Skip to end and Tail',  typ : 'btn', icn : 'fetch-only',       clk : '_toolBarSkipTl',   dom: '_tbSkipTl' }},

        // These options will be used as defaults
        options: {
            // dialog options default override
            height          : 350,
            width           : 520,
            minHeight       : 250,
            minWidth        : 350,
            resizable       : true,
            modal           : false,
            title           : "",
            canMinimize     : true,
            canMaximize     : true,
            closeOnEscape   : false,
            clear           : null,
            toolBarHeight   : 20,
            initialQuery    : "",
            dderlStatement  : null,
            close           : function() {
                $(this).dialog('destroy');
                $(this).remove();
            },

            // slickgrid options default override
            slickopts       : { editable: true,
                                enableAddRow: true,
                                enableColumnReorder: true,
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

            self._plotDiv = $('<div>').appendTo(self.element);
            self._dlg = self.element.dialog(self.options);

            for(var fun in self._handlers) {
                self.element.on(fun, null, self, self._handlers[fun]);
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
        },

        _init: function() {
            var self = this;

            // default dialog open behavior
            if (self.options.autoOpen) {
                self._dlg.dialog("open");
            }
        },

        open: function(rows) {
            var self = this;
            self._dlg.dialog("option", "position", {at : 'left top', my : 'left top', collision : 'flipfit'});
            self._dlg.dialog("widget").draggable("option", "containment", "#main-body");
            self._dlg.dialog("widget").appendTo("#main-body");
            self.setColumns();
            self.appendRows(self._createGridRows(rows));
        },

        _createGridRows: function(origRows) {
            var rows, count;
            rows = [];
            count = 0;
            for(var value in origRows) {
                ++count;
                rows.push({"id":count,"op":"nop","value":value,"count":origRows[value]});
            }
            return {op: "rpl", focus: 0, rows: rows};
        },

        _createSlickGrid: function() {
            var self = this;

            // building slickgrid
            // a dummy column needed to be added to enable slickgrid to enable column re-order
            self._grid = new Slick.Grid(self._tableDiv, [], [{id: "_"}], self.options.slickopts);
            self._grid.setSelectionModel(new Slick.CellRowColSelectionModel());
            var copyManager = new Slick.CellExternalCopyManager();
            self._grid.registerPlugin(copyManager);
//            copyManager.onPasteCells.subscribe($.proxy(self._gridPasteCells, self));

/*            self._grid.onContextMenu.subscribe($.proxy(self._gridContextMenu, self));
            self._grid.onHeaderContextMenu.subscribe($.proxy(self._gridHeaderContextMenu, self));
            self._grid.onCellChange.subscribe($.proxy(self._gridCellChange, self));
            self._grid.onBeforeEditCell.subscribe($.proxy(self._gridBeforeEdit, self));
            self._grid.onBeforeCellEditorDestroy.subscribe($.proxy(self._gridAfterEdit, self));
            self._grid.onAddNewRow.subscribe($.proxy(self._gridAddNewRow, self));
            self._grid.onColumnsReordered.subscribe($.proxy(self._gridColumnsReorder, self));
            self._grid.onKeyDown.subscribe($.proxy(self._handleKeyDown, self));
            self._grid.onClick.subscribe($.proxy(self._handleClick, self));
            self._grid.onMouseDown.subscribe($.proxy(self._handleMouseDown, self));
            self._grid.onDragInit.subscribe($.proxy(self._handleDragInit, self));
*/
            self._gdata = self._grid.getData();
        },

        setColumns: function() {
            var self = this;
            var dlg = self._dlg.dialog('widget');

            // Column Data
            var columns = [
                {"id":"sel","name":"","field":"id","behavior":"select","cssClass":"cell-selection","width":38,"minWidth":20,"cannotTriggerInsert":true,"resizable":false,"sortable":false,"selectable":false},
                {"id":"value","type":"text","name":"value","field":"value","resizable":true,"sortable":false,"selectable":true},
                {"id":"count","type":"numeric","name":"count","field":"count","resizable":true,"sortable":false,"selectable":true}];
            var fldWidth = 0;
            self._origcolumns = {};
            for (var i = 1; i < columns.length; ++i) {
                if(columns[i].type == "numeric") {
                    columns[i].cssClass = "numeric";
                    columns[i].headerCssClass = "numeric";
                }
                columns[i].formatter = Slick.Formatters.BinStringText;
                fldWidth = self._txtlen.text(columns[i].name).width()+25;
                if(columns[i].hasOwnProperty('editor')) {
                    columns[i].editor = Slick.Editors.ControlChars;
                }
                columns[i].minWidth = 40;
                columns[i].width    = fldWidth;
                self._origcolumns[columns[i].field] = i;
            }

            // load the column layout if its was saved
//            if(self._clmlay !== null) {
            if(self._clmlay) {
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
                        // adjusting the column to fill the rest of the window
                        if((self._getGridWidth() + 18) < dlg.width()) {
                            c[c.length - 1].width += (dlg.width()-self._getGridWidth()-18);
                            self._grid.setColumns(c);
                        }
                    }
                }
                // If we are tailing we need to keep the editor
                // TODO: Improve this design to avoid errors in edge cases.
                // Note: it is not posible to call invalidate before checking for
                //       the cell editor, since calling invalidate will destroy it.
                if(self._grid.getCellEditor() && _rows.loop == "tail") {
                    self._grid.invalidate();
                    if(self._grid.getActiveCell().row < self._gdata.length) {
                        self._grid.editActiveCell();
                    }
                } else if(self._pendingEditorCell && _rows.op == "ins") {
                    self._grid.invalidate();
                    self._grid.setActiveCell(self._pendingEditorCell.row + 1,
                                             self._pendingEditorCell.cell);
                    delete self._pendingEditorCell;
                } else {
                    self._grid.invalidate();
                }

                // loading of rows is the costliest of the operations
                // compared to computing and adjusting the table width/height
                // (so for now total time of function entry/exit is appromately equal to only row loading)
                //

                // update row styles
                self._applyStyle();
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
            ajaxCall(this, '/app/button', {button: {connection: dderlState.connection,
                                                    statement : this._stmt,
                                                    btn       : button
                                                   }}, 'button', 'updateData');
        },

        /*
         * Toolbar callbak functions
         */
        // NOTE: self is 'this' and 'this' is dom ;)
        _toolBarReload: function(self) {
            self.buttonPress("restart");
        },

        _toolBarSkFrst: function(self) {
            self.buttonPress("|<");
        },
        _toolBarJmPrev: function(self) {
            self.buttonPress("<<");
        },
        _toolBarGo2Prv: function(self) {
            self.buttonPress("<");
        },
        _toolBarTxtBox: function(self) {
            if(self.hasOwnProperty('_toolBarTxtBoxVal')) {
                self.buttonPress(self._toolBarTxtBoxVal);
            }
        },
        _toolBarGo2Nex: function(self) {
            self.buttonPress(">");
        },
        _toolBarJmNext: function(self) {
            self.buttonPress(">>");
        },
        _toolBarSekEnd: function(self) {
            self.buttonPress(">|");
        },
        _toolBarSkTail: function(self) {
            self.buttonPress(">|...");
        },
        _toolBarSkipTl: function(self) {
            self.buttonPress("...");
        }
    });
}( jQuery ) );
