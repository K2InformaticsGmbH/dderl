(function( $ ) {
    $.widget("dderl.plotTable", $.ui.dialog, {
        _dlg      : null,
        _plotDiv  : null,
        _footerDiv: null,
        _cmd      : "",
        _stmt     : "",
        _columns  : null,
        _series   : null,

        _handlers : {queryResult : function(e, _result) { e.data._createPlot(_result); },
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
            close           : function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        },

        _create : function() {
            var self = this;

            if(self.options.title !== self._title) {self._title = self.options.title;}
            if(self.options.initialQuery != self._cmd) {self._cmd = self.options.initialQuery;}

            self._plotDiv = $('<div>').appendTo(self.element);
            self._dlg = self.element.dialog(self.options);
            
            for(var fun in self._handlers) {
                self.element.on(fun, null, self, self._handlers[fun]);
            }

            self._series = [];

            // toolbar container
            self._footerDiv = $('<div>').appendTo(self.element);
            self._createDlgFooter();
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
            for(var i=0; i<childs.length; ++i)
                totWidth += $(childs[i]).width();
        },

        _init: function() {
            var self = this;
            
            // default dialog open behavior
    	    if (self.options.autoOpen) {
                self._dlg.dialog("open");
            }
        },

        open: function() {
            var self = this;
            self._dlg.dialog("option", "position", {at : 'left top', my : 'left top', collision : 'flipfit'});
            self._dlg.dialog("open").dialog("widget").draggable("option", "containment", "#main-body");
            self._dlg.dialog("widget").appendTo("#main-body");

            self._plotDiv.addClass('demo-container');
            self._divPlaceHolder = $('<div>').appendTo(self._plotDiv).addClass('demo-placeholder');
            ajaxCall(self, '/app/query', {query: {connection: connection, qstr : self._cmd}}, 'query', 'queryResult');
        },

        _createPlot: function(_data) {
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
                /*this.setColumns(_data.columns);
                if(_data.hasOwnProperty('sort_spec') && !$.isEmptyObject(_data.sort_spec)) {
                    this._setSortSpecFromJson(this, _data.sort_spec);
                }

                this._grid.setData([]);
                this._gdata = this._grid.getData();
                this.buttonPress(this._startBtn); */
                
                this._columns = [];
                for (var i = 1; i < _data.columns.length; ++i) {
                    this._columns[i - 1] = _data.columns[i].field;
                }

                this._stmt = _data.statement;
                this.buttonPress("...");
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
            ajaxCall(this, '/app/button', {button: {connection: connection,
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
        },

        _fillSeries: function(rows) {
            var self = this;
            for(var i = 0; i < rows.length; ++i) {
                var row = rows[i];
                for(var j = 1; j < self._columns.length; ++j) {
                    // The first column is the date.
                    var time = self._parseDate(row[self._columns[0]]);
                    var value = row[self._columns[j]];
                    value = parseFloat(value);
                    if(time && value !== NaN) {
                        if(!self._series[j-1]) {
                            self._series[j-1] = [];
                        }
                        self._series[j-1].push([time, value]);
                    }
                }
            }
        },

        // The recomendation is to avoid the default javascript parser
        // http://blog.dygraphs.com/2012/03/javascript-and-dates-what-mess.html
        _parseDate: function(dateTime) {
            var dateTimeArray = dateTime.split(" ");
            if(dateTimeArray.length === 2) {
                var date = dateTimeArray[0].split(".");
                if(date.length === 3) {
                    var timeAndMicro = dateTimeArray[1].split(".");
                    if(timeAndMicro.length === 2) {
                        var time = timeAndMicro[0].split(":");
                        if(time.length === 3) {
                            var millis = Math.floor(timeAndMicro[1]/1000);
                            //Month is 0 based. // new Date(year, month [, date [, hours[, minutes[, seconds[, ms]]]]])
                            var dateObject = new Date(date[2], date[1] - 1, date[0], time[0], time[1], time[2], millis);
                            return dateObject.getTime();
                        }
                    }
                }
            }
            return null;
        }
    });
}( jQuery ) );