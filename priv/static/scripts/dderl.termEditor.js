(function( $ ) {
    $.widget("dderl.termEditor", $.ui.dialog, {
        _dlg            : null,
        _termOwner      : null,
        _container      : null,
        _term           : null,
        _editDiv        : null,
        _editText       : null,
        _footerDiv      : null,
        _footerWidth    : 0,
        _txtlen         : null,
        _fnt            : null,
        _currentExpLvl  : -1,
        _tabDefault     : "    ",
        _isJson         : false,

        _handlers       : {
            updateTextArea      : function(e, _result) { e.data._updateTextArea(_result); },
            saveChangesResponse : function(e, _result) { e.data._saveChangesResponse(_result); }
        },

        _toolbarButtons : {
            'format' : {tip: 'Auto indent',       typ : 'btn', icn : 'arrowrefresh-1-e', clk : '_autoFormat', dom: '_tbAutoFormat'},
            '<'      : {tip: 'Reduce expansion',  typ : 'btn', icn : 'rev-play', clk : '_decreaseExp', dom: '_tbExpDown'},
            'textBox': {tip: 'Expansion level',   typ : 'txt',                   clk : '_setExpLevel', dom: '_tbTxtBox' },
            '>'      : {tip: 'Increase expansion',typ : 'btn', icn : 'play',     clk : '_increaseExp', dom: '_tbExpUp' },
            'accept' : {tip: 'Set changes',       typ : 'btn', icn : 'check',    clk : '_saveChanges', dom: '_tbAccept' },
            'cancel' : {tip: 'Discard changes',   typ : 'btn', icn : 'close',    clk : '_abortChanges',dom: '_tbCancel' }},


        // These options will be used as defaults
        options: {
            // dialog options default override
            height          : 400,
            width           : 450,
            minHeight       : 50,
            minWidth        : 100,
            resizable       : true,
            modal           : false,
            canMinimize     : true,
            canMaximize     : true,
            closeOnEscape   : false,
            clear           : null,
            toolBarHeight   : 20,
            focus           : function(e,ui) {},
            close           : function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        },

        // Set up the widget
        _create: function() {
            var self = this;

            self._fnt = $(document.body).css('font-family');
            self._fntSz = $(document.body).css('font-size');

            // preserve some options
            if(self.options.termOwner   !== self._termOwner)   self._termOwner = self.options.termOwner;
            if(self.options.term        !== self._term)        self._term      = self.options.term;
            if(self.options.container   !== self._container)   self._container = self.options.container;
            if(self.options.isJson      !== self._isJson)      self._isJson    = self.options.isJson;

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
                    .css('font-size', self._fntSz);
            }

            // editor container
            self._editText =
                $('<textarea>')
                .addClass('sql_text_flat')
                .attr('wrap', 'off')
                .val(self._term)
                .keydown(function(e) {
                    if(e.keyCode === 9) {
                        var start = this.selectionStart;
                        var end = this.selectionEnd;
                        var oldText = self._editText.val();
                        self._editText.val(oldText.substring(0, start)
                                           + self._tabDefault
                                           + oldText.substring(end));
                        this.selectionStart = this.selectionEnd = (start + self._tabDefault.length);
                        e.preventDefault();
                        e.stopImmediatePropagation();
                        return false;
                    }
                    return true;
                });

            self._editDiv =
                $('<div>')
                .append(self._editText)
                .css('position', 'absolute')
                .css('overflow', 'hidden')
                .css('top', '0')
                .css('left', '0')
                .css('right', '0')
                .css('bottom', self.options.toolBarHeight+'px')
                .removeClass('ui-corner-all')
                .appendTo(self.element);

            // toolbar container
            self._footerDiv = $('<div>').appendTo(self.element);

            // need the max footer with to set as dlg minWidth
            self._createDlgFooter();
            self.options.minWidth = self._footerWidth;
            self._updateTxtBox();
            self._dlg = self.element.dialog(self.options);
            // setting up the event handlers last to aid debugging
            self._setupEventHandlers();
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
                    .attr('size', 5)
                    .data('tag', btn)
                    .button()
                    .addClass('tb_empty')
                    .css('height', (inph-2)+'px')
                    .css('text-align', 'right')
                    .css('padding', '0')
                    .css('margin', '0px -1px 0px 0px')
                    .keypress(function(evt) {
                        if(evt.which == 13) {
                            var explvlnum = parseInt($(this).val());
                            if(explvlnum != NaN) {
                                self._currentExpLvl = explvlnum;
                            } else {
                                self._currentExpLvl = -1;
                            }
                            evt.data = self;
                            toolElmFn.call(this, evt);
                        }
                        return true;
                    })
                    .appendTo(self._footerDiv);

                if(self.options.readOnly) {
                    if(btn === "accept" || btn === "cancel") {
                        var tbBtnObj = self._toolbarButtons[btn];
                        var btnElm = self[tbBtnObj.dom];
                        btnElm.button('disable').attr('title', "Read only");
                    }
                }
            }

            if(self.options.readOnly) {
                self._editText.attr('wrap', 'on');
            }
            self._footerDiv
                .buttonset()
                .css('height', (self.options.toolBarHeight)+'px');

            // footer total width
            var childs = self._footerDiv.children();
            var totWidth = 0;
            for(var i=0; i < childs.length; ++i)
                totWidth += $(childs[i]).width();

            self._footerWidth = totWidth;
        },

        _updateTxtBox: function() {
            var self = this;
            if(self._currentExpLvl < 0) {
                self._tbTxtBox.val("auto");
            } else {
                self._tbTxtBox.val(self._currentExpLvl);
            }
        },

        open: function() {
            this._dlg.dialog("option", "position", {at : 'left top', my : 'left top', collision : 'flipfit'});
            this._dlg.dialog("open").dialog("widget").draggable("option","containment", this._container);
            this._dlg.dialog("widget").appendTo(this._container);
        },

        destroy: function() {
            this._termOwner.enableDialog();
        },

        updateExp: function(expansionLevel, force) {
            var stringToFormat = unescapeNewLines(this._editText.val());
            var expansionWithAuto = (expansionLevel < 0)? "auto": expansionLevel;
            ajaxCall(this, '/app/format_erlang_term', {
                format_erlang_term: {
                    erlang_term: stringToFormat, 
                    expansion_level: expansionWithAuto,
                    force: force
                }
            }, 'format_erlang_term', 'updateTextArea');
        },

        /*
         * Toolbar callbak functions
         */
        // NOTE: self is 'this' and 'this' is dom ;)
        _autoFormat: function(self) {
            console.log('cb _autoFormat current ' + self._currentExpLvl);
            self.updateExp(self._currentExpLvl, true);
        },
        _decreaseExp: function(self) {
            console.log('cb _decreaseExp current: ' + self._currentExpLvl);
            self._currentExpLvl = self._currentExpLvl - 1;
            if(self._currentExpLvl < -1) {
                self._currentExpLvl = -1;
            }
            self._updateTxtBox();
            self.updateExp(self._currentExpLvl, false);
        },
        _setExpLevel: function(self) {
            console.log('cb _setExpLevel ' + self._currentExpLvl);
            if(self._currentExpLvl < -1) {
                self._currentExpLvl = -1;
            }
            self._updateTxtBox();
            self.updateExp(self._currentExpLvl, false);
        },
        _increaseExp: function(self) {
            console.log('cb _increaseExp current: ' + self._currentExpLvl);
            self._currentExpLvl = self._currentExpLvl + 1;
            if(self._currentExpLvl < -1) {
                self._currentExpLvl = -1;
            }
            self._updateTxtBox();
            self.updateExp(self._currentExpLvl, false);
        },
        _saveChanges: function(self) {
            console.log('cb _saveChanges: the new term: ' + self._editText.val());
            var stringToFormat = unescapeNewLines(self._editText.val());
            var expansionWithAuto = (self._currentExpLvl < 0)? "auto": self._currentExpLvl;
            if(self._isJson) {
                ajaxCall(self, '/app/format_json_to_save', {
                    format_json_to_save: {
                        json_string: stringToFormat,
                        expansion_level: expansionWithAuto,
                        force: false
                    }
                }, 'format_json_to_save', 'saveChangesResponse');
            } else {
                ajaxCall(self, '/app/format_erlang_term', {
                    format_erlang_term: {
                        erlang_term: stringToFormat,
                        expansion_level: expansionWithAuto,
                        force: false
                    }
                }, 'format_erlang_term', 'saveChangesResponse');
            }
        },
        _abortChanges: function(self) {
            console.log('['+self.options.title+'] cb _abortChanges');
            self._dlg.dialog("close");
        },
        ////////////////////////////

        /*
         * ajaxCall callbacks
         */
        _updateTextArea: function(formattedString) {
            if(formattedString.hasOwnProperty('error')) {
                alert_jq('Error : '+ formattedString.error);
            } else {
                this._editText.val(formattedString);
            }
        },

        _saveChangesResponse: function(formattedString) {
            if(formattedString.hasOwnProperty('error')) {
                alert_jq('Error : ' + formattedString.error);
            } else {
                this._termOwner.updateErlangCell(formattedString);
                this._dlg.dialog("close");
            }
        }
        ////////////////////////////
    });
}( jQuery ) );
