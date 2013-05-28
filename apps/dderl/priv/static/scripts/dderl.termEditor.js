(function( $ ) {
    $.widget("dderl.termEditor", $.ui.dialog, {
        _dlg            : null,
        _termOwner      : null,
        _term           : null,
        _editDiv        : null,
        _editText       : null,
        _footerDiv      : null,
        _footerWidth    : 0,
        _txtlen         : null,
        _fnt            : null,

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

        // These options will be used as defaults
        options: {
            // dialog options default override
            height          : 500,
            width           : 500,
            minHeight       : 50,
            minWidth        : 100,
            resizable       : true,
            modal           : false,
            canMinimize     : true,
            canMaximize     : true,
            closeOnEscape   : false,
            clear           : null,
            toolBarHeight   : 27,
            open            : function(e,ui) {
                $(this).dialog("widget").appendTo("#main-body");
            },
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
                .val(self._term);

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
            //self._createDlg();

            self._dlg = self.element.dialog(self.options);
            // setting up the event handlers last to aid debugging
            //self._setupEventHandlers();
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

        open: function() {
            this._dlg.dialog("option", "position", {at : 'left top', my : 'left top', collision : 'flipfit'});
            this._dlg.dialog("open").dialog("widget").draggable("option","containment","#main-body");
        },
    });
}( jQuery ) );