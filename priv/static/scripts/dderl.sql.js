function StartSqlEditor() {
$('<div>')
    .appendTo(document.body)
    .sql({autoOpen  : false,
    title     : null,
    cmdOwner  : null
    })
    .sql('open');
}

(function( $ ) {
  var DEFAULT_COUNTER = 0;
  $.widget( "dderl.sql", $.ui.dialog, {

    _cmdOwner       : null,
    _dlg            : null,
    _editDiv        : null,
    _footerDiv      : null,
    _footerWidth    : 0,
    _txtlen         : null,
    _fnt            : null,
    _fntSz          : null,

    _flatTb         : null,
    _prettyTb       : null,
    _boxDiv         : null,

    _modCmd         : "",
    _cmdFlat        : "",
    _cmdPretty      : "",
    _boxJson        : {},  

    // private event handlers
    _handlers       : { parsedCmd : function(e, _parsed) {
                            var self = e.data; 
                            self._checkParsed(_parsed);
                            self._renderParsed(_parsed);
                        }
                      },

    _toolsBtns      : {'Validate SQL'               : { typ : 'btn', icn : 'refresh',       clk : '_toolBarValidate'        },
                       'Execute fetch first block'  : { typ : 'btn', icn : 'play',          clk : '_toolBarTblReload'       },
                       'Execute fetch to end'       : { typ : 'btn', icn : 'seek-end',      clk : '_toolBarTblRefetch'      },
                       'Execute fetch tail mode'    : { typ : 'btn', icn : 'fetch-tail',    clk : '_toolBarTblFetchNTail'   },
                       'Execute tail mode only'     : { typ : 'btn', icn : 'fetch-only',    clk : '_toolBarTblTailOnly'     }},

    // These options will be used as defaults
    options: {
        // dialog options default override
        toolBarHeight   : 20,
        height          : 500,
        width           : 500,
        minHeight       : 50,
        minWidth        : 100,
        position        : { at        : 'left top',
                            my        : 'left top+21',
                            collision : 'flipfit' },
        resizable       : true,
        modal           : false,
        title           : "_Set TITLE here_",
        canMinimize     : true,
        canMaximize     : true,
        closeOnEscape   : false,
        clear           : null,
        toolBarHeight   : 27,
        open            : function(e,ui) {},
        focus           : function(e,ui) {},
        close           : function() {
                            $(this).dialog('destroy');
                            $(this).remove();
                          },
        cmdOwner        : null
    },
 
    // Set up the widget
    _create: function() {
        var self = this;

        self._fnt = $(document.body).css('font-family');
        self._fntSz = $(document.body).css('font-size');

        // preserve some options
        if(self.options.cmdOwner    !== self._dlg) self._cmdOwner   = self.options.cmdOwner;
        if(self.options.cmdFlat     !== self._dlg) self._cmdFlat    = self.options.cmdFlat;
        if(self.options.cmdPretty   !== self._dlg) self._cmdPretty  = self.options.cmdPretty;
        if(self.options.title       !== self._title) {
            if(self.options.title === null) {
                self.options.title = 'Query'+DEFAULT_COUNTER+'.sql';
                ++DEFAULT_COUNTER;
            }
            self._title = self.options.title;
        }

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

        //
        // editor container
        //

        var flatBg      = 'rgb(240,240,255)';
        var prettyBg    = 'rgb(255,240,240)';
        var boxBg       = 'rgb(240,255,240)';

        self._flatTb =
            $('<textarea>')
            .css('background', flatBg)
            .css('width', '100%')
            .css('height', '100%')
            .css('overflow-y', 'scroll')
            .css('resize', 'none')
            .css('border', 'none')
            .on('keyup click blur focus change paste', this, function(e) {
                var that = e.data;
                that._cmdFlat = $(this).val();
                that._modCmd = that._cmdFlat;
            })
            .text(self._cmdFlat);

        self._prettyTb =
            $('<textarea>')
            .css('background', prettyBg)
            .css('width', '100%')
            .css('height', '100%')
            .css('overflow', 'auto')
            .attr('wrap', 'off')
            //.css('white-space', 'nowrap')
            .css('resize', 'none')
            .css('border', 'none')
            .on('keyup click blur focus change paste', this, function(e) {
                var that = e.data;
                that._cmdPretty = $(this).val();
                that._modCmd = that._cmdPretty;
            })
            .text(self._cmdPretty);

        self._boxDiv =
            $('<div>')
            .css('font-family', self._fnt)
            .css('font-size', self._fntSz)
            .css('margin', 0)
            .css('background', boxBg);

        self._editDiv =
            $('<div>')            
            .append(
              $('<ul>'
              +'  <li style="background:'+flatBg+'"><a href="#tabflat">Flat</a></li>'
              +'  <li style="background:'+prettyBg+'"><a href="#tabpretty">Pretty</a></li>'
              +'  <li style="background:'+boxBg+'"><a href="#tabbox">Box</a></li>'
              +'</ul>')
            )
            .append(
              $('<div>')
              .attr('id','tabflat')
              .append(self._flatTb)
            )
            .append(
              $('<div>')
              .attr('id','tabpretty')
              .append(self._prettyTb)
            )
            .append(
              $('<div>')
              .attr('id','tabbox')
              .append(self._boxDiv)
            )
            .css('position', 'absolute')
            .css('overflow', 'hidden')
            .css('top', '0')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', self.options.toolBarHeight+'px')
            .tabs()
            .on("tabsactivate", function(event, ui) {
                var shouldReparse = false;                
                     if(ui.oldPanel.attr('id') === 'tabpretty'
                        && (ui.newPanel.attr('id') === 'tabflat' || ui.newPanel.attr('id') === 'tabbox'))
                        shouldReparse = true;
                else if(ui.oldPanel.attr('id') === 'tabflat'
                        && (ui.newPanel.attr('id') === 'tabpretty' || ui.newPanel.attr('id') === 'tabbox'))
                        shouldReparse = true;
                else if(ui.oldPanel.attr('id') === 'tabbox'
                        && (ui.newPanel.attr('id') === 'tabflat' || ui.newPanel.attr('id') === 'tabpretty'))
                        shouldReparse = false;

                if(shouldReparse)
                    self._ajaxCall('/app/parse_stmt', {parse_stmt: {qstr:self._modCmd}},'parse_stmt','parsedCmd');
            })
            .removeClass('ui-corner-all')
            .appendTo(self.element);

        var tabTitles = self._editDiv.find('ul');

        tabTitles
            .removeClass('ui-corner-all')
            .css('border-top', 'none')
            .css('border-left', 'none')
            .css('border-right', 'none');
        self._editDiv.find('li').removeClass('ui-corner-top');

        var titleHeight = tabTitles.height();
        $('#tabflat, #tabpretty, #tabbox').css('top', titleHeight+'px');

        // toolbar container
        self._footerDiv = $('<div>').appendTo(self.element);

        // need the max footer with to set as dlg minWidth
        self._createDlgFooter();
        self._createDlg();

        // setting up the event handlers last to aid debugging
        self._setupEventHandlers();
    },

    _init: function() {
        var self = this;

        // default dialog open behavior
    	if (self.options.autoOpen)
            self._dlg.dialog("open");

        if (undefined != self._cmdFlat && self._cmdFlat.length > 0)
            this._ajaxCall('/app/parse_stmt', {parse_stmt: {qstr:self._cmdFlat}},'parse_stmt','parsedCmd');
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

        self._footerWidth = self._addBtngrpToDiv(self._toolsBtns, self._footerDiv);
    },

    _addBtngrpToDiv: function(_toolsBtns, _toolDiv) {
        var self = this;

        for(btnTxt in _toolsBtns) {
            var elm = _toolsBtns[btnTxt];

            var toolElmFn = function(e) {
                var self = e.data;
                var _btnTxt = $(this).text();
                var fName = _toolsBtns[_btnTxt].clk;
                var f = $.proxy(self[fName], self);
                if($.isFunction(f))
                    f();
                else
                    throw('['+self.options.title+'] toolbar '+_btnTxt+' has unimplimented cb '+fName);
            };

            var inph = self.options.toolBarHeight;
// TODO jQ 1.9 deprecated $.browser find work around
//            if($.browser.msie) inph -= 2;

            $('<button>')
                .text(btnTxt)
                .button({icons: {primary: 'ui-icon-' + elm.icn}, text: false})
                .css('height', this.options.toolBarHeight+'px')
                .click(self, toolElmFn)
                .appendTo(_toolDiv);
        }
        _toolDiv
            .buttonset()
            .css('height', (self.options.toolBarHeight)+'px');

        // footer total width
        var childs = _toolDiv.children();
        var totWidth = 0;
        for(var i=0; i<childs.length; ++i)
            totWidth += $(childs[i]).width();

        return totWidth;
    },
  
    /*
     * Toolbar callbak functions
     */
    _toolBarValidate: function() {
        this._ajaxCall('/app/parse_stmt', {parse_stmt: {qstr:this._modCmd}},'parse_stmt','parsedCmd');
    },
    _toolBarTblReload: function() {
        if(null === this._cmdOwner) {
            $('<div>')
            .appendTo(document.body)
            .table({
                title       : this._title,
                autoOpen    : false,
                dderlSession: session,
                dderlAdapter: adapter,
            })
            .table('cmdReload', this._modCmd)
            .table('open');
        } else
            this._cmdOwner.cmdReload(this._modCmd);
    },
    _toolBarTblRefetch: function() {
        throw('unimplimented');
    },
    _toolBarTblFetchNTail: function() {
        throw('unimplimented');
    },
    _toolBarTblTailOnly: function() {
        throw('unimplimented');
    },
    ////////////////////////////

    /*
     * _ajaxCall success handlers
     */
    _checkParsed: function(_parsed) {
        var error = ''
        if(_parsed.hasOwnProperty('boxerror'))      error += 'Box Error - <br>'+_parsed.boxerror+'<br>';
        if(_parsed.hasOwnProperty('prettyerror'))   error += 'Pretty Error - <br>'+_parsed.prettyerror+'<br>';
        if(_parsed.hasOwnProperty('flaterror'))     error += 'Flat Error - <br>'+_parsed.flaterror+'<br>';

        if (error.length > 0)
            alert_jq(error);
    },
    _renderParsed: function(_parsed) {
        if(_parsed.hasOwnProperty('sqlbox')) {
            this._boxJson = _parsed.sqlbox.box;
            build_boxes(this._boxDiv, this._boxJson);
        }
        if(_parsed.hasOwnProperty('pretty')) {
            this._prettyTb.text(_parsed.pretty);
            this._cmdPretty = this._prettyTb.val();
        }
        if(_parsed.hasOwnProperty('flat')) {
            this._flatTb.text(_parsed.flat);
            this._cmdFlat = this._flatTb.val();
        }
    },

    _createDlg: function() {
        var self = this;                    

        // dlg width can't be less than footer width
        self.options.minWidth = self._footerWidth;
        self._dlg = self.element
            .dialog(self.options);
//            .bind("dialogresize", function(event, ui) 
//            {
//            });

        // // for dialog title as html DOM / jQuery Obj
        // self._dlg.data( "uiDialog" )._title = function(title) {
        //     title.html('');
        //     this.options.title
        //         .click(function(e) {
        //             self._dlgTtlCnxtMnu.dom
        //                 .css("top", e.clientY - 10)
        //                 .css("left", e.clientX)
        //                 .data('cnxt', self)
        //                 .show();
        //         });
        //     title.append( this.options.title );
        // };

        // // converting the title text to a link
        // self._dlg.dialog('option', 'title', $('<a href="#">'+self.options.title+'</a>'));
    },
 
    // translations to default dialog behavior
    open: function() { this._dlg.dialog("open"); },
    close: function() { this._dlg.dialog("close"); },
    destroy: function() { this._dlg.dialog("destroy"); },

    showCmd: function(cmd) {
        var self = this;
        self._modCmd = cmd;
        self._flatTb.text(cmd);
        self._cmdFlat = self._flatTb.val();
        this._ajaxCall('/app/parse_stmt', {parse_stmt: {qstr:cmd}},'parse_stmt','parsedCmd');
    },

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
                console.log('received '+ JSON.stringify(_data));

                // save the new session - legacy maybe removed TODO
                if(_data.hasOwnProperty('session'))
                    this.options.dderlSession = self._session = _data.session;

                if(_data.hasOwnProperty(_resphead)) {
                    if(_data[_resphead].hasOwnProperty('error'))
                        alert_jq(_data[_resphead]['error']);
                    else {
                        if(this._handlers.hasOwnProperty(_successevt))
                            this.element.trigger(_successevt, _data[_resphead]);
                        else
                            throw('unsupported success event '+_successevt+' for '+_url);
                    }
                } else throw('resp '+_resphead+' doesn\'t match the request '+_url);
            }
        });
    },

    // // Use the _setOption method to respond to changes to options
    // _setOption: function( key, value ) {
    //     var self = this;
    //     var save = false;
    //     switch( key ) {
    //       case "clear":             // handle changes to clear option
    //         save = true;
    //         break;
    //     }
 
    //     // In jQuery UI 1.9 and above, you use the _super method instead
    //     if (save) this._super('_setOption', key, value);
    // },
 
    // Use the destroy method to clean up any modifications your widget has made to the DOM
    _destroy: function() {
        --DEFAULT_COUNTER;
        if(DEFAULT_COUNTER < 0)
            DEFAULT_COUNTER=0;
    },

  });
}( jQuery ) );
