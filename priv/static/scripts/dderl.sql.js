function StartSqlEditor() {
$('<div>')
    .appendTo(document.body)
    .sql({autoOpen  : false,
    title     : null,
    cmdOwner  : null
    })
    .sql('open');
}

function StartSqlEditorWithTitle(title, cmd) {
$('<div>')
    .appendTo(document.body)
    .sql({autoOpen  : false,
    title     : title,
    cmdOwner  : null,
    cmdFlat   : cmd,
    })
    .sql('open');
}

function insertAtCursor(myField, myValue) {
  //IE support
  if (document.selection) {
    var temp;
    myField.focus();
    sel = document.selection.createRange();
    temp = sel.text.length;
    sel.text = myValue;
    if (myValue.length == 0) {
      sel.moveStart('character', myValue.length);
      sel.moveEnd('character', myValue.length);
    } else {
      sel.moveStart('character', -myValue.length + temp);
    }
    sel.select();
  }
  //MOZILLA/NETSCAPE support
  else if (myField.selectionStart || myField.selectionStart == '0') {
    var startPos = myField.selectionStart;
    var endPos = myField.selectionEnd;
    myField.value = myField.value.substring(0, startPos) + myValue + myField.value.substring(endPos, myField.value.length);
    myField.selectionStart = startPos + myValue.length;
    myField.selectionEnd = startPos + myValue.length;
  } else {
    myField.value += myValue;
  }
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
    _paramsDiv      : null,
    _graphEdit      : null,

    _modCmd         : "",
    _cmdFlat        : "",
    _cmdPretty      : "",
    _boxJson        : {},
    _script         : "",  
    _history        : null,
    _historySelect  : null,
    _cmdChanged     : false,
    _reloadBtn      : null,
    _spinCounter    : 0,
    _pendingQueries : null,
    _optBinds       : null,

    // private event handlers
    _handlers       : { parsedCmd       : function(e, _parsed) { e.data._renderParsed      (_parsed, false); },
                        parsedSkipFocus : function(e, _parsed) { e.data._renderParsed      (_parsed, true); },
                        reloadParsedCmd : function(e, _parsed) { e.data._reloadParsedCmd   (_parsed); },
                        opViewResult    : function(e, _result) { e.data._operateViewResult (_result); },
                        saveViewResult  : function(e, _result) { e.data._saveViewResult    (_result); },
                        resultMultStmt  : function(e, _result) { e.data._resultMultStmt    (_result); },
                        resultStmt      : function(e, _result) { e.data._resultStmt        (_result); }
                      },

    // Dialog context menus
    _sqlTtlCnxtMnu  : {
                       'Save ddView'    : '_saveView',
                       'Save ddView As' : '_saveViewAs',
                       'Rename ddView'  : '_renameView',
                       'Delete ddView'  : '_deleteView'
    },

    _toolsBtns      : {'Validate SQL'               : { typ : 'btn', icn : 'refresh',               clk : '_toolBarValidate'        },
                       'Execute fetch first block'  : { typ : 'btn', icn : 'play',                  clk : '_toolBarTblReload'       },
                       'Execute fetch to end'       : { typ : 'btn', icn : 'forward',               clk : '_toolBarTblFetch2End'    },
                       'Execute fetch tail mode'    : { typ : 'btn', icn : 'step-forward',          clk : '_toolBarTblFetchNTail'   },
                       'Execute tail mode only'     : { typ : 'btn', icn : 'step-forward ellipsis', clk : '_toolBarTblTailOnly'     },
                       ''                           : { typ : 'sel',                                clk : '_toolBarChangeSql'       }},

    // These options will be used as defaults
    options: {
        // dialog options default override
        height          : 250,
        width           : 336,
        minHeight       : 50,
        minWidth        : 100,
        resizable       : true,
        modal           : false,
        title           : "_Set TITLE here_",
        canMinimize     : true,
        canMaximize     : true,
        closeOnEscape   : false,
        clear           : null,
        toolBarHeight   : 27,
        appendTo        : "#main-body",
        open            : function(e,ui) {
                          },
        focus           : function(e,ui) {},
        close           : function() {
                            $(this).dialog('destroy');
                            $(this).remove();
                          },
        cmdFlat         : "",
        script          : "",
        cmdOwner        : null,
        optBinds        : null,
        history         : [],
        viewId          : null
    },
 
    _refreshHistoryBoxSize: function() {
        var self = this;
        // footer total width
        var childs = self._footerDiv.children();
        var totWidth = 0;
        for(var i = 0; i + 1 < childs.length; ++i) {
            totWidth += $(childs[i]).width();
        }
        self._historySelect.css('width', self._footerDiv.width() - totWidth);
    },

    // Set up the widget
    _create: function() {
        var self = this;

        self._history = [];
        self._pendingQueries = [];

        self._fnt = $(document.body).css('font-family');
        self._fntSz = $(document.body).css('font-size');

        // preserve some options
        if(self.options.cmdOwner    !== self._cmdOwner)     self._cmdOwner  = self.options.cmdOwner;
        if(self.options.cmdFlat     !== self._cmdFlat)      self._cmdFlat   = self.options.cmdFlat;
        if(self.options.script      !== self._script)       self._script    = self.options.script;
        if(self.options.optBinds    !== self._optBinds)     self._optBinds  = self.options.optBinds;
        if(self.options.history     !== self._history)      self._history   = self.options.history;
        if(self.options.title       !== self._title) {
            if(self.options.title === null) {
                self.options.title = 'Query'+DEFAULT_COUNTER+'.sql';
                ++DEFAULT_COUNTER;
                self._isDefaultTitle = true;
            }
            self._title = self.options.title;
        }

        // dialog elements

        // Set the sql to 40% of the parent window
        self.options.width = $(window).width() * 0.4;
        self.options.height = $(window).height() * 0.4;

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

        //
        // editor container
        //

        var flatBg      = 'rgb(240,240,255)';
        var prettyBg    = 'rgb(255,240,240)';
        var boxBg       = 'rgb(240,255,240)';
        var paramsBg    = '#FFFFD1';

        var sqlKeyHandle = function(e, self, _cmd) {
            if(e.type == "keydown") {
                if((e.keyCode || e.which) == 9) {
                    e.preventDefault();
                    insertAtCursor(self, "  ");
                }
            } else if(e.type == "keyup" || e.type == "paste" || e.type == "blur") {
                var that = e.data;
                _cmd = $(self).val();
                that._modCmd = _cmd;
            }
        };

        self._flatTb =
            $('<textarea autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false">')
            .addClass('sql_text_editor')        
            .addClass('sql_text_flat')
            .on('keydown keyup click blur focus change paste', this, function(e) {
                sqlKeyHandle(e, this, e.data._cmdFlat);
            })
            .val(self._cmdFlat);

        self._prettyTb =
            $('<textarea autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false">')
            .attr('wrap', 'off')
            .addClass('sql_text_editor')        
            .addClass('sql_text_pretty')
            .on('keydown keyup click blur focus change paste', this, function(e) {
                sqlKeyHandle(e, this, e.data._cmdPretty);
            })
            .val(self._cmdPretty);

        self._boxDiv =
            $('<div>')
            .addClass('sql_text_box')
            .css('font-family', self._fnt);

        self._paramsDiv = $('<div>').css("display", "inline-block;");
        
        // TODO: This should be ace probably instead of just text area / snippets is good idea...
        self._graphEdit =
            $('<textarea autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false">')
            .addClass('sql_text_editor')
            .on('keydown', this, function(e) {
                if((e.keyCode || e.which) == 9) {
                    e.preventDefault();
                    insertAtCursor(this, "  ");
                }
            });

        if(!self._script) {
            var graphScriptHelp = "// This code is executed once and it should initialize the graph, the";
            graphScriptHelp += "\n// available parameters are (container, width, height)";
            graphScriptHelp += "\n\n// container: d3 selection of the contaner div for the graph";
            graphScriptHelp += "\n// width: width of the container";
            graphScriptHelp += "\n// height: height of the container";
            graphScriptHelp += "\n\n// The function must then return an object with the following callbacks:";
            graphScriptHelp += "\n\nreturn {";
            graphScriptHelp += "\n    on_data: function(data) {},";
            graphScriptHelp += "\n    on_resize: function(w, h) {},";
            graphScriptHelp += "\n    on_reset: function() {}";
            graphScriptHelp += "\n};";
            self._graphEdit.val(graphScriptHelp);
        } else {
            self._graphEdit.val(self._script);
        }
        
        // TODO: This should be dynamic as we need to create new script tabs on the fly.
        self._editDiv =
            $('<div>')
            .append(
              $('<ul>'
              +'  <li style="background:'+flatBg+'"><a href="#tabflat">Flat</a></li>'
              +'  <li style="background:'+prettyBg+'"><a href="#tabpretty">Pretty</a></li>'
              +'  <li style="background:'+boxBg+'"><a href="#tabbox">Box</a></li>'
              +'  <li style="background:'+paramsBg+'"><a href="#tabparams">Params</a></li>'
              +'  <li><a href="#tabgraph">D3 Graph</a></li>'
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
              .css('background-color', boxBg)
              .attr('id','tabbox')
              .append(self._boxDiv)
            )
            .append(
              $('<div>')
              .css('background-color', paramsBg)
              .css("overflow-x", "hidden")
              .css("overflow-y", "auto")
              .attr('id','tabparams')
              .append(self._paramsDiv)
            )
            .append(
                $('<div>')
                .attr('id','tabgraph')
                .append(self._graphEdit)    
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
                self._setTabFocus();
                if(ui.oldPanel.attr('id') !== ui.newPanel.attr('id') && self._modCmd) {
                    self.addWheel();
                    ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr:self._modCmd}},'parse_stmt','parsedCmd');
                }
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
        $('#tabflat, #tabpretty, #tabbox, #tabparams, #tabgraph').css('top', titleHeight+'px');

        // toolbar container
        self._footerDiv = $('<div>').appendTo(self.element);

        // need the max footer with to set as dlg minWidth
        self._createDlgFooter();
        self._createDlg();

        self._createContextMenus();

        // setting up the event handlers last to aid debugging
        self._setupEventHandlers();
    },

    getEditor: function() {
        var self = this;
        self._editDiv.find('#tabbox').append(self._boxDiv);
    },

    _init: function() {
        var self = this;

        // default dialog open behavior
    	if (self.options.autoOpen)
            self._dlg.dialog("open");

        if (undefined != self._cmdFlat && self._cmdFlat.length > 0) {
            self._modCmd = self._cmdFlat;
            self.addWheel();
            ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr:self._cmdFlat}}, 'parse_stmt', 'parsedCmd');
        }
    },

    _createContextMenus: function() {
        var self = this;

        self._cnxtMenu('_sqlTtlCnxtMnu');  // header context menu
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
                    if($(this).is(':visible')) {
                        $(this).hide();
//                        self._grid.focus();   <- where should we return the focus ?
                    }
                })
                .appendTo(document.body);
            for(var m in this[_menu]) {
                if($.type(this[_menu][m]) === "string") {
                    $('<li>')
                        .attr("action", m)
                        .click(function(e) {
                            var self = $('#'+_menu).data('cnxt');
                            if(undefined != self) {
                                console.log('self title _cnxtMenu ' + self.options.title);
                                self[_menu].dom.hide();
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
        if($.isFunction(fun)) {
            fun();
        } else {
            throw('unimplimented fun '+funName+' for \''+_action+ '\' in '+_menu);
        }
    },

    /*
     * Renaming a view
     */
    _renameView: function() {
        if(("All ddViews" === this.options.title) || ("Remote Tables" === this.options.title)){
            alert_jq("Error: The ddView '" + this.options.title + "' may not be renamed");
        } else {
            var self = this;
            var viewId = self.options.viewId;
            prompt_jq({label: "ddView new name", content: '', value: self.options.title},
                function(viewName) {
                    if (viewName) {
                        if(viewId) {
                            console.log("saving " + viewId + " with name " + viewName);
                            var renameView = {view_op : {operation : "rename", view_id : viewId, newname : viewName}};
                            self.addWheel();
                            ajaxCall(self, 'view_op', renameView, 'view_op', 'opViewResult');
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
        } else {
            var self = this;
            var viewId = self.options.viewId;
            if(viewId) {
                var viewName = self.options.title;
                confirm_jq({title: "Confirm delete view " + viewName, content: ''},
                    function() {
                        console.log("deleting a view " + viewId + " with name " + viewName);
                        var delView = {view_op : {operation : "delete", view_id : viewId, newname : ""}};
                        self.addWheel();
                        ajaxCall(self, 'view_op', delView, 'view_op', 'opViewResult');
                    });
            } else {
                alert_jq("Error: \"" + this.options.title + "\" is not a view and therefore may not be deleted here!");
            }
        }
    },

    /*
     * Saving a table
     */
    _saveView: function() {
        var viewId = this._getOwnerViewId();
        if(viewId) {
            this._updateView(viewId, this._title);
        } else {
            this._saveViewWithName(this._title, false);
        }
    },

    _saveViewAs: function() {
        self = this;
        prompt_jq({label: "ddView name", content: ''},
            function(viewName) {
                if (viewName) {
                    self._saveViewWithName(viewName, false);
                }
            });
    },

    _getSaveStructure: function(viewName) {
        var self = this;
        return {
            save_view : {
                conn_id       : dderlState.connectionSelected.connection,
                name          : viewName,
                content       : self._modCmd
            }
        };
    },

    _updateView: function(viewId, viewName) {
        var self = this;
        var saveView = self._getSaveStructure(viewName);
        var updateView = {update_view : saveView.save_view};
        updateView.update_view.view_id = viewId;
        self.addWheel();
        ajaxCall(self, 'update_view', updateView, 'update_view', 'saveViewResult');
    },

    _saveViewWithName: function(viewName, replace) {
        var self = this;
        var saveView =  self._getSaveStructure(viewName);
        saveView.save_view.replace = replace;
        self.addWheel();
        ajaxCall(self, 'save_view', saveView, 'save_view', 'saveViewResult');
    },

    _getOwnerViewId: function() {
        if(null != this._cmdOwner && this._cmdOwner.hasClass('ui-dialog-content')) {
            return this._cmdOwner.table('getViewId');
        }
        return null;
    },

    _saveViewResult: function(_saveView) {
        var self = this;
        if(_saveView === "ok") {
            console.log('[AJAX] view saved!');
        } else if(_saveView.hasOwnProperty('view_id')) {
            console.log('[AJAX] view saved, id = ' + _saveView.view_id + ' and name ' + _saveView.name);
            self._setTitle(_saveView.name);
        } else if(_saveView.hasOwnProperty('need_replace')) {
            $('<div><p><span class="ui-icon ui-icon-alert" style="float: left; margin: 0 7px 20px 0;"></span>A view with that name already exists. Are you sure you want to replace it?</p></div>').dialog({
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

    _setupEventHandlers: function() {
        // make this as context to private event handler functions
        // and register for named events
        for(var fun in this._handlers) {
            this.element.on(fun, null, this, this._handlers[fun]);
        }
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

        self._footerWidth = self._addBtngrpToDiv(self._footerDiv);
    },

    _setTitle: function(newTitle) {
        var self = this;
        self._title = self.options.title = newTitle;
        var newTitleHtml = $('<span>').text(newTitle).addClass('table-title');
        self._dlg.dialog('option', 'title', newTitleHtml[0].outerHTML);
        self._dlg.dialog("widget").find(".table-title").click(function(e) {
            self._sqlTtlCnxtMnu.dom
                .css("top", e.clientY - 10)
                .css("left", e.clientX)
                .data('cnxt', self)
                .show();
        });
    },


      _setTitleHtml: function(newTitle) {
          var self = this;
          self._dlg.dialog('option', 'title', newTitle[0].outerHTML);
          self._dlg.dialog("widget").find(".table-title").click(function(e) {
              self._sqlTtlCnxtMnu.dom
                  .css("top", e.clientY - 10)
                  .css("left", e.clientX)
                  .data('cnxt', self)
                  .show();
          });
      },

      removeWheel : function()
      {
          this._spinCounter--;
          var $dlgTitleObj = $(this._dlg.dialog('option', 'title'));
          if(this._spinCounter <= 0
             && this._dlg.hasClass('ui-dialog-content')
             && $dlgTitleObj.hasClass('table-title-wait')) {
              this._setTitleHtml($dlgTitleObj.removeClass('table-title-wait'));
              this._spinCounter = 0;
          }
      },

      addWheel : function()
      {
          if(this._spinCounter < 0)
              this._spinCounter = 0;
          this._spinCounter++;
          var $dlgTitleObj = $(this._dlg.dialog('option', 'title'));
          if(this._spinCounter > 0
             && this._dlg.hasClass('ui-dialog-content')
             && !($dlgTitleObj.hasClass('table-title-wait'))) {
              this._setTitleHtml($dlgTitleObj.addClass('table-title-wait'));
          }
      },

    _addBtngrpToDiv: function(toolDiv) {
        var self = this;

        for(btnTxt in self._toolsBtns) {
            var elm = self._toolsBtns[btnTxt];

            var toolElmFn = function(e) {
                var self = e.data;
                var _btnTxt = $(this).text();
                var fName = self._toolsBtns[_btnTxt].clk;
                var f = $.proxy(self[fName], self);
                if($.isFunction(f)) {
                    f();
                } else {
                    throw('['+self.options.title+'] toolbar '+_btnTxt+' has unimplimented cb '+fName);
                }
            };

            var inph = self.options.toolBarHeight;
// TODO jQ 1.9 deprecated $.browser find work around
//            if($.browser.msie) inph -= 2;

            if (self._toolsBtns[btnTxt].typ === 'btn') {
                $('<button>')
                    .text(btnTxt)
                    .button({icons: {primary: 'fa  fa-' + elm.icn}, text: false})
                    .css('height', this.options.toolBarHeight+'px')
                    .click(self, toolElmFn)
                    .appendTo(toolDiv);
            } else if (self._toolsBtns[btnTxt].typ === 'sel') {
                var sel = $('<select>')
                    .width(100)
                    .css('margin', '0px 0px 0px 0px')
                    .addClass('ui-button ui-widget ui-state-default ui-button-text-only ui-corner-right')
                    .css('height', this.options.toolBarHeight+'px')
                    .css('text-align', 'left')
                    .appendTo(toolDiv);

                for(var i = 0; i < self._history.length; ++i) {
                    var optionToAdd = $('<option>')
                        .text(self._history[i])
                        .click(function (evt) {
                            evt.preventDefault();
                            self.showCmd($(this).text(), false);
                        });
                    sel.append(optionToAdd);
                }
                self._historySelect = sel;
            }
        }
        
        toolDiv
            .buttonset()
            .css('height', (self.options.toolBarHeight)+'px');

        // footer total width
        var childs = toolDiv.children();
        var totWidth = 0;
        for(var i = 0; i < childs.length; ++i) {
            totWidth += $(childs[i]).width();
        }
        return totWidth;
    },
  
    /*
     * Toolbar callbak functions
     */
    _toolBarValidate: function() {
        var self = this;
        self._addToHistory(self._modCmd);
        self.addWheel();
        ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr:self._modCmd}}, 'parse_stmt',
                function (parse_stmt) {
                    self._renderParsed(parse_stmt, false);
                    if (parse_stmt.hasOwnProperty("binds")) {
                        self._optBinds = self._mergeBinds(parse_stmt.binds, self._optBinds);
                        sql_params_dlg(self._paramsDiv, self._optBinds);
                        self._editDiv.tabs("option", "active", 3);
                        self._setTabFocus();
                    } else {
                        self._optBinds = null;
                    }
                });
    },

    _mergeBinds: function(src, dst) {
        if (dst == null || !dst.hasOwnProperty('types')
                || !dst.hasOwnProperty('pars')) {
            dst = src;
        } else {
            dst.types = src.types;
            
            // remove stale parameters
            for (p in dst.pars)
                if (!src.pars.hasOwnProperty(p))
                    delete dst.pars[p];

            // import new parameters and retain values of old parameters
            for (p in src.pars)
                if (dst.pars.hasOwnProperty(p)) {
                    dst.pars[p].typ = src.pars[p].typ;
                } else {
                    dst.pars[p] = {typ : src.pars[p].typ, val : src.pars[p].val};
                }
        }
        return dst;
    },

    _toolBarTblReload: function() {
        this._loadTable('>');
    },
    _toolBarTblFetch2End: function() {
        this._loadTable('>|');
    },
    _toolBarTblFetchNTail: function() {
        this._loadTable('>|...');
    },
    _toolBarTblTailOnly: function() {
        this._loadTable('...');
    },
    _toolBarChangeSql: function() {
    },

    _loadTable: function(button) {
        var self = this;
        self._reloadBtn = button;
        self._addToHistory(self._modCmd);
        self.addWheel();
        ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr:self._modCmd}}, 'parse_stmt',
                function (parse_stmt) {
                    if (self._optBinds != null) {
                        self._reloadParsedCmd(parse_stmt);
                    } else {
                        if (parse_stmt.hasOwnProperty("binds")) {
                            self._optBinds = self._mergeBinds(parse_stmt.binds, self._optBinds);
                            sql_params_dlg(self._paramsDiv, self._optBinds);
                            self._editDiv.tabs("option", "active", 3);
                            self._setTabFocus();
                        } else {
                            self._optBinds = null;
                            self._reloadParsedCmd(parse_stmt);
                        }
                    }
                });
    },

    _reloadParsedCmd: function(_parsed) {
        var self = this;
        self._renderParsed(_parsed, false);
        if(_parsed.hasOwnProperty("flat_list")) {
            self._pendingQueries = $.extend(true, {}, _parsed.flat_list); // deep copy
            self._execMultStmts();
        } else {
            if(self._cmdOwner && self._cmdOwner.hasClass('ui-dialog-content')) {
                self._modCmd = self._cmdFlat;
                self._cmdOwner.table('cmdReload', self._modCmd, self._optBinds, self._reloadBtn, self._getPlaneData());
            } else {
                self.addWheel();
                ajaxCall(self, 'query', {query: {
                    connection: dderlState.connection, qstr: self._modCmd, conn_id: dderlState.connectionSelected.connection,
                    binds: (self._optBinds != null && self._optBinds.hasOwnProperty('pars') ? self._optBinds.pars : null)
                }}, 'query', 'resultStmt');
                self._modCmd = self._cmdFlat;
            }
        }
    },
    
    _processResultStmt: function(resultQry, isMultiple) {
        var self = this;
        
        var initOptions = {
            autoOpen       : false,
            dderlConn      : dderlState.connection,
            dderlAdapter   : dderlState.adapter,
            dderlStartBtn  : this._reloadBtn,
            dderlCmdStrs   : this._history,
        };

        if(resultQry.hasOwnProperty('result') && resultQry.result === 'ok') {
            // Here we should write results to the operation logs
            if(isMultiple) {
                self._execMultStmts();
            }
        } else if(resultQry.hasOwnProperty('error')) {
            alert_jq(resultQry.error + "<br><br><b><center>" + self._pendingQueries.length + " statements not executed</center></b>");
        } else if(!resultQry.hasOwnProperty('statement')) {
            alert_jq('missing statement handle <br><br><b><center>' + self._pendingQueries.length + " statements not executed</center></b>");
        } else if(isMultiple) {
            initOptions.title = resultQry.qstr;
            $('<div>')
                .table(initOptions)
                .table('renderTable', resultQry);
            self._execMultStmts();
        } else {
            initOptions.dderlSqlEditor = this._dlg;
            initOptions.title = this._title;
            if(null === this._cmdOwner) {
                this._cmdOwner = $('<div>')
            }
            resultQry.qparams = self._optBinds;

            if(!resultQry.hasOwnProperty('table_layout')) {
                resultQry.table_layout = {};
            }
            $.extend(resultQry.table_layout, this._getPlaneData());
            
            this._cmdOwner
                .table(initOptions)
                .table('renderTable', resultQry);
        }
    },

    // TODO: This is restricted to one d3 script for now.
    _getPlaneData: function() {
        var self = this;
        planeToShow = 0;
        if(self._editDiv.tabs("option", "active") > 3) {
            planeToShow = 1;
        }
        return {
            plane_specs: [{
                script: self._graphEdit.val()
            }],
            plane_to_show: planeToShow
        };        
    },
      
    _resultStmt: function(resultQry) {
        this._processResultStmt(resultQry, false);
    },

    _resultMultStmt: function(resultQry) {
        this._processResultStmt(resultQry, true);
    },

    _execMultStmts: function() {
        var self = this;
        if(self._pendingQueries.length <= 0) {
            alert_jq("All statements executed");
        } else {
            var qstr = self._pendingQueries.shift();
            self.addWheel();
            ajaxCall(self, 'query', {query: {
                connection: dderlState.connection, qstr : qstr, conn_id: dderlState.connectionSelected.connection,
                binds: (self._optBinds != null && self._optBinds.hasOwnProperty('pars') ? self._optBinds.pars : null)
            }}, 'query', 'resultMultStmt');
        }
    },

    _addToHistory: function(sql) {
        var self = this;
        if(self._history.indexOf(sql) == -1) {
            self._history.unshift(sql);
            self._historySelect.prepend(
                $('<option>').text(sql)
                    .click(function (evt) {
                        evt.preventDefault();
                        self.showCmd($(this).text(), false);
                    }));
        }
    },

    _setTabFocus: function() {
        var self = this;
        var selected = self._editDiv.tabs("option", "active");

        switch(selected) {
            case 0:
                self._flatTb.focus();
                textBox = self._flatTb[0];
                textBox.selectionStart = textBox.selectionEnd = textBox.value.length;
                break;
            case 1:
                self._prettyTb.focus();
                textBox = self._prettyTb[0];
                textBox.selectionStart = textBox.selectionEnd = textBox.value.length;
                break;
            case 2:
                self._boxDiv.focus();
                break;
            case 3:
                self._paramsDiv.focus();
                break;
            default:
                break;
        }
    },

    ////////////////////////////

    /*
     * ajaxCall success handlers
     */
    _renderParsed: function(_parsed, skipFocus) {
        var boxResult, self = this;

        if(!skipFocus) {
            self._setTabFocus();
        }
        if(_parsed.hasOwnProperty('sqlbox')) {
            console.log(self._boxJson);
            self._boxJson = _parsed.sqlbox;
            boxResult = self._boxing(self._boxJson, self._boxDiv.width(), null, self._boxDiv[0]);
            self._boxDiv.html('');
            boxResult.div.appendTo(self._boxDiv);
        }
        if(_parsed.hasOwnProperty('flat')) {
            self._flatTb.val(_parsed.flat);
            self._cmdFlat = self._flatTb.val();
        }
        if(_parsed.hasOwnProperty('pretty')) {
            self._prettyTb.val(_parsed.pretty);
            self._cmdPretty = self._prettyTb.val();
            if(!self._cmdChanged) {
                self._cmdChanged = true;
                self._editDiv.tabs("option", "active", 1);
                if(!skipFocus) {
                    self._setTabFocus();
                }
                var nlines = _parsed.pretty.split("\n").length;
                var dialogPos = self._dlg.dialog("widget").position();
                var newDialogHeight = Math.min($(window).height() * 0.8, Math.round(nlines * 16.8) + 62);
                var distanceToBottom = $(window).height() - (dialogPos.top + newDialogHeight) - 30;

                if(distanceToBottom < 0) {
                    var newTop = dialogPos.top + distanceToBottom - 20;
                    var newPos = {
                        my: "left top",
                        at: "left+" + dialogPos.left + " top+" + newTop,
                        of: "#main-body",
                        collision : 'none'
                    };

                    // Override default dialog options.
                    self._dlg.dialog("option", "position", newPos);
                }
                self._dlg.dialog("option", "height", newDialogHeight);
            }
        }
        if(_parsed.hasOwnProperty('sqlTitle') && self._isDefaultTitle) {
            self._setTitle(_parsed.sqlTitle);
            self._isDefaultTitle = false;
        }
    },

    _leaf_box: function (bx, collapsed, nametxt, alltxt, children, maxwidth, parent) {
        var self = this;
        var edit = $('<textarea autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false">')
            .addClass('boxEdit')
            .width(maxwidth)
            .attr('rows', 2)
            .val(alltxt);
        var name = $('<span>')
            .addClass('boxName')
            .text(nametxt);
    
        bx.append(edit);
        bx.append(name);
        bx.data("oldText", alltxt);

        var childrendiv = null;
        if(children.length > 0) {
            childrendiv = $('<div>')
                .width(maxwidth)
                .addClass('boxChildren');
            bx.append(childrendiv);
        }
    
        for(var i = 0; i < children.length; ++i) {
            childrendiv.append(children[i]);
        }
    
        edit.dblclick(function(e) {
            e.preventDefault();
            e.stopPropagation();
            edit.css('display','none');
            if(bx.data("oldText") != edit.val()) {
                bx.data("oldText", edit.val());
                name.text(edit.val());
                nametxt = edit.val();
                alltxt = edit.val();
                // delete all the children.
                if(childrendiv) {
                    console.log(childrendiv);
                    childrendiv.remove();
                }
                if(parent) {
                    var updateFunc = parent.data("updateFunc");
                    updateFunc();
                }
            }
            name.css('display','inline');
            if(childrendiv) childrendiv.css('display','inline');
        });
    
        var dblClkFn = function(e) {
            e.preventDefault();
            e.stopPropagation();
            edit.css('display','inline');
            name.css('display','none');
            if(childrendiv) childrendiv.css('display','none');
        };
    
        if (collapsed) {
            edit.css('display','inline');
            name.css('display','none');
            if(childrendiv) childrendiv.css('display','none');
        }

        if (nametxt.length === 0) {
            bx.dblclick(dblClkFn);
        } else {
            name.dblclick(dblClkFn);
        }

        bx.data("updateFunc", function() {
            var childText;
            alltxt = nametxt;
            for(var i = 0; i < children.length; ++i) {
                childText = children[i].data("oldText");
                alltxt += (' ' + childText);
            }
            bx.data("oldText", alltxt);
            edit.val(alltxt);

            if(parent) {
                var updateFunc = parent.data("updateFunc");
                updateFunc();
            } else {
                // This is the root, update the cmd
                console.log("this is the root " + nametxt);
                console.log("the new text " + alltxt);
                console.log(self);
                self._modCmd = alltxt;
            }
        });

        return bx;
    },
    
    _boxing: function(box, maxwidth, parent, oldBox) {
        var children = new Array();
        var alltext = box.name;
        var allChildCollapsed = true;
        var res;
        var bx = $('<div>')
            .addClass('boxParent')
            .width(maxwidth);
        for (var i = 0; i < box.children.length; ++i) {
            res = this._boxing(box.children[i], maxwidth-20, bx, oldBox ? oldBox.children[i] : oldBox);
            children.push(res.div);
            alltext += (' ' + res.text);
            if (!box.children[i].collapsed) {
                allChildCollapsed = false;
            }
        }
        var collapsed = box.collapsed || (allChildCollapsed && box.name.length === 0);
        var myRes = {div : this._leaf_box(bx, collapsed, box.name, alltext, children, maxwidth-20, parent), text: alltext};
        return myRes;
    },

    _createDlg: function() {
        var self = this;                    

        // dlg width can't be less than footer width
        self.options.minWidth = self._footerWidth;
        self._dlg = self.element
            .dialog(self.options)
            .bind("dialogresizestop", function(event, ui) {
                self._refreshHistoryBoxSize();
            });

        // Update title to add context menu handlers.
        self._setTitle(self.options.title);
    },
 
    // translations to default dialog behavior
    open: function() {
        this._dlg.dialog("option", "position", {at : 'center center', my : 'center center', collision : 'none'});
        this._dlg.dialog("open").dialog("widget").draggable("option","containment","#main-body");
        if(this._cmdOwner !== null && this._cmdOwner.hasClass('ui-dialog-content')) {
            smartDialogPosition($("#main-body"), this._cmdOwner, this._dlg, ['center']);
        } else {
            // TODO: Here we maximize unused space
        }
        this._refreshHistoryBoxSize();
        this._setTabFocus();
        if (this._optBinds != null) {
            sql_params_dlg(this._paramsDiv, this._optBinds);
            this._editDiv.tabs("option", "active", 3);
            this._setTabFocus();
        }
    },

    setFlatCmd: function(cmd) {
        var self = this;
        self._modCmd = cmd;
        this._flatTb.val(cmd);
    },

    close: function() { this._dlg.dialog("close"); },

    showCmd: function(cmd, skipFocus) {
        var self = this;
        var callback = 'parsedCmd';
        self._modCmd = cmd;
        self._flatTb.val(cmd);
        if(skipFocus) {
            callback = 'parsedSkipFocus';
        }
        self.addWheel();
        ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr:cmd}}, 'parse_stmt', callback);
    },

    selHistorySelect: function(pos, sql) {
        var self = this;
        self._historySelect[0].options[pos].selected = true;
        self.showCmd(sql, true);
    },

    addToHistorySelect: function(sql) {
        var self = this;
        self._historySelect.prepend(
            $('<option>').text(sql)
                .click(function (evt) {
                    evt.preventDefault();
                    self.showCmd($(this).text(), false);
                })
        );
        self.selHistorySelect(0, sql);
    },

    // Use the destroy method to clean up any modifications your widget has made to the DOM
    _destroy: function() {
        this._editDiv.remove();
        this._footerDiv.remove();
        this.element.removeAttr('style class scrolltop scrollleft');
    },

  });
}( jQuery ) );

// $(document).ready(function() {    
//     var BOX =
//     {"ind":0,"name":"select","children":[
//         {"ind":1,"name":"","children":[
//             {"ind":2,"name":"c.owner","children":[],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":",","children":[],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":"v.name","children":[],"collapsed":false,"error":"","color":"black","pick":""}
//             ],"collapsed":false,"error":"","color":"black","pick":""},
//         {"ind":1,"name":"from","children":[
//             {"ind":2,"name":"ddView as v","children":[],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":",","children":[],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":"ddCmd as c","children":[],"collapsed":false,"error":"","color":"black","pick":""}
//             ],"collapsed":false,"error":"","color":"black","pick":""},
//         {"ind":1,"name":"where","children":[
//             {"ind":2,"name":"","children":[
//                 {"ind":3,"name":"c.id","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                 {"ind":3,"name":"=","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                 {"ind":3,"name":"v.cmd","children":[],"collapsed":true,"error":"","color":"black","pick":""}
//                 ],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":"and","children":[],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":"","children":[
//                 {"ind":3,"name":"c.adapters","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                 {"ind":3,"name":"=","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                 {"ind":3,"name":"\"[imem]\"","children":[],"collapsed":true,"error":"","color":"black","pick":""}
//                 ],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":"and","children":[],"collapsed":false,"error":"","color":"black","pick":""},
//             {"ind":2,"name":"","children":[
//                 {"ind":3,"name":"(","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                 {"ind":3,"name":"","children":[
//                     {"ind":4,"name":"","children":[
//                         {"ind":5,"name":"c.owner","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                         {"ind":5,"name":"=","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                         {"ind":5,"name":"user","children":[],"collapsed":true,"error":"","color":"black","pick":""}
//                         ],"collapsed":true,"error":"","color":"black","pick":""},
//                     {"ind":4,"name":"or","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                     {"ind":4,"name":"","children":[
//                         {"ind":5,"name":"c.owner","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                         {"ind":5,"name":"=","children":[],"collapsed":true,"error":"","color":"black","pick":""},
//                         {"ind":5,"name":"system","children":[],"collapsed":true,"error":"","color":"black","pick":""}
//                         ],"collapsed":true,"error":"","color":"black","pick":""}
//                     ],"collapsed":true,"error":"","color":"black","pick":""},
//                 {"ind":3,"name":")","children":[],"collapsed":true,"error":"","color":"black","pick":""}
//                 ],"collapsed":false,"error":"","color":"black","pick":""}
//             ],"collapsed":false,"error":"","color":"black","pick":""}
//         ],"collapsed":false,"error":"","color":"black","pick":""};
//     boxing(BOX).div.appendTo(document.body);
// });

