import $ from 'jquery';
import 'imports-loader?$=jquery,$.uiBackCompat=>false!jquery-ui/ui/widgets/tabs';
import {alert_jq, prompt_jq, confirm_jq} from '../dialogs/dialogs';
import {ajaxCall, dderlState, smartDialogPosition} from './dderl';
import {result_out_params, clear_out_fields, sql_params_dlg, all_out_params} from './dderl.sqlparams';
import {controlgroup_options} from '../jquery-ui-helper/helper.js';
import * as monaco from 'monaco-editor';

export function newSqlEditor(title = null, query = "", owner = null, history = [], optBinds = null, script = "", viewId = null, viewLayout = null, columnLayout = null, autoExec = false) {
    return $('<div>')
        .appendTo(document.body)
        .sql({
            autoOpen: false,
            title: title,
            cmdOwner: owner,
            query: query,
            history: history,
            optBinds: optBinds,
            script: script,
            viewId: viewId,
            viewLayout: viewLayout,
            columnLayout: columnLayout,
            autoExec: autoExec
        })
        .sql('open');
}

const tabPositions = Object.freeze({
    QUERY: 0,
    PARAMS: 1,
    GRAPH: 2
});

(function() {
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

    _queryTb        : null,
    _paramsDiv      : null,
    _graphTb        : null,

    _script         : "",
    _history        : null,
    _historySelect  : null,
    _cmdChanged     : false,
    _reloadBtn      : null,
    _spinCounter    : 0,
    _pendingQueries : null,
    _optBinds       : null,
    _outParamInputs : null,
    _viewLayout     : null,
    _runGraph       : false,
    _isFlatOnly     : false,

    // private event handlers
    _handlers       : { parsedCmd       : function(e, _parsed) { e.data._renderParsed      (_parsed, false); },
                        parsedSkipFocus : function(e, _parsed) { e.data._renderParsed      (_parsed, true); },
                        reloadParsedCmd : function(e, _parsed) { e.data._reloadParsedCmd   (_parsed); },
                        opViewResult    : function(e, _result) { e.data._operateViewResult (_result); },
                        saveViewResult  : function(e, _result) { e.data._saveViewResult    (_result); },
                        resultMultStmt  : function(e, _result) { e.data._resultMultStmt    (_result); },
                        resultStmt      : function(e, _result) { e.data._resultStmt        (_result); },
                        listTemplates   : function(e, _result) { e.data._listTemplates     (_result); }
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
                       'Execute fetch to end'       : { typ : 'btn', icn : 'step-forward',          clk : '_toolBarTblFetch2End'    },
                       'Execute fetch tail mode'    : { typ : 'btn', icn : 'step-forward ellipsis', clk : '_toolBarTblFetchNTail'   },
                       'Execute tail mode only'     : { typ : 'btn', icn : 'fetch-only ellipsis',   clk : '_toolBarTblTailOnly'     },
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
        open            : function() {},
        focus           : function() {},
        close           : function() {
                            $(this).dialog('destroy');
                            $(this).remove();
                          },
        query           : "",
        script          : "",
        cmdOwner        : null,
        optBinds        : null,
        history         : [],
        viewId          : null,
        viewLayout      : null,
        columnLayout    : null,
        autoExec        : false
    },

    _getToolbarSelectWidth: function() {
        // 10 pixels for resize handler
        return this._footerDiv.width() - this._getToolbarButtonsWidth() - 10;
    },

    _getToolbarButtonsWidth: function() {
        let childs = this._footerDiv.children();
        let btnWidth = 0;
        for(let i = 0; i < childs.length - 1; ++i) {
            btnWidth += Math.max(childs[i].scrollWidth, childs[i].offsetWidth, childs[i].clientWidth);
        }
        return btnWidth;
    },

    _refreshHistoryBoxSize: function() {
        var self = this;
        var w = self._getToolbarSelectWidth();
        self._historySelect.css('width', w);
    },

    // Set up the widget
    _create: function() {
        var self = this;

        self._history = [];
        self._pendingQueries = [];
        self._outParamInputs = [];

        self._fnt = $(document.body).css('font-family');
        self._fntSz = $(document.body).css('font-size');

        // preserve some options
        if(self.options.cmdOwner    !== self._cmdOwner)     self._cmdOwner  = self.options.cmdOwner;
        if(self.options.script      !== self._script)       self._script    = self.options.script;
        if(self.options.optBinds    !== self._optBinds)     self._optBinds  = self.options.optBinds;
        if(self.options.history     !== self._history)      self._history   = self.options.history;
        if(self.options.viewLayout  !== self._viewLayout)   self._viewLayout= self.options.viewLayout;
        if(self.options.title       !== self._title) {
            if(self.options.title === null) {
                self.options.title = 'Query'+DEFAULT_COUNTER+'.sql';
                ++DEFAULT_COUNTER;
                self._isDefaultTitle = true;
            }
            self._title = self.options.title;
        }

        // dialog elements

        // Set the size to 40% of the parent window
        self.options.width = $(window).width() * 0.4;
        self.options.height = $(window).height() * 0.4;

        // Create container
        var tabContainer = document.createElement('div');
        tabContainer.id = 'tabquery';

        // Creating a monaco editor
        var queryContainer = document.createElement('div');
        queryContainer.className = 'monaco-query-container';
        // Add container to the tab
        tabContainer.appendChild(queryContainer);
        self._queryTb = monaco.editor.create(queryContainer, {
            value: self.options.query,
            language: 'sql',
            scrollBeyondLastLine: false
        });

        self._queryTb.onMouseDown((e) => {
            console.log("mouse down", e);
        });

        // Flat only checkbox flag
        // TODO: Add toolbox for formatting and helper actions.
        var flatSelectionSpan = document.createElement('span');
        flatSelectionSpan.className = 'flat-only-select';

        var flatCheckbox = document.createElement('input');
        flatCheckbox.type = 'checkbox';
        flatCheckbox.onchange = () => {
            self._isFlatOnly = flatCheckbox.checked;
            self.addWheel();
            ajaxCall(self, 'parse_stmt', { parse_stmt: { qstr: self._queryTb.getValue() } }, 'parse_stmt', 'parsedCmd');
        };
        flatSelectionSpan.appendChild(flatCheckbox);
        flatSelectionSpan.appendChild(document.createTextNode('flat only'));
        tabContainer.appendChild(flatSelectionSpan);
        
        self._paramsDiv = $('<div>').css("display", "inline-block;");

        var graphContainer = document.createElement('div');
        graphContainer.id = 'tabgraph';

        self._graphTb = monaco.editor.create(graphContainer, {
            value: getDefaultScript(self._script),
            language: 'javascript',
            scrollBeyondLastLine: false
        });

        var queryBg    = 'rgb(255,240,240)';
        var paramsBg    = '#FFFFD1';

        var titleHeight = 26; // Default height it is recalculated later...
        var ulTabs = $('<ul>');
        var queryTabTitle = $('<li style="background:'+queryBg+'"><a href="#tabquery">Sql</a></li>');
        var paramsTabTitle = $('<li style="background:'+paramsBg+'"><a href="#tabparams">Params</a></li>');
        var graphTabTitle = $('<li><a href="#tabgraph">D3 Graph</a></li>');

        ulTabs
            .append(queryTabTitle)
            .append(paramsTabTitle)
            .append(graphTabTitle);

        self._editDiv =
            $('<div>')
            .append(ulTabs)
            .append(tabContainer)
            .append(
              $('<div>')
              .css('background-color', paramsBg)
              .css("overflow-x", "hidden")
              .css("overflow-y", "auto")
              .attr('id','tabparams')
              .append(self._paramsDiv)
            )
            .append(graphContainer)
            .css('position', 'absolute')
            .css('overflow', 'hidden')
            .css('top', '0')
            .css('left', '0')
            .css('right', '0')
            .css('bottom', self.options.toolBarHeight+'px')
            .tabs()
            .on("tabsactivate", function() { // function(event, ui)
                var selected = self._editDiv.tabs("option", "active");

                if(selected === tabPositions.PARAMS) {
                } else {
                    self._runGraph = (selected > tabPositions.PARAMS);
                    if(self._runGraph) {
                        graphTabTitle.addClass("sql-tab-highlight");
                        queryTabTitle.removeClass("sql-tab-highlight");
                        self._graphTb.layout();
                    } else {
                        queryTabTitle.addClass("sql-tab-highlight");
                        graphTabTitle.removeClass("sql-tab-highlight");
                        self._queryTb.layout();
                    }
                    self._setTabFocus();
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

        titleHeight = tabTitles.height();
        $('#tabquery, #tabparams, #tabgraph').css('top', titleHeight+'px');

        // toolbar container
        self._footerDiv = $('<div>').appendTo(self.element);

        // populate the graph dropdown with the template names
        ajaxCall(self, 'list_d3_templates', {}, 'list_d3_templates', 'listTemplates');

        // need the max footer with to set as dlg minWidth
        self._createDlgFooter();
        self._createDlg();
        // TODO: This has to be replaced for monaco actions.
        self._addKeyEventHandlers();
        self._createContextMenus();

        // setting up the event handlers last to aid debugging
        self._setupEventHandlers();
    },

    _listTemplates: function(templateList) {
        // Reference to objects we need.
        var historySelect = this._historySelect[0];
        var footerDiv = this._footerDiv[0];

        console.log("The templates found", templateList);
        var templates = document.createElement('select');
        templates.className = 'ui-button ui-widget ui-state-default ui-button-text-only ui-corner-right';
        templates.style.width = this._getToolbarSelectWidth() + 'px';
        templates.style.margin = 0;
        templates.style.height = this.options.toolBarHeight + 'px';
        templates.style.textAlign = 'left';
        templateList.forEach(function(t) {
            let text = t.application + " - " + t.name;
            templates.appendChild(new Option(text, JSON.stringify(t)));
        });
        templates.selectedIndex = -1;

        // We load the content on demand, but keep it cached on the client.
        var templatesContent = {};

        templates.onchange = (evt) => {
            var value = JSON.parse(evt.target.value);
            var text = value.application + " - " + value.name;
            if(templatesContent.hasOwnProperty(text)) {
                console.log("content cached found for", text);
                this._graphTb.setValue(templatesContent[text]);
            } else {
                ajaxCall(null, "get_d3_template", {get_d3_template: value}, 'get_d3_template', (content) => {
                    console.log("Content requested from server for", text);
                    templatesContent[text] = content;
                    this._graphTb.setValue(templatesContent[text]);
                });
            }
        };

        this._dlg.on("dialogresizestop", () => {
            templates.style.width = this._getToolbarSelectWidth() + 'px';
        });

        this._editDiv.on("tabsactivate", () => {
            var selected = this._editDiv.tabs("option", "active");
            console.log("Selected tab", selected);
            if(selected <= tabPositions.PARAMS) {
                if(historySelect.parentNode) { return; }
                footerDiv.removeChild(templates);
                footerDiv.appendChild(historySelect);
            } else {
                if(templates.parentNode) { return; }
                footerDiv.removeChild(historySelect);
                footerDiv.appendChild(templates);
                templates.selectedIndex = -1;
            }
        });
    },

    _init: function() {
        var self = this;

        // default dialog open behavior
        if (self.options.autoOpen) {
            self._dlg.dialog("open");
        }

        if (self.options.query) {
            self.addWheel();
            ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr : self.options.query}}, 'parse_stmt', 'parsedCmd');
        }
    },

    _createContextMenus: function() {
        this._cnxtMenu('_sqlTtlCnxtMnu');  // header context menu
    },

    // create the context menu and add them to document.body
    // only if they do not exist
    // TODO: Create a context menu once per table instead of the global
    //       to allow dynamic menu options depending on the column content.
    _cnxtMenu: function(_menu) {
        function leaveMenuHandler(e) {
            e.preventDefault();
            if($(this).is(':visible')) {
                $(this).hide();
            }
        }

        function clickMenuHandler() {
            var self = $('#' + _menu).data('cnxt');
            if(self) {
                console.log('self title _cnxtMenu ' + self.options.title);
                self[_menu].dom.hide();
                self._cnxtMenuAction(_menu, $(this).attr("action"));
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

        if(!viewId) {
            viewId = this.options.viewId;
        }

        if(viewId) {
            this._updateView(viewId, this._title);
        } else {
            this._saveViewWithName(this._title, false);
        }
    },

    _saveViewAs: function() {
        var self = this;
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
                content       : self._queryTb.getValue(),
                table_layout  : this._getLayout()
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
        if(this._cmdOwner && this._cmdOwner.hasClass('ui-dialog-content')) {
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

    // TODO: Replace this using monaco actions.
    _addKeyEventHandlers: function() {
        var self = this;
        this.element.keydown(function(e) {
            var c = e.keyCode;
            var ctrlDown = e.ctrlKey || e.metaKey;
            // Check for ctrl+enter
            if(ctrlDown && c === 13) {
                console.log("ctrl+enter detected");
                self._toolBarTblReload();
            }
        });
    },

    _setTitle: function(newTitle) {
        var self = this;
        self._title = self.options.title = newTitle;
        var newTitleHtml = $('<span>').text(newTitle).addClass('table-title');
        self._setTitleHtml(newTitleHtml);
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

      removeWheel : function() {
          this._spinCounter--;
          var $dlgTitleObj = $(this._dlg.dialog('option', 'title'));
          if(this._spinCounter <= 0 && this._dlg.hasClass('ui-dialog-content') &&
            $dlgTitleObj.hasClass('table-title-wait')) {
              this._setTitleHtml($dlgTitleObj.removeClass('table-title-wait'));
              this._spinCounter = 0;
          }
      },

      addWheel : function()
      {
          if(this._spinCounter < 0) { this._spinCounter = 0; }
          this._spinCounter++;
          var $dlgTitleObj = $(this._dlg.dialog('option', 'title'));
          if(this._spinCounter > 0 && this._dlg.hasClass('ui-dialog-content') &&
            !($dlgTitleObj.hasClass('table-title-wait'))) {
              this._setTitleHtml($dlgTitleObj.addClass('table-title-wait'));
          }
      },

    _addBtngrpToDiv: function(toolDiv) {
        var self = this;

        var toolElmFn = function(e) {
            var self = e.data;
            var fName = $(this).data('fName');
            var f = $.proxy(self[fName], self);
            if($.isFunction(f)) {
                f();
            } else {
                throw ('[' + self.options.title + '] toolbar ' + $(this).text() + ' has unimplimented cb ' + fName);
            }
        };

        for(let btnTxt in self._toolsBtns) {
            var elm = self._toolsBtns[btnTxt];
            if (self._toolsBtns[btnTxt].typ === 'btn') {
                $('<button>')
                    .text(btnTxt)
                    .data('fName', self._toolsBtns[btnTxt].clk)
                    .button({icon: 'fa fa-' + elm.icn, showLabel: false})
                    .css('height', this.options.toolBarHeight+'px')
                    .addClass('colorIcon')
                    .click(self, toolElmFn)
                    .appendTo(toolDiv);
            }
        }

        // This is not dynamic as we only support one select for
        // the history anyways
        var sel = $('<select>')
            .width(100)
            .css('margin', '0px 0px 0px 0px')
            .addClass('ui-button ui-widget ui-state-default ui-button-text-only ui-corner-right')
            .css('height', this.options.toolBarHeight + 'px')
            .css('text-align', 'left')
            .appendTo(toolDiv);

        for(let i = 0; i < self._history.length; ++i) {
            sel.append($('<option>').text(self._history[i]));
        }
        sel.change(function(evt) {
            evt.preventDefault();
            self.showCmd(sel.val(), false);
        });

        self._historySelect = sel;

        toolDiv
            .controlgroup(controlgroup_options())
            .css('height', (self.options.toolBarHeight)+'px');

        // footer total width
        var totWidth = self._getToolbarButtonsWidth() + sel.width();
        return totWidth;
    },

    /*
     * Toolbar callbak functions
     */
    _toolBarValidate: function() {
        var self = this;
        var query = self._queryTb.getValue();
        self._addToHistory(query);
        self.addWheel();
        ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr: query}}, 'parse_stmt',
                function (parse_stmt) {
                    self._renderParsed(parse_stmt, false);
                    if (parse_stmt.hasOwnProperty("binds")) {
                        self._optBinds = self._mergeBinds(parse_stmt.binds, self._optBinds);
                        sql_params_dlg(self._paramsDiv, self._optBinds, self._outParamInputs);
                        self._editDiv.tabs("option", "active", tabPositions.PARAMS);
                        self._setTabFocus();
                    } else {
                        self._optBinds = null;
                    }
                });
    },

    _mergeBinds: function(src, dst) {
        if(!dst || !dst.hasOwnProperty('types') || !dst.hasOwnProperty('pars')) {
            dst = src;
        } else {
            dst.types = src.types;

            // remove stale parameters
            for(let p in dst.pars) {
                if(!src.pars.hasOwnProperty(p)) {
                    delete dst.pars[p];
                }
            }
            // import new parameters and retain values of old parameters
            for(let p in src.pars)
                if(dst.pars.hasOwnProperty(p)) {
                    dst.pars[p].typ = src.pars[p].typ;
                    dst.pars[p].dir = src.pars[p].dir;
                } else {
                    dst.pars[p] = { typ: src.pars[p].typ,
                                    dir: src.pars[p].dir,
                                    val: src.pars[p].val };
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
        var query = self._queryTb.getValue();
        self._reloadBtn = button;
        self._addToHistory(query);
        self.addWheel();
        ajaxCall(self, 'parse_stmt', {parse_stmt: {qstr : query}}, 'parse_stmt',
                function (parse_stmt) {
                    if (self._optBinds !== null) {
                        self._reloadParsedCmd(parse_stmt);
                    } else {
                        if (parse_stmt.hasOwnProperty("binds")) {
                            self._optBinds = self._mergeBinds(parse_stmt.binds, self._optBinds);
                            sql_params_dlg(self._paramsDiv, self._optBinds,  self._outParamInputs);
                            self._editDiv.tabs("option", "active", tabPositions.PARAMS);
                            self._cmdChanged = true;
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
            self._pendingQueries = $.extend(true, [], _parsed.flat_list); // deep copy
            self._execMultStmts();
        } else {
            clear_out_fields(self._outParamInputs);
            var params = null;
            if(self._optBinds !== null && self._optBinds.hasOwnProperty('pars')) {
                params = self._optBinds.pars;
                for (let p in params) {
                    let param = params[p];
                    if(param.dir === "out") {
                        param.val = "";
                    }
                }
            }
            if(self._cmdOwner && self._cmdOwner.hasClass('ui-dialog-content')) {
                self._cmdOwner.table('cmdReload', self._queryTb.getValue(), self._optBinds, self._reloadBtn, self._getPlaneData());
            } else {
                self.addWheel();
                ajaxCall(self, 'query', {query: {
                    connection: dderlState.connection,
                    qstr: self._queryTb.getValue(),
                    conn_id: dderlState.connectionSelected.connection,
                    binds: params
                }}, 'query', 'resultStmt');
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
            dderlCmdStrs   : this._history
        };

        if(resultQry.hasOwnProperty('result') && resultQry.result === 'ok') {
            // Here we should write results to the operation logs
            if(isMultiple) {
                self._execMultStmts();
            }
        } else if(resultQry.hasOwnProperty('error')) {
            alert_jq(resultQry.error + "<br><br><b><center>" + self._pendingQueries.length + " statements not executed</center></b>");
        } else if(resultQry.hasOwnProperty('data')) {
            result_out_params(resultQry.data, self._outParamInputs);
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
            initOptions.dderlViewId = this.options.viewId;
            initOptions.dderlClmlay = this.options.columnLayout;

            if(null === this._cmdOwner) {
                this._cmdOwner = $('<div>');
            }
            resultQry.qparams = self._optBinds;

            if(!resultQry.hasOwnProperty('table_layout')) {
                resultQry.table_layout = this._getLayout();
            } else {
                $.extend(resultQry.table_layout, this._getPlaneData());
            }

            this._cmdOwner
                .table(initOptions)
                .table('renderTable', resultQry);
        }
    },

    _getLayout: function() {
        if(this._cmdOwner && this._cmdOwner.hasClass('ui-dialog-content')) {
            return $.extend(this._cmdOwner.table('getTableLayout'), this._getPlaneData());
        } else {
            if(!this._viewLayout) {
                return this._getPlaneData();
            }
            return $.extend(this._viewLayout, this._getPlaneData());
        }
    },

    // TODO: This is restricted to one d3 script for now.
    _getPlaneData: function() {
        var self = this;
        var planeToShow = 0;
        var script = self._graphTb.getValue();
        if(self._runGraph) {
            // TODO: Fix this as the plane_spec has to contain all definitions...
            planeToShow = 1;
        } else if(script == getDefaultScript()) {
            console.log("not saving script as it is the template");
            script = "";
        }
        return {
            plane_specs: [{
                script: script
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
                binds: (self._optBinds && self._optBinds.hasOwnProperty('pars') ? self._optBinds.pars : null)
            }}, 'query', 'resultMultStmt');
        }
    },

    _addToHistory: function(sql) {
        var self = this;
        if(self._history.indexOf(sql) == -1) {
            self._history.unshift(sql);
            self._historySelect.prepend($('<option>').text(sql));
        }
    },

    _setTabFocus: function() {
        var self = this;
        var selected = self._editDiv.tabs("option", "active");

        switch(selected) {
            case tabPositions.QUERY:
                self._queryTb.focus();
                break;
            case tabPositions.PARAMS:
                self._paramsDiv.focus();
                break;
            case tabPositions.GRAPH:
                self._graphTb.focus();
                break;
        }
    },

    ////////////////////////////

    /*
     * ajaxCall success handlers
     */
    _renderParsed: function(_parsed, skipFocus) {
        var self = this;

        if(_parsed.hasOwnProperty('pretty') && !self._isFlatOnly) {
            self._queryTb.setValue(_parsed.pretty);
            if(!self._cmdChanged) {
                self._cmdChanged = true;
                self._editDiv.tabs("option", "active", tabPositions.SQL);
                var dialogPos = self._dlg.dialog("widget").position();
                var newDialogHeight = Math.min($(window).height() * 0.8, self._queryTb.getScrollHeight() + 80); // +75 for header and footer 5 for extra margin.
                var distanceToBottom = $(window).height() - (dialogPos.top + newDialogHeight) - 30;

                var left = dialogPos.left;
                if(left < 0) { left = 0; }

                if(distanceToBottom < 0) {
                    var newTop = dialogPos.top + distanceToBottom - 20;
                    var newPos = {
                        my: "left top",
                        at: "left+" + left + " top+" + newTop,
                        of: "#main-body",
                        collision : 'none'
                    };

                    // Override default dialog options.
                    self._dlg.dialog("option", "position", newPos);
                }
                self._dlg.dialog("option", "height", newDialogHeight);
                self._queryTb.layout();
            }
        } else if(_parsed.hasOwnProperty('flat')) {
            self._queryTb.setValue(_parsed.flat);
        }

        if(_parsed.hasOwnProperty('sqlTitle') && self._isDefaultTitle) {
            self._setTitle(_parsed.sqlTitle);
            self._isDefaultTitle = false;
        }
        if(!skipFocus) { self._setTabFocus(); }
    },

    _createDlg: function() {
        var self = this;

        // dlg width can't be less than footer width
        self.options.minWidth = self._footerWidth;
        self._dlg = self.element
            .dialog(self.options)
            .bind("dialogresizestop", function() {
                self._refreshHistoryBoxSize();
                console.log("calling the layout...");
                self._graphTb.layout();
                self._queryTb.layout();
            });

        // Update title to add context menu handlers.
        self._setTitle(self.options.title);
    },

    // traslations to default dialog behavior
    open: function() {
        this._dlg.dialog("option", "position", {at : 'center center', my : 'center center', of: '#main-body', collision : 'none'});
        this._dlg.dialog("open").dialog("widget").draggable("option","containment","#main-body");
        if(this._cmdOwner !== null && this._cmdOwner.hasClass('ui-dialog-content')) {
            smartDialogPosition($("#main-body"), this._cmdOwner, this._dlg, ['center']);
        } else {
            // TODO: Here we maximize unused space
        }
        this._refreshHistoryBoxSize();
        if (this._optBinds !== null) {
            sql_params_dlg(this._paramsDiv, this._optBinds, this._outParamInputs);
            this._editDiv.tabs("option", "active", tabPositions.PARAMS);
            this._cmdChanged = true;
            //Run the query if there are only outparams
            if(all_out_params(this._optBinds.pars) && this.options.autoExec) {
                this._toolBarTblReload();
            }
        }
        // TODO: Maybe layout refresh call should go to tabfocus ?
        console.log("calling the layout...");
        this._graphTb.layout();
        this._queryTb.layout();
        this._setTabFocus();
    },

    close: function() { this._dlg.dialog("close"); },

    showCmd: function(cmd, skipFocus) {
        var self = this;
        var callback = 'parsedCmd';
        self._queryTb.setValue(cmd);
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
        self._historySelect.prepend($('<option>').text(sql));
        self.selHistorySelect(0, sql);
    },

    // Use the destroy method to clean up any modifications your widget has made to the DOM
    _destroy: function() {
        this._editDiv.remove();
        this._footerDiv.remove();
        this.element.removeAttr('style class scrolltop scrollleft');
        this.element.off("keydown");
    }

  });
}());

function getDefaultScript(currentScript) {
    if(currentScript) { return currentScript; }

    var graphScriptHelp = `function initGraph(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are (container, width, height)

    // container: d3 selection of the contaner div for the graph
    // width: width of the container
    // height: height of the container

    // The function must then return an object with the following callbacks:

    var svg = container
        .append('svg')
        .attr('width', width)
        .attr('height', height)
        .style('background-color', 'antiquewhite');

    return {
        on_data: function(data) {
            // Process data and add draw here.
            console.log("the new data arrived", data);
            var points = svg
                .selectAll('circle')
                .data(data, function(d) { return d.id; })
                .enter()
                .append('circle');

            points
                .attr('r', function(d) { return 3; })
                .attr('cx', function(d) { return d.id * 5; })
                .attr('cy', 60);
        },
        on_resize: function(w, h) {
            // Apply transformations and scale if when the dialog is resized.
            svg.attr('width', w)
                .attr('height', h);
        },
        on_reset: function() {
            // Called when the button clear the graph is clicked.
            svg.selectAll('svg > *').remove();
        },
        on_close: function() {
            // This should cleanup event listeners and element added
            // outside the container, the container itself will be removed
            // after this function call.
        }
    };
}`;

    return graphScriptHelp;
}
