var OpsBufEnum = { APPEND  : 1
                 , PREPEND : 2
                 , REPLACE : 3
                 };

var OpsFetchEnum = { NEXT     :1
                   , PREVIOUS :2
                   , JUMPNEXT :3
                   , JUMPPREV :4
                   , TOEND    :5
                   , TOBEGIN  :6
                   , RELOAD   :7
                   };

if(Object.hasOwnProperty('freeze')) {
    Object.freeze(OpsBufEnum);
    Object.freeze(OpsFetchEnum);
}

String.prototype.visualLength = function()
{
    var ruler = $('#txtlen');
    ruler.html(''+this);
    return ruler.width();
}

function getUniqueTime() {
  var time = new Date().getTime();
  while (time == new Date().getTime());
  return new Date().getTime();
}

var session = null;
var adapter = null;
var connection = null;

// generic dderlserver call interface
// TODO: currently the widget and non-widget
//       is determined by the presence of the
//       context variable for widget there is
//       no this['context']
function ajaxCall(_ref,_url,_data,_resphead,_successevt) {
    var self = _ref;

    // if data is JSON object format to string
    if(_data == null) _data = JSON.stringify({});
    else
        try {
            _data = JSON.stringify(_data);
        } catch (ex) {
            console.error(_data + ' is not JSON');
            throw(ex);
        }

    console.log('[AJAX] TX '+_url);

    var headers = new Object();
    if (adapter != null) headers['adapter'] = adapter;
    headers['dderl_sess'] = (session != null ? ''+session : '');
    if (null != self) {
        if(self.hasOwnProperty('_session')) headers['dderl_sess'] = self._session;
        if(self.hasOwnProperty('_adapter')) headers['adapter'] = self._adapter;
    }

    $.ajax({
        type: 'POST',
        url: _url,
        data: _data,
        dataType: "JSON",
        contentType: "application/json; charset=utf-8",
        headers: headers,
        context: self,

        success: function(_data, textStatus, request)
        {
            console.log('Requst '+_url+' Result '+textStatus);
            
            // dderl_sess is saved in global var session and (optionally)
            // in the dderlSession and _session property of the widget
            var s = request.getResponseHeader('dderl_sess');
            if(s != null)
                session = s;

            if(!this.hasOwnProperty('context'))
                this.options.dderlSession = this._session = session;

            if(!_data)
                throw('null data received for the request '+_url);
            else if(_data.hasOwnProperty(_resphead)) {
                console.log('[AJAX] RX '+_resphead);
                if(this.hasOwnProperty('context') && null == this.context) {
                    if(null === _successevt)
                        console.log('no success callback for '+_url);
                    else if($.isFunction(_successevt))
                        _successevt(_data[_resphead]);
                    else
                        throw('unsupported success event '+_successevt+' for '+_url);
                } else {
                    if(this._handlers.hasOwnProperty(_successevt))
                        this.element.trigger(_successevt, _data[_resphead]);
                    else
                        throw('unsupported success event '+_successevt+' for '+_url);
                }
            }
            else if(_data.hasOwnProperty('error')) {
                alert_jq('Error : '+_data.error);
            }
            else throw('resp '+_resphead+' doesn\'t match the request '+_url);
        },

        error: function (request, textStatus, errorThrown) {
             alert_jq('HTTP Error'+
                   (textStatus.length > 0 ? ' '+textStatus:'')+
                   (errorThrown.length > 0 ? ' details '+errorThrown:''));
        }
    });
}

function show_qry_files()
{
    $('<div>')
    .appendTo(document.body)
    .table({
        autoOpen    : false,
        dderlConn   : connection,
        dderlSession: session,
        dderlAdapter: adapter,
    })
    .table('loadViews');
}

function alert_jq(string)
{
    var dlgDiv =
        $('<div id="dialog-message" title="DDerl message"></div>')
        .appendTo(document.body)
        .append('<p><span class="ui-icon ui-icon-info" style="float: left; margin: 0 7px 50px 0;"></span>'+string+'</p>')
        .dialog({
            modal:false,
            width: 300,
            height: 300,
            open: function() {
                $(this).dialog("widget").appendTo("#main-body");
            },
            close: function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        }).dialog("widget").draggable("option","containment","#main-body");
}

function create_ws(url)
{
    var ws = new WebSocket(url);
    ws.onopen = function(){
        console.log('WebSocket: opened');
        ws.send(JSON.stringify({time : ""}));
    };
    ws.onclose = function(){
        console.log('WebSocket: closed');
        create_ws(url, url);
    };
    ws.onmessage = function(e) {
        $('#server-time').text(e.data);
    };
}

function edit_table()
{
    context = $('#tbl-opts').data('data');
    edit_sql(context.tblDlg, context.content);
}

function save_table()
{
    context = $('#tbl-opts').data('data');
    qStr = context.content.replace(/(\r\n|\n|\r)/gm," ");
    var colnamesizes = new Array();
    var cols = context.grid.getColumns();
    // Column names and width
    for(var idx = 0; idx < cols.length; ++idx)
        if(cols[idx].name.length > 0)
            colnamesizes[colnamesizes.length] = {name: cols[idx].name, width: cols[idx].width};
    // Table width/height/position
    var w = context.tblDlg.width();
    var h = context.tblDlg.height();
    var x = context.tblDlg.dialog('widget').position().left;
    var y = context.tblDlg.dialog('widget').position().top;
    var saveView = {save_view : {table_layout : {width : w,
                                                height : h,
                                                     y : y,
                                                     x : x},
                                column_layout : colnamesizes,
                                         name : context.name,
                                      content : qStr}
                   };
    ajaxCall(null,'/app/save_view',saveView,'save_view', function(data) {
        if (data != "ok") {
            alert_jq(data);
        }
    });
}

function save_as_table()
{
    context = $('#tbl-opts').data('data');
    qStr = context.content.replace(/(\r\n|\n|\r)/gm," ");
    undefinedTableIdx = 0;
    $('<div><input type="text" value="'+context.name+'"/></div>')
    .appendTo(document.body)
    .dialog({
        autoOpen: false,
        height: 105,
        width: 'auto',
        modal: true,
        resizable: false,
        title: "Save SQL as",
        close: function() {
            $(this).dialog('destroy');
            $(this).remove();
        },
        buttons: {
            "Save": function() {
                var fileName = $(this).children('input').val();
                ajaxCall(null,'/app/save_file',{save: {file_name:fileName, file_content:qStr}},'save_file', null);
                $(this).dialog('close');
            }
        }
    }).dialog("open");
}

function beep()
{
    var beepStorage = sessionStorage.getItem("beep-sound");
    var beep = $("#beep-sound")[0];
    if (beepStorage) {
        // Reuse existing Data URL from sessionStorage
        beep.setAttribute("src", beepStorage);
    }
    else {
        // Create XHR and FileReader objects
        var xhr = new XMLHttpRequest();
        var fileReader = new FileReader();

        xhr.open("GET", beep.currentSrc, true);
        // Set the responseType to blob
        xhr.responseType = "blob";

        xhr.addEventListener("load", function () {
            if (xhr.status === 200) {
                // onload needed since Google Chrome doesn't support addEventListener for FileReader
                fileReader.onload = function (evt) {
                    // Read out file contents as a Data URL
                    var result = evt.target.result;
                    beep.setAttribute("src", result);
                    // Store Data URL in sessionStorage
                    try {
                        sessionStorage.setItem("beep-sound", result);
                    }
                    catch (e) {
                        console.log("Storage failed: " + e);
                    }
                };
                // Load blob as Data URL
                fileReader.readAsDataURL(xhr.response);
            }
        }, false);
        // Send XHR
        xhr.send();
    }
    beep.load();
    beep.play();
}

$(".grid-header .g-ui-icon").addClass("ui-state-default ui-corner-all");

// In some environment, console is defined but console.log or console.error is missing.
if (window.console && window.console.log && window.console.error) {
    console.log('console log is defined');
} else {
  window['console'] = {log: function(){ }, error: function(){ }};
  console.log('dummy console is created');
}

// adding new formattier to slickgrid
(function ($) {
  // register namespace
  $.extend(true, window, {
    "Slick": {
      "Formatters": {
        "Checkmark": CheckmarkFormatter,
        "AscDescSelect": AscDescSelectFormatter,
      }
    }
  });

  function CheckmarkFormatter(row, cell, value, columnDef, dataContext) {
    return "<img src='./static/media/cross.png'>";
  }

  function AscDescSelectFormatter(row, cell, value, columnDef, dataContext) {
    return '<SELECT><OPTION value="true" '+ (value ? 'selected' : '') +'>ASC</OPTION>'+
                   '<OPTION value="false" '+(!value ? 'selected' : '')+'>DESC</OPTION></SELECT>';
  }

})(jQuery);

