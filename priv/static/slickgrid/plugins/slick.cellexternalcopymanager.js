import jQuery from "jquery";
import {dderlState} from '../../scripts/dderl';

(function ($) {
  // register namespace
  $.extend(true, window, {
    "Slick": {
      "CellExternalCopyManager": CellExternalCopyManager
    }
  });


  function CellExternalCopyManager(options) {
    /*
      This manager enables users to copy/paste data from/to an external Spreadsheet application
      such as MS-Excel® or OpenOffice-Spreadsheet.
      
      Since it is not possible to access directly the clipboard in javascript, the plugin uses
      a trick to do it's job. After detecting the keystroke, we dynamically create a textarea
      where the browser copies/pastes the serialized data. 
      
      options:
        copiedCellStyle : sets the css className used for copied cells. default : "copied"
        copiedCellStyleLayerKey : sets the layer key for setting css values of copied cells. default : "copy-manager"
        dataItemColumnValueExtractor : option to specify a custom column value extractor function
        dataItemColumnValueSetter : option to specify a custom column value setter function
        clipboardCommandHandler : option to specify a custom handler for paste actions
    */
    var _grid;
    var _self = this;
    var _copiedRanges;
    var _options = options || {};
    var _copiedCellStyleLayerKey = _options.copiedCellStyleLayerKey || "copy-manager";
    var _copiedCellStyle = _options.copiedCellStyle || "copied";
    var _clearCopyTI = 0;
    
    var keyCodes = {
        'C':67,
        'V':86
    };

    function init(grid) {
      _grid = grid;
      _grid.onKeyDown.subscribe(handleKeyDown);
      
      // we need a cell selection model
      var cellSelectionModel = grid.getSelectionModel();
      if (!cellSelectionModel){
        throw new Error("Selection model is mandatory for this plugin. Please set a selection model on the grid before adding this plugin: grid.setSelectionModel(new Slick.CellSelectionModel())");
      }
    }

    function destroy() {
      _grid.onKeyDown.unsubscribe(handleKeyDown);
    }
    
    function getDataItemValueForColumn(item, columnDef) {
      if (_options.dataItemColumnValueExtractor) {
        return _options.dataItemColumnValueExtractor(item, columnDef);
      }

      var retVal = '';

      // if a custom getter is not defined, we call serializeValue of the editor to serialize
      if (columnDef.editor){
        var editorArgs = {
          'container':$('body'),  // a dummy container
          'column':columnDef,
          'position':{'top':0, 'left':0}  // a dummy position required by some editors
        };
        var editor = new columnDef.editor(editorArgs);
        editor.loadValue(item);
        retVal = editor.serializeValue();
        editor.destroy();
      }

      return retVal;
    }
    
    function setDataItemValueForColumn(item, columnDef, value) {
      if (_options.dataItemColumnValueSetter) {
        return _options.dataItemColumnValueSetter(item, columnDef, value);
      }

      // if a custom setter is not defined, we call applyValue of the editor to unserialize
      if (columnDef.editor){
        var editorArgs = {
          'container': $('body'),  // a dummy container
          'column':columnDef,
          'position':{'top':0, 'left':0}  // a dummy position required by some editors
        };
        var editor = new columnDef.editor(editorArgs);
        editor.loadValue(item);
        editor.applyValue(item, value);
        editor.destroy();
      }
    }

    function readImagesFromEditable(element, cb) {
        var findImages = function() {
            var allImgs = $(element).find('img');
            if(allImgs || allImgs.length !== 0) {
                return getImageData(allImgs[0].src, function(data) {
                    document.body.removeChild(element);
                    cb(data);
                });
            } else {
                document.body.removeChild(element);
                return cb('');
            }
        };
        return setTimeout(findImages, 10);
    }

    function getImageData(src, cb) {
        var loader;
        loader = new Image();
        loader.onload = function() {
            var canvas, ctx, dataURL;
            canvas = document.createElement('canvas');
            canvas.width = loader.width;
            canvas.height = loader.height;
            ctx = canvas.getContext('2d');
            ctx.drawImage(loader, 0, 0, canvas.width, canvas.height);
            dataURL = null;
            try {
                dataURL = canvas.toDataURL('image/png');
                return cb(dataURL);
            } catch (_error) {
                return cb('');
            }
        };
        loader.src = src;
        return loader.src;
    }

    function getTime() {
        var d = new Date();
        return (d.getHours() + ':' + d.getMinutes() + ':' + d.getSeconds() + '.' + d.getMilliseconds());
    }

    function _createDivPaste() {
        var div = document.createElement('div');
        div.contentEditable = true;
        document.body.appendChild(div);
        div.focus();
        $(div).css({
            width: 1,
            height: 1,
            position: 'fixed',
            left: -100,
            overflow: 'hidden'
        }).on('paste', function(ev) {
            var clipboardData,
                item = null,
                reader, text, _i, _len,
                _ref = ev.originalEvent;
            //
            // IMPORTANT: to consume the paste event here to free the event loop
            //
            ev.preventDefault();

            if (_ref != null && _ref.clipboardData != null) {
                clipboardData = _ref.clipboardData;
                if (clipboardData.items) {                    
                    var _ref1 = clipboardData.items;
                    // Find items by type priority
                    // text > image
                    // rest of item types are ignored for now
                    for (_i = 0, _len = _ref1.length; _i < _len; _i++)
                        if(_ref1[_i].type === 'text/plain') {
                            item = _ref1[_i];
                            break;
                        }
                    for (_i = 0, _len = _ref1.length; _i < _len && item == null; _i++)
                        if(_ref1[_i].type.match(/^image\//)) {
                            item = _ref1[_i];
                            break;
                        }
                    if (item != null && item.type.match(/^image\//)) {
                        reader = new FileReader();
                        reader.onload = function(event) {
                            _processTabularData([[event.target.result]]);
                        };
                        reader.readAsDataURL(item.getAsFile());
                    } else if(item != null && item.type === 'text/plain') {
                        if(_grid.hasOwnProperty("gridowner")
                                && typeof _grid.gridowner.startPaste === 'function')
                            _grid.gridowner.startPaste();
                        item.getAsString(function(string) {
                            _decodeTabularTextData(string);
                        });
                    }
                } else {
                    if (clipboardData.types.length) {
                        if(_grid.hasOwnProperty("gridowner")
                                && typeof _grid.gridowner.startPaste === 'function')
                            _grid.gridowner.startPaste();
                        text = clipboardData.getData('Text');
                        if (text != null && text.length) {
                            _decodeTabularTextData(text);
                        }
                    } else {
                        readImagesFromEditable(div, function(data) {
                            _processTabularData([[data]]);
                        });
                    }
                }
            }
            clipboardData = window.clipboardData;
            if (clipboardData) {
                if(_grid.hasOwnProperty("gridowner")
                        && typeof _grid.gridowner.startPaste === 'function')
                    _grid.gridowner.startPaste();
                text = clipboardData.getData('Text');
                setTimeout(function() {
                    if (text != null && text.length) {
                        _decodeTabularTextData(text);
                    } else {
                        readImagesFromEditable(div, function(data) {
                            _processTabularData([[data]]);
                        });
                    }
                }, 1);
            }
        });
    }
    
    function _decodeTabularTextData(clipText) {
        setTimeout(function() {
            var clipRows = [clipText];

            console.log(getTime()+" clipboard paste "+clipText.length+" bytes");
            clipRows = clipText.replace(/\r\n/g, "\n").split("\n");
            var last = clipRows.pop();
            if(last) {
                clipRows.push(last);
            }
            var clpdRange = [];
            console.log(getTime()+" processing "+clipRows.length+" rows");
            processRowsInGroups(clipRows, clpdRange);
        }, 1);
    }

    function processRowsInGroups(clpRows, clpdRange) {
        if (clpRows.length > 0) {
            var newClpRows = clpRows.splice(0, Math.min(1000, clpRows.length));
            var startClipIdx = clpdRange.length;
            for (var i = 0; i < newClpRows.length; i++) {
                clpdRange[startClipIdx + i] = newClpRows[i].split("\t");
            }
            setTimeout(function() {
                processRowsInGroups(clpRows, clpdRange);
            }, 1);
        } else {
            console.log(getTime()+" rendering "+clpdRange.length+" rows");
            _processTabularData(clpdRange);
        }
    }

    function _processTabularData(clippedRange) {
      var columns = _grid.getColumns();
      var selectedCell = _grid.getActiveCell();
      var ranges = _grid.getSelectionModel().getSelectedRanges();
      var selectedRange = ranges && ranges.length ? ranges[0] : null;   // pick only one selection
      var activeRow = null;
      var activeCell = null;

      if (selectedRange){
        activeRow = Math.max(selectedRange.fromRow, 0);
        activeCell = Math.max(selectedRange.fromCell, 1);
      } else if (selectedCell){
        activeRow = selectedCell.row;
        activeCell = Math.max(selectedCell.cell, 1);
      } else {
        // we don't know where to paste
        return;
      }
      
      var oneCellToMultiple = false;
      var destH = clippedRange.length;
      var destW = clippedRange.length ? clippedRange[0].length : 0;
      if (clippedRange.length == 1 && clippedRange[0].length == 1 && selectedRange){
        oneCellToMultiple = true;
        destH = selectedRange.toRow - activeRow + 1;
        destW = selectedRange.toCell - activeCell + 1;
      }
      var availableCols = columns.length - activeCell;
      if(availableCols < destW) {
          alert("Error: Trying to paste out of the range of columns");
          return;
      }

      var readOnlyCols = [];
      for(var i = activeCell; i < activeCell + destW; ++i) {
          if(!columns[i].hasOwnProperty('editor')) {
              readOnlyCols.push(columns[i].name);
          }
      }
      if(readOnlyCols.length > 0) {
          var confirmText = "Information pasted on the readonly column";
          confirmText += (readOnlyCols.length == 1)?" ":"s ";
          confirmText += readOnlyCols.join(",");
          confirmText += " will be lost.\n\nDo you want to continue?";
          if(!confirm(confirmText)) {
              return;
          }
      }

      var availableRows = _grid.getDataLength() - activeRow;
      var addRows = 0;
      if(availableRows < destH)
      {
          var d = _grid.getData();
          if (_grid.getData() instanceof Slick.Data.DataView) {
              d = _grid.getData().getItems();
          }

        for(addRows = 1; addRows <= destH - availableRows; addRows++) {
            d.push({id: -addRows});
        }

        if (_grid.getData() instanceof Slick.Data.DataView) {
            _grid.getData().setItems(d);
        } else {
            _grid.setData(d);
        }
        _grid.render();
      }

      var clipCommand = {
        isClipboardCommand: true,
        clippedRange: clippedRange,
        cellExternalCopyManager: _self,
        _options: _options,
        setDataItemValueForColumn: setDataItemValueForColumn,
        markCopySelection: markCopySelection,
        oneCellToMultiple: oneCellToMultiple,
        activeRow: activeRow,
        activeCell: activeCell,
        destH: destH,
        destW: destW,
        desty: activeRow,
        destx: activeCell,
        maxDestY: _grid.getDataLength(),
        maxDestX: _grid.getColumns().length,
        h: 0,
        w: 0,

        execute: function() {
            var self = this;
            this.h=0;
            self.processRows(destW, activeRow, activeCell, destH, 0, function() {
                var bRange = new Slick.Range(activeRow, activeCell, activeRow + self.h-1, activeCell + self.w-1);
                self.markCopySelection([bRange]);
                _grid.getSelectionModel().setSelectedRanges([bRange]);
                _grid.focus();
                self.cellExternalCopyManager.onPasteCells.notify({ranges: [bRange]});
                if(_grid.hasOwnProperty("gridowner")
                    && typeof _grid.gridowner.endPaste === 'function')
                    _grid.gridowner.endPaste();
            });
        },

        processRows : function(destW, activeRow, activeCell, limit, y, cb) {
            //We need to process max 100 rows to not block the gui.
            var self = this;
            var batchLimit = y + Math.min(limit - y, 100);
            var parsedNewValue = unescapeNewLines(clippedRange[0][0]);

            var dv = null;
            var dvi = null;
            if (_grid.getData() instanceof Slick.Data.DataView) {
                dv = _grid.getData();
                dvi = dv.getItems();
            } else {
                console.error("Missing grid DataView, paste not supported");
                return;
            }

            console.log(getTime()+" loading "+batchLimit+" of "+limit+
                    " ("+Math.floor(batchLimit*100/limit)+"% completed)");
            dv.beginUpdate();
            for(; y < batchLimit; ++y) {
                this.w=0;
                this.h++;
                var rowW = destW;
                if(!oneCellToMultiple) {
                    rowW = clippedRange[y].length;
                }

                var desty = activeRow + y;
                var dt = dvi[desty];

                var dat = dv.getItemById(dt.id);
                for (var x = 0; x < rowW; x++) {
                    this.w++;
                    var destx = activeCell + x;

                    if (desty < this.maxDestY && destx < this.maxDestX ) {
                        if (dt != undefined) {
                            if (!oneCellToMultiple) {
                                parsedNewValue = unescapeNewLines(clippedRange[y][x]);
                            }
                            dat[columns[destx].field] = parsedNewValue;
                        }
                    }
                }
                dv.updateItem(dat.id, dat);
            }
            dv.endUpdate();
            _grid.scrollRowIntoView(activeRow + y);

            if(y < limit) {
                setTimeout(function() {
                    self.processRows(destW, activeRow, activeCell, limit, y, cb);
                }, 1);
            } else {
                console.log(getTime()+" loaded "+limit+" rows");
                cb();
            }
        }
      };

      if(_options.clipboardCommandHandler) {
          _options.clipboardCommandHandler(clipCommand);
      } else {
          clipCommand.execute();
      }
    }

    function handleKeyDown(e, args) {
      var ranges;
      if (!_grid.getEditorLock().isActive()) {
        if (e.which == $.ui.keyCode.ESCAPE) {
          if (_copiedRanges) {
            e.preventDefault();
            clearCopySelection();
            _self.onCopyCancelled.notify({ranges: _copiedRanges});
            _copiedRanges = null;
          }
        }
        
        ranges = _grid.getSelectionModel().getSelectedRanges();
        if (e.ctrlKey || e.metaKey) {
            switch (e.which) {
            case keyCodes.C:
                process_copy(e, args, ranges, dderlState.copyMode);
                // Reset the copy mode to normal after a operation.
                dderlState.copyMode = "normal";
                return false;
            case keyCodes.V:
                _createDivPaste();
                return false;
            }
        }
      }
    }

    function process_copy(e, args, ranges, copyMode) {
        if (ranges.length != 0) {
            _copiedRanges = ranges;
            markCopySelection(ranges);
            _self.onCopyCells.notify({ranges: ranges});
            
            var columns = _grid.getColumns();
            var clipTextArr = [];

            // Find the bounding range
            var boundRange = new Slick.Range(ranges[0].fromRow, ranges[0].fromCell, ranges[0].toRow, ranges[0].toCell);
            for (let rg = 1; rg < ranges.length; ++rg) {
                var range = ranges[rg];
                boundRange.fromRow = Math.min(range.fromRow, boundRange.fromRow);
                boundRange.fromCell = Math.min(range.fromCell, boundRange.fromCell);
                boundRange.toRow = Math.max(range.toRow, boundRange.toRow);
                boundRange.toCell = Math.max(range.toCell, boundRange.toCell);
            }
            boundRange.fromCell = Math.max(boundRange.fromCell, 1);

            // Find the used columns
            var usedCols = [];
            for (let colpos = boundRange.fromCell; colpos <= boundRange.toCell; colpos++) {
                for (let rg = 0; rg < ranges.length; ++rg) {
                    if (colpos >= ranges[rg].fromCell && colpos <= ranges[rg].toCell) {
                        usedCols.push(colpos);
                        break;
                    }
                }
            }

            if (copyMode === "header") {
                var headerColumns = [];
                for (let k = 0; k < usedCols.length; ++k) {
                    headerColumns.push(columns[usedCols[k]].name);
                }
                clipTextArr.push(headerColumns.join("\t"));
                clipTextArr.push("\r\n");
            }

            var clipText = "";
            if (copyMode === "json") {
                clipText = jsonFromBoundRange(ranges, boundRange, columns, usedCols);
            } else {
                clipTextArr.push(textFromBoundRange(ranges, boundRange, columns, usedCols));
                clipText = clipTextArr.join('');
            }
            var focusElement = document.activeElement;

            var ta = createCopyTextBox(clipText);
            ta.focus();
            
            setTimeout(function(){
                document.body.removeChild(ta);
                // restore focus
                if (focusElement) {
                    focusElement.focus();
                }
            }, 100);
        }
    }

    function jsonFromBoundRange(ranges, boundRange, columns, usedCols) {
        var rowObjects, clipTextCells;

        var gridData = _grid.getData();
        if (_grid.getData() instanceof Slick.Data.DataView) {
            gridData = _grid.getData().getItems();
        }

        var counter = 0;
        var numericIdList = {};
        var currentId = "";
        rowObjects = [];
        for (var i = boundRange.fromRow; i <= boundRange.toRow ; ++i) {
            clipTextCells = {};
            var isRowEmpty = true;
            for (var j = 0; j < usedCols.length; ++j) {
                clipTextCells[columns[usedCols[j]].name] = null;
                for(var rg = 0; rg < ranges.length; ++rg) {
                    if(ranges[rg].contains(i, usedCols[j])) {
                        if(gridData[i][columns[usedCols[j]].field] !== "'$not_a_value'") {
                            if(columns[usedCols[j]].type === "numeric") {
                                currentId = uniqueid() + counter.toString();
                                ++counter;
                                numericIdList[currentId] = escapeNewLines(gridData[i][columns[usedCols[j]].field]);
                                clipTextCells[columns[usedCols[j]].name] = currentId;
                            } else if (columns[usedCols[j]].type === "text" || gridData[i][columns[usedCols[j]].field]) {
                                clipTextCells[columns[usedCols[j]].name] = escapeNewLines(gridData[i][columns[usedCols[j]].field]);
                            }
                            isRowEmpty = false;
                        }
                        break;
                    }
                }
            }
            if(!isRowEmpty) {
                rowObjects.push(clipTextCells);
            }
        }
        var result = JSON.stringify(rowObjects, undefined, 4);
        for(currentId in numericIdList) {
            if(numericIdList[currentId] === "") {
                numericIdList[currentId] = "null";
            }
            result = result.replace("\"" + currentId + "\"", numericIdList[currentId]);
        }
        return result;
    }

    function uniqueid() {
        // always start with a letter (for DOM friendlyness)
        var idstr=String.fromCharCode(Math.floor((Math.random()*25)+65));
        do {
            // between numbers and characters (48 is 0 and 90 is Z (42-48 = 90)
            var ascicode=Math.floor((Math.random()*42)+48);
            if (ascicode<58 || ascicode>64){
                // exclude all chars between : (58) and @ (64)
                idstr+=String.fromCharCode(ascicode);
            }
        } while (idstr.length<32);

        return (idstr);
    }

    function textFromBoundRange(ranges, boundRange, columns, usedCols) {
        var clipTextRows, clipTextCells, cellValue;

        var gridData = _grid.getData();
        if (_grid.getData() instanceof Slick.Data.DataView) {
            gridData = _grid.getData().getItems();
        }

        clipTextRows = [];
        for (var i = boundRange.fromRow; i <= boundRange.toRow ; ++i) {
            clipTextCells = [];
            var isRowEmpty = true;
            for (var j = 0; j < usedCols.length; ++j) {
                cellValue = "";
                for(var rg = 0; rg < ranges.length; ++rg) {
                    if(ranges[rg].contains(i, usedCols[j])) {
                        if(gridData[i][columns[usedCols[j]].field] !== "'$not_a_value'") {
                            cellValue = escapeNewLines(gridData[i][columns[usedCols[j]].field]);
                            isRowEmpty = false;
                        }
                        break;
                    }
                }
                clipTextCells.push(cellValue);
            }
            if(!isRowEmpty) {
                clipTextRows.push(clipTextCells.join("\t"));
            }
        }
        return clipTextRows.join("\r\n");
    }

    function markCopySelection(ranges) {
      clearCopySelection();
      
      var columns = _grid.getColumns();
      var hash = {};
      for (var i = 0; i < ranges.length; i++) {
        for (var j = ranges[i].fromRow; j <= ranges[i].toRow; j++) {
          hash[j] = {};
          for (var k = ranges[i].fromCell; k <= ranges[i].toCell && k<columns.length; k++) {
            hash[j][columns[k].id] = _copiedCellStyle;
          }
        }
      }
      _grid.setCellCssStyles(_copiedCellStyleLayerKey, hash);
      clearTimeout(_clearCopyTI);
      _clearCopyTI = setTimeout(function(){
        _self.clearCopySelection();
      }, 2000);
    }

    function clearCopySelection() {
      _grid.removeCellCssStyles(_copiedCellStyleLayerKey);
    }

    $.extend(this, {
      "init": init,
      "destroy": destroy,
      "clearCopySelection": clearCopySelection,
      "handleKeyDown":handleKeyDown,
      
      "onCopyCells": new Slick.Event(),
      "onCopyCancelled": new Slick.Event(),
      "onPasteCells": new Slick.Event()
    });
  }
})(jQuery);

/* Escape new lines and tabs */
function escapeNewLines(str) {
    var result = "";
    if(typeof str == 'string' || str instanceof String) {
        for(var i = 0; i < str.length; ++i) {
            if(str.charCodeAt(i) === 9) {
                result += "\\t";
            } else if(str.charCodeAt(i) === 10) {
                result += "\\n";
            } else if(str.charCodeAt(i) !== 13) {
                result += str[i];
            }
        }
    } else {
        result = str;
    }
    return result;
}

function unescapeNewLines(str) {
    try {
        JSON.parse(str);
    } catch (e) {
        str = str.replace(/\\t/gi, "\t");
        str = str.replace(/\\n/gi, "\n");
    }
    return unescape(str);
}

export function createCopyTextBox(innerText) {
    var ta = document.createElement('textarea');
    ta.style.position = 'absolute';
    ta.style.left = '-1000px';
    ta.style.top = document.body.scrollTop + 'px';
    ta.value = innerText;
    document.body.appendChild(ta);
    ta.select();

    return ta;
}