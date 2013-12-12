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
    }

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
          'container':$(document),  // a dummy container
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
          'container':$(document),  // a dummy container
          'column':columnDef,
          'position':{'top':0, 'left':0}  // a dummy position required by some editors
        };
        var editor = new columnDef.editor(editorArgs);
        editor.loadValue(item);
        editor.applyValue(item, value);
        editor.destroy();
      }
    }
    
    
    function _createTextBox(innerText){
      var ta = document.createElement('textarea');
      ta.style.position = 'absolute';
      ta.style.left = '-1000px';
      ta.style.top = document.body.scrollTop + 'px';
      ta.value = innerText;
      document.body.appendChild(ta);
      ta.select();
      
      return ta;
    }
    
    function _decodeTabularData(_grid, ta){
      var columns = _grid.getColumns();
      var clipText = ta.value;
      var clipRows = clipText.split(/[\n\f\r]/);
      var clippedRange = [];
      
      document.body.removeChild(ta);

      for (var i=0; i<clipRows.length; i++) {
          clippedRange[i] = clipRows[i].split("\t");
      }

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

      var readOnlyCols = new Array();
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
        if (_grid.getData() instanceof Slick.Data.DataView)
            d = _grid.getData().getItems();

        for(addRows = 1; addRows <= destH - availableRows; addRows++) {
            d.push({id: addRows - 9999});
        }

        if (_grid.getData() instanceof Slick.Data.DataView)
            _grid.getData().setItems(d);
        else
            _grid.setData(d);
        _grid.render();
      }
      var clipCommand = {

        isClipboardCommand: true,
        clippedRange: clippedRange,
        oldValues: [],
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
          this.h=0;
          for (var y = 0; y < destH; y++){
            this.oldValues[y] = [];
            this.w=0;
            this.h++;
            for (var x = 0; x < destW; x++){
              this.w++;
              var desty = activeRow + y;
              var destx = activeCell + x;

              if (desty < this.maxDestY && destx < this.maxDestX ) {
                var nd = _grid.getCellNode(desty, destx);
                var dt = null;
                if (_grid.getData() instanceof Slick.Data.DataView)
                    dt = _grid.getData().getItems()[desty];
                else
                    dt = _grid.getDataItem(desty);

                this.oldValues[y][x] = dt[columns[destx]['id']];
                if (oneCellToMultiple) {
                    this.setDataItemValueForColumn(dt, columns[destx], clippedRange[0][0]);
                } else {
                    this.setDataItemValueForColumn(dt, columns[destx], clippedRange[y][x]);
                }
                _grid.updateCell(desty, destx);
              }
            }
          }

          var bRange = new Slick.Range(activeRow, activeCell, activeRow+this.h-1, activeCell+this.w-1);
          this.markCopySelection([bRange]);
          _grid.getSelectionModel().setSelectedRanges([bRange]);
          _grid.focus();
          this.cellExternalCopyManager.onPasteCells.notify({ranges: [bRange]});
        },

        undo: function() {
          for (var y = 0; y < destH; y++){
            for (var x = 0; x < destW; x++){
              var desty = activeRow + y;
              var destx = activeCell + x;

              if (desty < this.maxDestY && destx < this.maxDestX ) {
                var nd = _grid.getCellNode(desty, destx);
                var dt = null;
                if (_grid.getData() instanceof Slick.Data.DataView)
                    dt = _grid.getData().getItems()[desty];
                else
                    dt = _grid.getDataItem(desty);
                if (oneCellToMultiple)
                  this.setDataItemValueForColumn(dt, columns[destx], this.oldValues[0][0]);
                else
                  this.setDataItemValueForColumn(dt, columns[destx], this.oldValues[y][x]);
                _grid.updateCell(desty, destx);
              }
            }
          }

          var bRange = new Slick.Range(activeRow, activeCell, activeRow+this.h-1, activeCell+this.w-1);

          this.markCopySelection([bRange]);
          _grid.getSelectionModel().setSelectedRanges([bRange]);
          this.cellExternalCopyManager.onPasteCells.notify({ranges: [bRange]});
          
          if(addRows > 1){
            var d = _grid.getData();
            if (_grid.getData() instanceof Slick.Data.DataView)
                d = _grid.getData().getItems();
            for(; addRows > 1; addRows--)
              d.splice(d.length - 1, 1);
            if (_grid.getData() instanceof Slick.Data.DataView)
                _grid.getData().setItems(d);
            else
                _grid.setData(d);
            _grid.render();
          }
        }
      };

      if(_options.clipboardCommandHandler) {
        _options.clipboardCommandHandler(clipCommand);
      }
      else {
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
        
        if (e.which == keyCodes.C && (e.ctrlKey || e.metaKey)) {    // CTRL + C
          ranges = _grid.getSelectionModel().getSelectedRanges();
          if (ranges.length != 0) {
            _copiedRanges = ranges;
            markCopySelection(ranges);
            _self.onCopyCells.notify({ranges: ranges});
            
            var columns = _grid.getColumns();
            var clipTextArr = [];
            var gridData = _grid.getData();
            if (_grid.getData() instanceof Slick.Data.DataView)
                gridData = _grid.getData().getItems();

            for (var rg = 0; rg < ranges.length; rg++){
                var range = ranges[rg];
                var fromCellSafe = Math.max(range.fromCell, 1);
                var clipTextRows = [];
                for (var i=range.fromRow; i < range.toRow+1 ; i++){
                    var clipTextCells = [];
                    for (var j=fromCellSafe; j< range.toCell+1 ; j++){
                        var cellValue = gridData[i][columns[j].field];
                        clipTextCells.push(escapeNewLines(cellValue));
                    }
                    clipTextRows.push(clipTextCells.join("\t"));
                }
                clipTextArr.push(clipTextRows.join("\r\n"));
                if (clipTextRows.length === 1 && ranges.length > 1 && rg < ranges.length - 1) {
                    clipTextArr.push("\r\n");
                }
            }
            var clipText = clipTextArr.join('');
            var $focus = $(_grid.getActiveCellNode());

            var ta = _createTextBox(clipText);

            ta.focus();
            
            setTimeout(function(){
                document.body.removeChild(ta);
                // restore focus
                if ($focus && $focus.length > 0) {
                    $focus.attr('tabIndex', '-1');
                    $focus.focus();
                    $focus.removeAttr('tabIndex');
                }
            }, 100);

            return false;
          }
        }

        if (e.which == keyCodes.V && (e.ctrlKey || e.metaKey)) {    // CTRL + V
            var ta = _createTextBox('');
            
            setTimeout(function(){
                _decodeTabularData(_grid, ta);
            }, 100);
            
            return false;
        }
      }
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
