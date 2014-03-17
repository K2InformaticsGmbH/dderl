(function ($) {
    // register namespace
    $.extend(true, window, {
        "Slick": {
            "CellRowColSelectionModel": CellRowColSelectionModel
        }
    });


    function CellRowColSelectionModel(options) {
        var _grid;
        var _canvas;
        var _ranges = [];
        var _self = this;
        var _handler = new Slick.EventHandler();
        var _inHandler;
        var _selector = new Slick.CellRangeSelector({
            "selectionCss": {
                "border": "2px solid black"
            }
        });
        var _options;
        var _defaults = {
            selectActiveCell: true,
            selectActiveRow: true
        };


        function init(grid) {
            _options = $.extend(true, {}, _defaults, options);
            _grid = grid;
            _canvas = _grid.getCanvasNode();
            _handler.subscribe(_grid.onActiveCellChanged, wrapHandler(handleActiveCellChange));
            _handler.subscribe(_grid.onKeyDown, wrapHandler(handleKeyDown));
            _handler.subscribe(_grid.onClick, wrapHandler(handleClick));
            _handler.subscribe(_grid.onHeaderClick, wrapHandler(handleOnHeaderClick));
            _handler.subscribe(_selector.onCellRangeSelected, wrapHandler(handleCellRangeSelected));
            _handler.subscribe(_selector.onBeforeCellRangeSelected, wrapHandler(handleBeforeCellRangeSelected));

            grid.registerPlugin(_selector);
        }

        function destroy() {
            _handler.unsubscribeAll();
            _grid.unregisterPlugin(_selector);
        }

        function wrapHandler(handler) {
            return function () {
                if (!_inHandler) {
                    _inHandler = true;
                    handler.apply(this, arguments);
                    _inHandler = false;
                }
            };
        }

        function rangesToRows(ranges) {
            var rows = [];
            for (var i = 0; i < ranges.length; i++) {
                for (var j = ranges[i].fromRow; j <= ranges[i].toRow; j++) {
                    rows.push(j);
                }
            }
            return rows;
        }

        function rowsToRanges(rows) {
            var ranges = [];
            var lastCell = _grid.getColumns().length - 1;
            for (var i = 0; i < rows.length; i++) {
                ranges.push(new Slick.Range(rows[i], 0, rows[i], lastCell));
            }
            return ranges;
        }

        function getRowsRange(from, to) {
            var i, rows = [];
            for (i = from; i <= to; i++) {
                rows.push(i);
            }
            for (i = to; i < from; i++) {
                rows.push(i);
            }
            return rows;
        }

        function getSelectedRows() {
            return rangesToRows(_ranges);
        }

        function setSelectedRows(rows) {
            setSelectedRanges(rowsToRanges(rows));
        }

        function removeInvalidRanges(ranges) {
            var result = [];
            for (var i = 0; i < ranges.length; i++) {
                var r = ranges[i];
                if (!r.fromCell) {
                    // we return here if r.fromCell is NaN
                    // this it the case if we are selecting a row
                    return ranges;
                } else if(r.fromRow <= 0 && r.toRow === 0) {
                    // we are selecting a column when there are no rows.
                    result.push(r);
                } else {
                    // we are selecting a cell range
                    if (_grid.canCellBeSelected(r.fromRow, r.fromCell) && _grid.canCellBeSelected(r.toRow, r.toCell)) {
                        result.push(r);
                    }
                }
            }
            return result;
        }

        function setSelectedRanges(origRanges) {
            _ranges = removeInvalidRanges(origRanges);
            _self.onSelectedRangesChanged.notify(_ranges);
        }

        function getSelectedRanges() {
            return _ranges;
        }

        function handleBeforeCellRangeSelected(e, args) {
            if (_grid.getEditorLock().isActive()) {
                e.stopPropagation();
                return false;
            }
        }

        function handleCellRangeSelected(e, args) {
            _grid.getEditorLock().commitCurrentEdit();
            setSelectedRanges([args.range]);
        }

        function handleActiveCellChange(e, data) {
            if (data.cell == 0) {
                // we are in row select mode
                if (_options.selectActiveRow) {
                    setSelectedRanges([new Slick.Range(data.row, 0, data.row, _grid.getColumns().length - 1)]);
                }
            } else {
                // we are in cell select mode
                if (_options.selectActiveCell) {
                    setSelectedRanges([new Slick.Range(data.row, data.cell)]);
                }
            }
        }

        function createFullColRange(fromRow, fromCell, toRow, toCell) {
            var range = new Slick.Range(fromRow, fromCell, toRow, toCell);
            range.fullCol = true;
            return range;
        }

        function handleOnHeaderClick(e, data) {
            if (data.column) {
                if (typeof data.column.id != "undefined") {
                    var gvp = _grid.getViewport();
                    var gvpMid = Math.floor((gvp.top + gvp.bottom)/2);

                    var col = _grid.getColumnIndex(data.column.id);
                    var maxRow = _grid.getDataLength() - 1;
                    _grid.getEditorLock().commitCurrentEdit();
                    var activeCell = _grid.getActiveCell();

                    if (!e.ctrlKey && !e.shiftKey && !e.metaKey) {
                        if(col === 0) {
                            _grid.setActiveCell(gvpMid, 1);
                            _ranges = [];
                            var range = new Slick.Range(0, 1, maxRow, _grid.getColumns().length - 1);
                            range.fullCol = true;
                            _ranges.push(range);
                            setSelectedRanges(_ranges);
                        } else {
                            _grid.setActiveCell(gvpMid, col);
                            setSelectedRanges([createFullColRange(0, col, maxRow, col)]);
                        }
                    } else if(col !== 0) {
                        var matches = $.grep(_ranges, function(o,i) {
                            return (col === _ranges[i].fromCell || col === _ranges[i].toCell);
                        });
                        if (matches.length === 0 && (e.ctrlKey || e.metaKey)) {
                            _grid.setActiveCell(gvpMid, col);
                            _ranges.push(createFullColRange(0, col, maxRow, col));
                        } else if (matches.length !== 0 && (e.ctrlKey || e.metaKey)) {
                            _ranges = $.grep(_ranges, function (o, i) {
                                return !(col === _ranges[i].fromCell || col === _ranges[i].toCell);
                            });
                        } else if (e.shiftKey) {
                            if(activeCell) {
                                var from = Math.min(activeCell.cell, col);
                                var to = Math.max(activeCell.cell, col);
                                _ranges = [];
                                for(var i=from; i <= to; ++i) {
                                    _ranges.push(createFullColRange(0, i, maxRow, i));
                                }
                            } else {
                                _grid.setActiveCell(0, col);
                                _ranges = [createFullColRange(0, col, maxRow, col)];
                            }
                        }
                        setSelectedRanges(_ranges);
                    }
                }
            }
        }

        function handleKeyDown(e) {
            var activeCell = _grid.getActiveCell();
            if(e.ctrlKey && e.keyCode === 65) {
                var maxRow = _grid.getDataLength() - 1;
                _grid.setActiveCell(0, 1);
                _ranges = [];
                for(var i = 1; i < _grid.getColumns().length; ++i) {
                    _ranges.push(createFullColRange(0, i, maxRow, i));
                }
                setSelectedRanges(_ranges);
                e.preventDefault();
                e.stopPropagation();
            } else if (activeCell && e.shiftKey && !e.ctrlKey && !e.altKey && !e.metaKey &&
                (e.which >= 37 && e.which <= 40)) {
                _grid.getEditorLock().commitCurrentEdit();

                if(_ranges.length !== 1 || !_ranges[0].contains(activeCell.row, activeCell.cell)) {
                    if(activeCell.cell !== 0) {
                        _ranges = [new Slick.Range(activeCell.row, activeCell.cell)];
                    } else {
                        _ranges = [new Slick.Range(activeCell.row, 0, activeCell.row, _grid.getColumns().length - 1)];
                    }
                }

                if(e.which == 37) { // left
                    if(_ranges[0].toCell > activeCell.cell) {
                        if(_ranges[0].toCell > 1) {
                            _ranges[0].toCell -=1;
                        }
                    } else {
                        if(_ranges[0].fromCell > 1) {
                            _ranges[0].fromCell -= 1;
                        }
                    }
                } else if (e.which == 38) { // up
                    if(_ranges[0].toRow > activeCell.row) {
                        _ranges[0].toRow -=1;
                    } else {
                        if(_ranges[0].fromRow > 0) {
                            _ranges[0].fromRow -= 1;
                        }
                    }
                } else if (e.which == 39) { // right
                    if(_ranges[0].fromCell < activeCell.cell) {
                        _ranges[0].fromCell +=1;
                    } else {
                        if(_ranges[0].toCell < (_grid.getColumns().length - 1)) {
                            _ranges[0].toCell += 1;
                        }
                    }
                } else { // down
                    if(_ranges[0].fromRow < activeCell.row) {
                        _ranges[0].fromRow +=1;
                    } else {
                        if(_ranges[0].toRow < (_grid.getDataLength() - 1)) {
                            _ranges[0].toRow += 1;
                        }
                    }
                }

                // Scroll view into the selection area.
                var viewRow = activeCell.row == _ranges[0].fromRow ? _ranges[0].toRow : _ranges[0].fromRow;
                var viewCell = activeCell.cell == _ranges[0].fromCell ? _ranges[0].toCell : _ranges[0].fromCell;
                _grid.scrollCellIntoView(viewRow, viewCell);

                setSelectedRanges(_ranges);

                e.preventDefault();
                e.stopPropagation();
            }
        }

        function handleClick(e) {
            var cell = _grid.getCellFromEvent(e);
            if (!cell || !_grid.canCellBeActive(cell.row, cell.cell)) {
                return false;
            }

            var activeCell = _grid.getActiveCell();
            if (!e.ctrlKey && !e.shiftKey && !e.metaKey) {
                return false;
            } else if (activeCell && _grid.getOptions().multiSelect) {
                _grid.getEditorLock().commitCurrentEdit();
                if(cell.cell === 0) {
                    var selection = rangesToRows(_ranges);
                    var idx = $.inArray(cell.row, selection);

                    if (idx === -1 && (e.ctrlKey || e.metaKey)) {
                        selection.push(cell.row);
                        _grid.setActiveCell(cell.row, cell.cell);
                        _ranges = rowsToRanges(selection);
                    } else if (idx !== -1 && (e.ctrlKey || e.metaKey)) {
                        selection = $.grep(selection, function (o, i) {
                            return (o !== cell.row);
                        });
                        _ranges = rowsToRanges(selection);
                    } else if (e.shiftKey) {
                        var from = Math.min(cell.row, activeCell.row);
                        var to = Math.max(cell.row, activeCell.row);
                        _ranges = [new Slick.Range(from, 0, to, _grid.getColumns().length - 1)];
                    }
                } else {
                    var matches = $.grep(_ranges, function(o,i) {
                        return _ranges[i].contains(cell.row, cell.cell);
                    });
                    if (matches.length === 0 && (e.ctrlKey || e.metaKey)) {
                        //TODO: Consolidate ranges.
                        _ranges.push(new Slick.Range(cell.row, cell.cell, cell.row, cell.cell));
                        _grid.setActiveCell(cell.row, cell.cell);
                    } else if (matches.length !== 0 && (e.ctrlKey || e.metaKey)) {
                        // TODO: Split ranges if the cell is inside, test using contains.
                        _ranges = $.grep(_ranges, function (o, i) {
                            return !((cell.row === _ranges[i].fromRow && cell.cell === _ranges[i].fromCell)
                                     ||
                                     (cell.row === _ranges[i].toRow && cell.cell === _ranges[i].toCell));
                        });
                    } else if (e.shiftKey) {
                        var fromRow = Math.min(cell.row, activeCell.row);
                        var fromCell = Math.min(cell.cell, activeCell.cell);
                        var toRow = Math.max(cell.row, activeCell.row);
                        var toCell = Math.max(cell.cell, activeCell.cell);
                        _ranges = [new Slick.Range(fromRow, fromCell, toRow, toCell)];
                    }
                }
                setSelectedRanges(_ranges);

                e.stopImmediatePropagation();
                return true;
            }
            return false;
        }

        $.extend(this, {
            "getSelectedRanges": getSelectedRanges,
            "setSelectedRanges": setSelectedRanges,

            "init": init,
            "destroy": destroy,

            "onSelectedRangesChanged": new Slick.Event()
        });
    }
})(jQuery);
