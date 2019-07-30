function init(container, width, height) {
    "use strict";
    // This code is executed once and it should initialize the graph.

    // Run the query:
    // select
    //    ckey, x, y, nodeType, functional, frame, fill
    // from
    //    grid
    
    // If the table doesn't exit create it and sample data:
    /****

create table
    grid (
        ckey term,
        x integer,
        y integer,
        nodeType atom,
        functional boolean,
        available boolean,
        forbidden boolean,
        frame atom,
        fill atom
    )

    // Sample initial data of hidden query:

[1,2]	1	2	path	true	black	#F0F0F0
[3,7]	3	7	path	true	black	#F0F0F0
[3,3]	3	3	path	true	black	#F0F0F0
[3,5]	3	5	path	true	black	#F0F0F0
[0,1]	0	1	exit	true	black	palegreen
[2,3]	2	3	path	true	black	#F0F0F0
[2,2]	2	2	path	true	black	#F0F0F0
[3,4]	3	4	path	true	white	#F0F0F0
[1,1]	1	1	path	true	black	#F0F0F0
[3,6]	3	6	path	true	white	#F0F0F0
[3,8]	3	8	path	true	black	#F0F0F0
[3,9]	3	9	entrance	true	black	lightsalmon
[4,5]	4	5	path	false	white	#F0F0F0

    */
    // Table containing the tracks i.e trackid 123:
    /*

create table
    routes (
        ckey term,
        x integer,
        y integer,
        frame atom,
        fill atom
    )

[1,2,123]	1	2	black	indigo
[3,7,123]	3	7	black	indigo
[3,3,123]	3	3	black	indigo
[3,5,123]	3	5	black	indigo
[2,3,123]	2	3	black	indigo
[2,2,123]	2	2	black	indigo
[3,4,123]	3	4	black	indigo
[1,1,123]	1	1	black	indigo
[3,6,123]	3	6	black	indigo
[3,8,123]	3	8	black	indigo


    // sample query for comparison (Hidden stmt so check the console for the data):

select
    :binstr_start_x x1, :binstr_start_y y1, :binstr_end_x x2, :binstr_end_y y2
from
    dual
    */


    // Name of the master grid view.
    var viewName = 'baseGrid';
    var canvasBackground = 'aliceblue';

    // View called when selecting multiple points using shift+click.
    var comparisonViewName = 'cell_compare';
    var compareViewParameters = { // Add dummy placeholder values.
        ':binstr_start_x': {typ: "binstr", val: 0},
        ':binstr_start_y': {typ: "binstr", val: 0},
        ':binstr_end_x': {typ: "binstr", val: 0},
        ':binstr_end_y': {typ: "binstr", val: 0}
    };

    var animDuration = 200;
    // Not exact as we have world coodinates for zoom and real for
    // the zoom helper but probably close enough
    var zoomThreshold = 5;

    // token radius in virtual coordinates
    var radius = 3;
    var squareSize = 12;

    var margin = { top: 10, right: 10, bottom: 10, left: 10 }; // physical margins in px

    // base element for virtual dom
    var base = d3.select(document.createElement('custom'));

    var canvas = container
        .append('canvas')
        .style('background-color', canvasBackground)
        .style('margin-top', margin.top + 'px')
        .style('margin-right', margin.right + 'px')
        .style('margin-bottom', margin.bottom + 'px')
        .style('margin-left', margin.left + 'px');

    var context = canvas.node().getContext('2d');

    function resize(w, h) {
        height = h - (margin.top + margin.bottom);
        width = w - (margin.left + margin.right);
        canvas.attr('width', width)
            .attr('height', height);
        render();
    }

    function clearCanvas(ctx) {
        context.setTransform(1, 0, 0, 1, 0, 0);
        ctx.clearRect(0, 0, width, height);
    }

    function parseError(term) {
        return new Error(term + " is not a valid json term");
    }

    function parseKey(ckey) {
        try {
            return JSON.parse(ckey);
        } catch (e) {
            throw parseError(ckey);
        }
    }

    function updateGrid(rows, prevGrid) {
        rows.forEach(function(row) {
            var id = row.x_2 + '_' + row.y_3;  // ckey [x,y] or id

            if(row.op === 'del') {
                delete prevGrid.squares[id];
                return;
            }

            var functional = row.functional_5 == 'true' // string to bool.

            prevGrid.squares[id] = {
                type: row.nodeType_4,
                fill: row.fill_7,
                x: row.x_2 * squareSize,
                y: row.y_3 * squareSize,
                functional: functional, 
                frame: row.frame_6,

            };

            if(row.x_2 < prevGrid.box.start.x) {
                prevGrid.box.start.x = +row.x_2;
            }
            if(row.x_2 >= prevGrid.box.end.x) {
                prevGrid.box.end.x = +row.x_2 + 1;
            }

            if(row.y_3 < prevGrid.box.start.y) {
                prevGrid.box.start.y = +row.y_3;
            }
            if(row.y_3 >= prevGrid.box.end.y) {
                prevGrid.box.end.y = +row.y_3 + 1;
            }

        });

        return prevGrid;
    }

    function entries(obj) {
        var res = [];
        for(var k in obj) {
            obj[k].id = k;
            res.push(obj[k]);
        }
        return res;
    }

    function initGrid() {
        // Minimum grid size 10x10
        return {
            squares: {},
            circles: {},
            box: {
                start: {x: 0, y: 0},
                end: {x: 10, y: 10}
            },
            colors: {}
        };
    }

    function getOffset(pGrid) {
        return {
            x: pGrid.box.start.x * squareSize - radius,
            y: pGrid.box.start.y * squareSize - radius
        };
    }

    var scale;
    // Prefer to avoid `grid` param name to not shadow the external variable
    function updateViewBox(pGrid) { //parameter grid.
        var w = pGrid.box.end.x - pGrid.box.start.x;
        var h = pGrid.box.end.y - pGrid.box.start.y;
        var offset = getOffset(pGrid);
        var vBox = {
            x: offset.x,
            y: offset.y,
            w: w * squareSize + 2 * radius,
            h: h * squareSize + 2 * radius
        };

        if(zoom) {
            w = zoomBoundingBox.end.x - zoomBoundingBox.start.x;
            h = zoomBoundingBox.end.y - zoomBoundingBox.start.y;
            // Pick min scale to preserve aspect ratio.
            scale = Math.min(width / w, height / h);
            vBox.x = zoomBoundingBox.start.x * scale;
            vBox.y = zoomBoundingBox.start.y * scale;
        } else {
            // Pick min scale to preserve aspect ratio.
            scale = Math.min(width / vBox.w, height / vBox.h);
            vBox.x = vBox.x * scale;
            vBox.y = vBox.y * scale;
        }
        /**
         * a: Horizontal scaling. A value of 1 results in no scaling.
         * b: Vertical skewing.
         * c: Horizontal skewing.
         * d: Vertical scaling. A value of 1 results in no scaling.
         * e: Horizontal translation (moving).
         * f: Vertical translation (moving).
        */
       context.setTransform(scale, 0, 0, scale, -vBox.x, -vBox.y);
    }

    function toWorldCoordinates(x, y) {
        var offset = getOffset(grid);
        var worldX = x / scale;
        var worldY = y / scale;
        if (zoom) {
            worldX += zoomBoundingBox.start.x;
            worldY += zoomBoundingBox.start.y;
        } else {
            worldX += offset.x;
            worldY += offset.y;
        }
        return {
            x: worldX,
            y: worldY
        };
    }

    var tooltipDiv = d3.select("body").append('div')
        .styles({
            position: "absolute",
            "text-align": "left",
            padding: "2px",
            font: "12px courier",
            border: "0px",
            "border-radius": "8px",
            "pointer-events": "none",
            opacity: 0,
            "z-index": 99996,
            "background-color": "lightgrey",
            "max-width": "calc(100% - 10px)",
            "max-height": "calc(100% - 40px)",
            overflow: "hidden"
        });

    var selectionDiv = d3.select("body").append('div')
        .styles({
            position: "absolute",
            "text-align": "left",
            border: "2px solid red",
            "pointer-events": "none",
            opacity: 0,
            "z-index": 99996,
            "background-color": "lightgrey"
        });

    function tooltipStringifyFilter(name, val) {
        /*if(name === "position"){
            return undefined;
        }*/
        return val;
    }

    function showTooltip(d) {
        var html = formatJSON(JSON.stringify(d, tooltipStringifyFilter, 2), true);
        apply_transition(tooltipDiv.html(html), animDuration).style('opacity', 0.95);
    }

    function formatJSON(json, preformatted) {
        json = json.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
        var result = json.replace(/("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)/g, function (match) {
            var color = 'brown'; // number
            if (/^"/.test(match)) {
                if (/:$/.test(match)) {
                    match = match.slice(1, -2) + ":";
                    color = 'blue'; // key
                } else {
                    color = 'green'; // string
                }
            } else if (/true|false/.test(match)) {
                color = 'magenta'; // boolean
            } else if (/null/.test(match)) {
                color = 'red'; //null
            }
            return '<span style="color:' + color + '">' + match + '</span>';
        });
        if(preformatted) {
            result = "<pre>" + result + "</pre>";
        }
        return result;
    }

    function moveTooltip() {
        // Position the tooltip without letting it go outside the window.
        var availableHeight = document.documentElement.clientHeight;
        var availableWidth = document.documentElement.clientWidth;

        var d = tooltipDiv.node();
        var tooltipHeight = d.scrollHeight;
        var tooltipWidth = d.scrollWidth;

        var left = d3.event.pageX + 15;
        if(left + tooltipWidth + 5 > availableWidth) {
            left = Math.max(availableWidth-tooltipWidth-5, 5);
        }
        var top = d3.event.pageY + 15;
        if(top + tooltipHeight + 5 > availableHeight) {
            top = Math.max(availableHeight-tooltipHeight-5, 30);
        }

        tooltipDiv
            .style('left', left + "px")
            .style('top', top + "px");
    }

    function hideTooltip() {
        apply_transition(tooltipDiv, animDuration).style('opacity', 0);
    }

    function apply_transition(d3Obj, duration) {
        if (!document.hidden) {
            return d3Obj.transition().duration(duration);
        }
        return d3Obj;
    }

    // Zoom selection helper functions
    function showZoomSelection(start) {
        selectionDiv
            .style('opacity', 0.5)
            .style('width', 0)
            .style('height', 0)
            .style('left', start.x + 'px')
            .style('top', start.y + 'px');
    }

    function hideZoomSelection() {
        selectionDiv
            .style('opacity', 0);
    }

    function updateZoomSelection(start, end) {
        var w = end.x - start.x;
        var h = end.y - start.y;
        selectionDiv
            .style('width', w + 'px')
            .style('height', h + 'px')
            .style('opacity', 0.5);
    }

    // Zoom bounding box.
    var zoom = false;
    var isMouseDown = false;
    var zoomBoundingBox = {};
    var cmpStartAdded = false;
    canvas.on('mousedown', function() {
        hideTooltip();
        var world = toWorldCoordinates(d3.event.offsetX, d3.event.offsetY);
        if(!d3.event.shiftKey) {
            isMouseDown = true;
            zoomBoundingBox.start = world;
            zoomBoundingBox.rawStart = { // Needed to draw zoom selection.
                x: d3.event.offsetX,
                y: d3.event.offsetY
            };
            // Need to use pageX/Y for positioning the div counting for the dialog offset.
            showZoomSelection({x: d3.event.pageX, y: d3.event.pageY});
        } else { // TODO: How to simplify this code ?
            var x = Math.trunc(world.x / squareSize);
            var y = Math.trunc(world.y / squareSize);
            if (!cmpStartAdded) {
                compareViewParameters[':binstr_start_x'].val = x.toString(10);
                compareViewParameters[':binstr_start_y'].val = y.toString(10);
                cmpStartAdded = true;
            } else {
                compareViewParameters[':binstr_end_x'].val = x.toString(10);
                compareViewParameters[':binstr_end_y'].val = y.toString(10);
                helper.runView(comparisonViewName, function(data, p_closeFn) {
                    console.log('result of the comparison view', data);
                    // run the view for side effect, not interested in response
                    p_closeFn();
                }, compareViewParameters);
                endComparison();
            }
        }
    });

    // Drag should be bigger than zoomThreshold pixels for zoom to be activated.
    canvas.on('mouseup', function() {
        console.log('mouseup');
        hideZoomSelection();
        if(!isMouseDown) { return; }
        isMouseDown = false;
        var end = toWorldCoordinates(d3.event.offsetX, d3.event.offsetY);
        if(zoomBoundingBox.start &&
            end.x - zoomBoundingBox.start.x > zoomThreshold && 
            end.y - zoomBoundingBox.start.y > zoomThreshold) {
                zoom = true;
                zoomBoundingBox.end = end;
                render();
        }
    })

    canvas.on('dblclick', function() {
        console.log('reset zoom');
        zoom = false;
        render();
    })

    var currentSquare;
    canvas.on('mousemove', function() {
        if (isMouseDown) {
            var end = {
                x: d3.event.offsetX,
                y: d3.event.offsetY
            };
            if(zoomBoundingBox.rawStart &&
                end.x - zoomBoundingBox.rawStart.x > zoomThreshold &&
                end.y - zoomBoundingBox.rawStart.y > zoomThreshold) {
                    updateZoomSelection(zoomBoundingBox.rawStart, end);
            } else {
                hideZoomSelection();
            }
            return;
        }

        var world = toWorldCoordinates(d3.event.offsetX, d3.event.offsetY);
        var x = Math.trunc(world.x / squareSize);
        var y = Math.trunc(world.y / squareSize);
        var squareId = x + '_' + y;
        if (currentSquare !== squareId) {
            currentSquare = squareId;
            if(grid.squares[squareId]) {
                var currentCell = grid.squares[squareId];
                showTooltip({
                    x: x,
                    y: y,
                    type: currentCell.type,
                    functional: currentCell.functional,
                    frame: currentCell.frame,
                    fill: currentCell.fill,
                    zoom: zoom
                });
                moveTooltip();
            } else {
                hideTooltip();
            }
        } else {
            moveTooltip();
        }
    });
    
    canvas.on('mouseout', function() {
        hideTooltip();
        endComparison();
    });

    var comparison = false;
    function startComparison() {
        // Set pointer for compare view
        container.style('cursor', 'crosshair');
        comparison = true;
    }

    function endComparison() {
        comparison = false;
        cmpStartAdded = false;
        container.style('cursor', 'default');
    }

    container.on('keydown', function() {
        if(d3.event.key === 'Shift' && !comparison) {
            console.log('Shift keydown detected');
            startComparison();
        }
    });

    container.on('keyup', function() {
        if(d3.event.key === 'Shift') {
            console.log('Shift keyup detected:');
            endComparison();
        }
    });

    var grid = initGrid();
    resize(width, height);

    // Call to hidden view setting the background
    var closeFn;
    var onBgData = function(data, p_closeFn) {
        closeFn = p_closeFn; // Keep the close function and restart on reset.
        if(data.length === 0) {
            return;
        }

        // Process data and add draw here.
        updateGrid(data, grid);

        // Add new rectangles.
        var rects = base.selectAll('custom.rect')
            .data(entries(grid.squares), function(d) { return d.id; });

        // remmove ? or only change color
        rects.exit().remove();

        rects.enter()
            .append('custom')
            .attr('class', 'rect')
            .attr('x', function(d) { return d.x; })
            .attr('y', function(d) { return d.y; })
            .attr('width', squareSize)
            .attr('height', squareSize);

        // Update bg colors
        base.selectAll('custom.rect')
            .attr('stroke', function(d) { return d.frame; })
            .attr('fill', function(d) { return d.fill; });

        render();
    }

    function render() {
        clearCanvas(context);
        updateViewBox(grid)

        // Render bg
        var rects = base.selectAll('custom.rect');
        rects.each(function(d, i) {
            var node = d3.select(this); 
            // This is each individual element in the loop. 

            // Here you retrieve the colour from the individual in-memory node and set the fillStyle for the canvas paint
            context.fillStyle = node.attr('fill');
            context.strokeStyle = node.attr('stroke');

            context.beginPath();
            // Convert to numbers, reducing the size to have a small margin.
            var x = (+node.attr('x')) + 1;
            var y = (+node.attr('y')) + 1;
            var w = (+node.attr('width')) - 1;
            var h = (+node.attr('height')) - 1;
            // Reduce the size to have a small margin
            context.rect(x, y, w, h);

            // Here you retrieve the position of the node and apply it to the fillRect context function which will fill and paint the square.
            // leaving one more pixel fo
            context.fillRect(+x, y, w, h);
            context.stroke();
        });

        // Render fg
        var circles = base.selectAll('custom.circle');
        circles.each(function(d, i) {
            var node = d3.select(this);

            context.fillStyle = node.attr('fill');
            context.strokeStyle = node.attr('stroke');

            context.beginPath();
            context.arc(node.attr('x'), node.attr('y'), radius, 0, 2 * Math.PI);
            context.fill();
            context.stroke();
        });
    }

    helper.runView(viewName, onBgData, {}, 'passthrough');

    function updateTrack(rows, prevGrid) {
        rows.forEach(function(row) {
            var id = row.ckey_1;

            if(row.op === 'del') {
                delete prevGrid.circles[id];
                return;
            }

            prevGrid.circles[id] = {
                x: row.x_2 * squareSize + squareSize / 2 + 1,
                y: row.y_3 * squareSize + squareSize / 2 + 1,
                frame: row.frame_4,
                fill: row.fill_5
            };
        });
    }

    return {
        on_data: function(data) {
            if(data.length === 0) { return; }

            // Process data and add draw here.
            updateTrack(data, grid);

            var circles = base.selectAll('custom.circle')
                .data(entries(grid.circles), function(d) { return d.id; });

            circles.exit().remove();

            circles.enter()
                .append('custom')
                .attr('class', 'circle')
                .attr('r', radius)
                .attr('x', function(d) { return d.x; })
                .attr('y', function(d) { return d.y; });

            base.selectAll('custom.circle')
                .attr('x', function(d) { return d.x; })
                .attr('y', function(d) { return d.y; })
                .attr('stroke', function(d) { return d.frame; })
                .attr('fill', function(d) { return d.fill; });

            render();

        },
        on_resize: resize,
        on_reset: function() {
            // Called when the button clear the graph is clicked.
            clearCanvas(context);
            base.selectAll('custom').remove();
            grid = initGrid();
            if(typeof closeFn == 'function') {
                closeFn();
            }
            helper.runView(viewName, onBgData, {}, 'passthrough');
        },
        on_close: function() {
            // This should cleanup event listeners and element added
            // outside the container, the container itself will be removed
            // after this function call.
            tooltipDiv.remove();
            selectionDiv.remove();
        }
    };
}
