function init(container, width, height) {

    var dom = {lin:'linear',log:'log',time:'time'}; // domain types
    var tParseEuL = helper.tParseEuL; // timeParse with msec
    var tParseEu = helper.tParseEu;    // timeParse without msec
    var tParseInt = helper.tParseInt;    // timeParse international format without msec
    var tParseIntL = helper.tParseIntL; // timeParse international format
    var yAxisInverted = false; // set it to true to set the 0,0 at the top left

    function setup() {
        xDom = dom.lin;         // dom.lin | dom.log | dom.time 
        yDom = dom.lin;         // dom.lin | dom.log | dom.time 
        yCount = 4;             // 1..4, set to 1 for only 1 y-Value
        margin = { top: 20, right: 20, bottom: 50, left: 90 };  // physical margins in px
        switch (xDom) {
        case dom.lin:
            xParse = helper.parseFloat;
            xScaleTemplate = d3.scaleLinear();
            xMin = 1e100, xMax = -1e100;                // autoscale defaults
            xAutoscale = true;
            xAllowance = 0.05;
            xTickCount = 10;
            xTickFormatSpecifier = null;      
        break;
        case dom.log:
            xParse = helper.parseFloat;
            xScaleTemplate = d3.scaleLog().nice();
            xMin = 1e100, xMax = 1e-100;                // autoscale defaults
            xAutoscale = true;
            xAllowance = 0.33;
            xTickCount = 10;
            xTickFormatSpecifier = ".0s";
        break;
        case dom.time:
            xParse = helper.parseTime;
            xScaleTemplate = d3.scaleUtc();    // scaleTime
            xMin = 1e100;   // xParse("01.01.2300 00:00:00.000");   // autoscale defaults
            xMax = 0;       // xParse("01.01.1900 00:00:00.000");   // autoscale defaults
            xAutoscale = true;
            xAllowance = 0.05;
            xTickCount = 8;
            xTickFormatSpecifier = "%H:%M:%S";
            /*
            %Y - for year boundaries, such as 2011.
            %B - for month boundaries, such as February.
            %b %d - for week boundaries, such as Feb 06.
            %a %d - for day boundaries, such as Mon 07.
            %I %p - for hour boundaries, such as 01 AM.
            %H:%M - for minute boundaries, such as 01:23.
            :%S - for second boundaries, such as :45.
            .%L - milliseconds for all other times, such as .012.
            */
        break;
        };
        switch (yDom) {
        case dom.lin:
            yParse = helper.parseFloat;
            yScaleTemplate = d3.scaleLinear();
            yMin = 1e100, yMax = -1e100;                // autoscale defaults
            yAutoscale = true;
            yAllowance = 0.05;
            yTickCount = 10;
            yTickFormatSpecifier = null;      
            /*
            null        // automatic
            "%"         // percentage, "12%"
            ".0%"       // rounded percentage, "12%"
            "($.2f"     // localized fixed-point currency, "(Â£3.50)"
            "+20"       // space-filled and signed, "                 +42"
            ".^20"      // dot-filled and centered, ".........42........."
            "s"         // SI-prefix
            ".2s"       // SI-prefix with two significant digits, "42M"
            "#x"        // prefixed lowercase hexadecimal, "0xbeef"
            ",.2r"      // grouped thousands with two significant digits, "4,200"
            */
        break;
        case dom.log:
            yParse = helper.parseFloat;
            yScaleTemplate = d3.scaleLog().nice();
            yMin = 1e100, yMax = 1e-100;                // autoscale defaults
            yAutoscale = true;
            yAllowance = 0.33;  // cannot be <= 0
            yTickCount = 10;
            yTickFormatSpecifier = ".0s";
        break;
        case dom.time:
            yParse = helper.parseTime;
            yScaleTemplate = d3.scaleUtc();     // scaleTime
            yMin = 1e100;   // yParse("01.01.2300 00:00:00.000");   // autoscale defaults
            yMax = 0;       // yParse("01.01.1900 00:00:00.000");   // autoscale defaults
            yAutoscale = true;
            yAllowance = 0.05;
            yTickCount = 8;
            yTickFormatSpecifier = "%H:%M:%S";
            /*
            %Y - for year boundaries, such as 2011.
            %B - for month boundaries, such as February.
            %b %d - for week boundaries, such as Feb 06.
            %a %d - for day boundaries, such as Mon 07.
            %I %p - for hour boundaries, such as 01 AM.
            %H:%M - for minute boundaries, such as 01:23.
            :%S - for second boundaries, such as :45.
            .%L - milliseconds for all other times, such as .012.
            */
        break;
        };
        radius = 3;     // circle radius
        a = 3;          // half of square edge size
        fsLeg = '1.3em';
        xLeg =  margin.left + 60;     
        yLeg =  margin.top + 20;
        dxLeg = 13;     // Legend horizontal spacer
        dyLeg = 18;     // Legend vertical spacer
    }

    var margin;
    var cWidth, cHeight;        // main physical content size in px
    var xVar, y1Var, y2Var, y3Var, y4Var;   // sql column aliases
    var xDom, xParse, xScaleTemplate, xScale, xAutoscale, xAllowance;
    var yDom, yParse, yScaleTemplate, yScale, yAutoscale, yAllowance;
    var xMin, xMax, yMin, yMax;   // current zoom domain extent with Allowance
    var xMinFull, xMaxFull, yMinFull, yMaxFull; // raw full domain extent
    var gxAxis, xAxis, xText, gyAxis, yAxis, yTexts;
    var xTickCount, xTickFormatSpecifier, yTickCount, yTickFormatSpecifier;

    var svg = container.append('svg');
    // Added random id suffix to avoid clip id conflict with multiple graphs
    var clipId = "clip_" + Math.random().toString(36).substr(2, 14);
    var clipPath = svg.append("defs").append("clipPath").attr("id", clipId).append("rect");
    var g1, g2, g3, g4;                 // data point groups
    var xLeg, yLeg, dxLeg, dyLeg, xLegStart, yLegStart, fsLeg;  // legend position and font size
    var zoom = svg.append("g").attr("class", "brush_zoom");
    var brush_zoom = d3.brush().on("end", brush_zoom_ended);    // zoom brush
    var g = svg.append("g");                    // main svg item group (axes, point groups)
    var gxTitle = svg.append("g");              // x axis title group
    var gLeg = svg.append("g");                 // legend group
    var idleTimeout, idleDelay = 350;

    function init() {
        g.selectAll('svg > g > *').remove();
        g.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
        g1 = g.append("g");
        g2 = g.append("g");
        g3 = g.append("g");
        g4 = g.append("g");
        xMinFull = xMin;
        xMaxFull = xMax;
        yMinFull = yMin;
        yMaxFull = yMax;
        noData = true;
        brush_zoom.extent([[0,0],[2000,2000]]);
    }

    function idled() {
        idleTimeout = null;
    }

    function brush_zoom_ended() {
        var s = d3.event.selection;
        if (!s) {
            if (!idleTimeout) return idleTimeout = setTimeout(idled, idleDelay);
            xAutoscale = true;
            yAutoscale = true;
            xMin = xMinFull;
            xMax = xMaxFull;
            yMin = yMinFull;
            yMax = yMaxFull;
        } else {
            xMin = xScale.invert(s[0][0]-margin.left);
            xMax = xScale.invert(s[1][0]-margin.left);
            xAutoscale = ((s[1][0]-s[0][0]) >= cWidth) ;
            yMin = yScale.invert(s[1][1]-margin.top);
            yMax = yScale.invert(s[0][1]-margin.top);
            yAutoscale = ((s[1][1]-s[0][1]) >= cHeight);
            svg.select(".brush_zoom").call(brush_zoom.move, null);
        }
        resize(width, height);
    }

    var drag = d3.drag()
        .subject(dragsubject)
        .on("start", dragstarted)
        .on("drag", dragged); 

    function dragsubject() {
      return d3.select(this);
    }

    function dragstarted() {
        var translation = getTranslation(d3.event.subject.attr("transform"));
        d3.event.subject.fx = d3.event.x - translation[0];
        d3.event.subject.fy = d3.event.y - translation[1];
    }

    function dragged() {
        var x = d3.event.x - d3.event.subject.fx;
        var y = d3.event.y - d3.event.subject.fy;
        d3.event.subject.attr("transform", "translate(" + x + "," + y + ")");
    }

    function getTranslation(transform) {
        var g = document.createElementNS("http://www.w3.org/2000/svg", "g");
        g.setAttributeNS(null, "transform", transform);
        var matrix = g.transform.baseVal.consolidate().matrix;
        return [matrix.e, matrix.f];
    }

    var idVal = function(d) {
        return d.id;    // d.label_4; 
    }

    var xVal = function(d) {
        return xParse(d[xVar]);
    }

    var y1Val = function(d) {
        return yParse(d[y1Var]);
    }

    var y2Val = function(d) {
        return yParse(d[y2Var]);
    }

    var y3Val = function(d) {
        return yParse(d[y3Var]);
    }

    var y4Val = function(d) {
        return yParse(d[y4Var]);
    }

    function contextMenu(d) {
        d3.event.preventDefault();
        var menuSpec = [
            {
                label: "Add label",
                icon: "file-text-o",
                cb: function(evt) {
                    var pos = d3.clientPoint(svg.node(), evt);
                    helper.createLabel(svg, pointTitle(d), pos[0], pos[1]);
                }
            }, 
            {
                label: "Copy",
                icon: "copy",
                cb: function(evt) {
                    window.prompt("Copy to clipboard: Ctrl+C, Enter", JSON.stringify(d));
                }
            }
        ];
        var pos = {x: d3.event.pageX - 15, y: d3.event.pageY - 20};
        helper.contextMenu(menuSpec, pos);
    }

    var pointTitle = function(d) { 
        var res = '';
        for (prop in d) {
            var sp = prop.split('_');
            if (prop != 'id' && prop != 'op') {
                sp.pop();
                res += sp.join('_') + ': ' + d[prop] + '\n';
            }
        }
        return res;
    };

    var xAxisVar = function(d) { 
        var res = '';
        for (prop in d) {
            var sp = prop.split('_');
            if (sp.pop() == '1') {
                res += sp.join('_');
            }
        }
        return res;
    };

    var yAxisVar = function(i,d) { 
        var res = '';
        for (prop in d) {
            var sp = prop.split('_');
            if (sp.pop() == i+1) {
                res += sp.join('_');
            }
        }
        return res;
    };

    var circleAttrs1 = function(d) { 
        // console.log("xVal(d):" + xVal(d));
        return {
            'clip-path': 'url(#' + clipId + ')',
            cx: xScale(xVal(d)),
            cy: yScale(y1Val(d)),
            r: radius   // r: radius + radius - d.label_4.split('_').length
        };
    };

    var squareAttrs2 = function(d) { 
        return {
            'clip-path': 'url(#' + clipId + ')',
            x: xScale(xVal(d))-a,
            y: yScale(y2Val(d))-a,
            width: a+a,
            height: a+a
        };
    };

    var circleAttrs3 = function(d) { 
        return {
            'clip-path': 'url(#' + clipId + ')',
            cx: xScale(xVal(d)),
            cy: yScale(y3Val(d)),
            r: radius
        };
    };

    var squareAttrs4 = function(d) { 
        return {
            'clip-path': 'url(#' + clipId + ')',
            x: xScale(xVal(d))-a,
            y: yScale(y4Val(d))-a,
            width: a+a,
            height: a+a
        };
    };

    var circleStyles1 = function(d) { 
        var obj = {
            fill: "steelblue"
        };
        return obj;
        /*
        var color;
        switch (d.label_4.split('_').length) {
        case 1: 
            color = "red";
            break;
        case 2: 
            color = "steelblue";
            break;
        case 3: 
            color = "green";
            break;
        default:
            color = "black";
        };
        return {fill: color};
        */
    };

    var squareStyles2 = function(d) { 
        var obj = {
            fill: "red"
        };
        return obj;
    };

    var circleStyles3 = function(d) { 
        var obj = {
            fill: "green"
        };
        return obj;
    };

    var squareStyles4 = function(d) { 
        var obj = {
            fill: "black"
        };
        return obj;
    };

    function printHorTitle(x,y,text) {
        gxTitle.append("text")
            .text(text)
            .attr("x", 0)
            .attr("y", 0)
            .attr('font-size', fsLeg)
            .attr('font-family','sans-serif')
            .attr('fill','#000')
            .style("text-anchor", "end");
        gxTitle.attr("transform", "translate(" + x + "," + y + ")");
    };

    function printLegendText(x,y,text) {
        gLeg.append("text")
            .text(text)
            .attr("x",x)
            .attr("y",y)
            .attr('font-size', fsLeg)
            .attr('font-family','sans-serif')
            .attr('fill','#000');
    };

    function printLegendCircle(x,y,st) {
        gLeg.append("svg:circle")
            .attr("cx",x)
            .attr("cy",y-radius-2)
            .attr("r",radius)
            .styles(st);
    };

    function printLegendSquare(x,y,st) {
        gLeg.append("svg:rect")
            .attr("x",x-a)
            .attr("y",y-a-a-2)
            .attr("width",a+a)
            .attr("height",a+a)
            .styles(st);
    };

    function xGrow(xMinNew,xMaxNew) {
        var res = false;
        if (xMinNew < xMinFull) {
            xMinFull = xMinNew;
        }
        if (xMaxNew > xMaxFull) {
            xMaxFull = xMaxNew;
        }
        if (xAutoscale) {
            if (xDom == dom.log) {
                if (xMinNew < xMin) {
                    xMin = xMinNew * xAllowance;
                    res = true;
                }
                if (xMaxNew > xMax) {
                    xMax = xMaxNew / xAllowance;
                    res = true;
                }
            } else {
                if (xMinNew < xMin) {
                    xMin = xMinNew - xAllowance * (xMaxNew-xMinNew);
                    res = true;
                }
                if (xMaxNew > xMax) {
                    xMax = xMaxNew + xAllowance * (xMaxNew-xMinNew);
                    res = true;
                }
            }
        }
        return res;
    }


    function yGrow(yMinNew,yMaxNew) {
        var res = false;
        if (yMinNew < yMinFull) {
            yMinFull = yMinNew;
        }
        if (yMaxNew > yMaxFull) {
            yMaxFull = yMaxNew;
        }
        if (yAutoscale) {
            if (yDom == dom.log) {
                if (yMinNew < yMin) {
                    yMin = yMinNew * yAllowance; 
                    res = true;
                }
                if (yMaxNew > yMax) {
                    yMax = yMaxNew / yAllowance;
                    res = true;
                }
            } else {
                if (yMinNew < yMin) {
                    yMin = yMinNew - yAllowance * (yMaxNew-yMinNew);
                    res = true;
                }
                if (yMaxNew > yMax) {
                    yMax = yMaxNew + yAllowance * (yMaxNew-yMinNew);
                    res = true;
                }
            }
        }
        return res;
    }

    function resize(w, h) {
        console.log("resize called");
        width = w;
        height = h;
        cWidth = width - margin.left - margin.right;
        cHeight = height - margin.top - margin.bottom;
        svg.attr('width', width).attr('height', height);
        if (!noData) {
            rescale();
            gxAxis.remove();
            gxAxis = g.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(0," + cHeight + ")")
                .call(xAxis);
            gyAxis.transition().call(yAxis);  // Update Y-Axis
            gxTitle.attr("transform", "translate(" + (w-margin.right) + "," + (h-dyLeg) + ")");

            g1.selectAll("circle").transition().attrs(circleAttrs1);
            if (yCount >= 2) {
                g2.selectAll("rect").transition().attrs(squareAttrs2);
            }
            if (yCount >= 3) {
                g3.selectAll("circle").transition().attrs(circleAttrs3);
            }
            if (yCount >= 4) {
                g4.selectAll("rect").transition().attrs(squareAttrs4);
            }
        }
        clipPath.attr("width", cWidth);
        clipPath.attr("height", cHeight);
    }

    function rescale() {
        if (xMin >= xMax) {
            xScale = xScaleTemplate.domain([xMax-1, xMin+1]).range([0, cWidth]);
        } else {
            xScale = xScaleTemplate.domain([xMin, xMax]).range([0, cWidth]);
        }

        var range = yAxisInverted ? [0, cHeight] : [cHeight, 0];

        if (yMin >= yMax) {
            yScale = yScaleTemplate.domain([yMax-1, yMin+1]).range(range);
        } else {
            yScale = yScaleTemplate.domain([yMin, yMax]).range(range);
        }
        xAxis = d3.axisBottom(xScale).ticks(xTickCount, xTickFormatSpecifier);
        yAxis = d3.axisLeft(yScale).ticks(yTickCount, yTickFormatSpecifier);
    }

    setup();
    init();
    resize(width, height);
    zoom.call(brush_zoom);
    gLeg.call(drag);
    gxTitle.call(drag);

    return {

        on_data: function(data) {

            if (data.length === 0) {return;}

            if (noData) {
                var x = margin.left + xLeg + dxLeg;
                var y = margin.top + yLeg;
                xVar = xAxisVar(data[0]);
                xText = xVar;
                xVar = xVar + '_1';
                y1Var = yAxisVar(1,data[0]) 
                yTexts = [y1Var];
                y1Var = y1Var + '_2';
                printLegendCircle(0,0,circleStyles1(data[0]));
                printLegendText(dxLeg,0,yTexts[0]);
                if (yCount >= 2) {
                    y2Var = yAxisVar(2,data[0]);
                    yTexts.push(y2Var);
                    y2Var = y2Var + '_3';
                    printLegendSquare(0,dyLeg,squareStyles2(data[0]));
                    printLegendText(dxLeg,dyLeg,yTexts[1]);
                };
                if (yCount >= 3) {
                    y3Var = yAxisVar(3,data[0]);
                    yTexts.push(y3Var);
                    y3Var = y3Var + '_4';
                    printLegendCircle(0,dyLeg+dyLeg,circleStyles3(data[0]));
                    printLegendText(dxLeg,dyLeg+dyLeg,yTexts[2]);
                };
                if (yCount >= 4) {
                    y4Var = yAxisVar(4,data[0]);
                    yTexts.push(y4Var);
                    y4Var = y4Var + '_5';
                    printLegendSquare(0,dyLeg+dyLeg+dyLeg,squareStyles4(data[0]));
                    printLegendText(dxLeg,dyLeg+dyLeg+dyLeg,yTexts[3]);
                };
                gLeg.attr("transform", "translate(" + xLeg + "," + yLeg + ")");
                printHorTitle(width-margin.right,height-dyLeg,xText);
            }
            var xG = xGrow(Math.min(xMin, d3.min(data, xVal)), Math.max(xMax, d3.max(data, xVal)));
            var yG = yGrow(Math.min(yMin, d3.min(data, y1Val)), Math.max(yMax, d3.max(data, y1Val)));
            if (yCount >= 2) {
                yG = (yGrow(Math.min(yMin, d3.min(data, y2Val)), Math.max(yMax, d3.max(data, y2Val))) || yG);
            }; 
            if (yCount >= 3) {
                yG = (yGrow(Math.min(yMin,d3.min(data, y3Val)), Math.max(d3.max(data, y3Val))) || yG);
            }; 
            if (yCount >= 4) {
                yG = (yGrow(Math.min(yMin,d3.min(data, y4Val)), Math.max(d3.max(data, y4Val))) || yG);
            }; 

            rescale();

            if (noData) {
                noData = false;
                gxAxis = g.append("g")
                    .attr("class", "x axis")
                    .attr("transform", "translate(0," + cHeight + ")")
                    .call(xAxis);
                gyAxis = g.append("g")
                    .attr("class", "y axis")
                    .call(yAxis);
            } else {
                if (xG) {
                    gxAxis
                    .attr("transform", "translate(0," + cHeight + ")")
                    .transition().call(xAxis);  // Update X-Axis
                }
                if (yG) {
                    gyAxis.transition().call(yAxis);  // Update Y-Axis
                }
            }

            var points1 = g1.selectAll("circle");
            if (xG || yG) {
                points1.transition().attrs(circleAttrs1);
            }
            points1.data(data, idVal)
                .enter()
                .append("svg:circle")
                .attrs(circleAttrs1)
                .styles(circleStyles1)
                .on('contextmenu', contextMenu)
                .append("title")
                .text(pointTitle)
                ;

            if (yCount >= 2) {
                var points2 = g2.selectAll("rect");
                if (xG || yG) {
                    points2.transition().attrs(squareAttrs2);
                }
                points2.data(data, idVal)
                    .enter()
                    .append("svg:rect")
                    .attrs(squareAttrs2)
                    .styles(squareStyles2)
                    .on('contextmenu', contextMenu)
                    .append("title")
                    .text(pointTitle)
                    ;
            };
            if (yCount >= 3) {
                var points3 = g3.selectAll("circle");
                if (xG || yG) {
                    points3.transition().attrs(circleAttrs3);
                }
                points3.data(data, idVal)
                    .enter()
                    .append("svg:circle")
                    .attrs(circleAttrs3)
                    .styles(circleStyles3)
                    .on('contextmenu', contextMenu)
                    .append("title")
                    .text(pointTitle)
                    ;
            };
            if (yCount >= 4) {
                var points4 = g4.selectAll("rect");
                if (xG || yG) {
                    points4.transition().attrs(squareAttrs4);
                }
                points4.data(data, idVal)
                    .enter()
                    .append("svg:rect")
                    .attrs(squareAttrs4)
                    .styles(squareStyles4)
                    .on('contextmenu', contextMenu)
                    .append("title")
                    .text(pointTitle)
                    ;
            };
        },

        on_resize: function(w, h) {
            resize(w, h);
        },

        on_reset: function() {
            setup();
            init(); 
        },

        on_close: function() {
            
        }
    };
}
