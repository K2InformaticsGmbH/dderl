function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // container: d3 selection of the contaner div for the graph
    // width: width of the container
    // height: height of the container
    // The function must then return an object with the following callbacks:
    /*
    select 0.01 * item as x
        , 0.01 * item + 0.1 * sin(0.1 * item) as sinXscaled
        , 1.0 + 0.01 * item * cos(0.1 * item) as cosXscaled
        , sin(0.2 * item) / (0.2 * item + 1) as sinDamped
        , cos(0.3 * item) / (0.01 * item + 1) as cosDamped 
    from integer 
    where item >= 0 and item <= 270   

    select time, memory from ddMonitor_86400@ 
    */

    var margin;
    var cWidth, cHeight;							// main physical content size in px
    var xScale, yScale;
    var xMin, xMax;
    var yMin, yMax; 
    var xTickCount, xTickFormatSpecifier;      
    var yTickCount, yTickFormatSpecifier;      
    var xAxisGroup, xMinFull, xMaxFull, xAxis, xVar, xText;
    var yAxisGroup, yMinFull, yMaxFull, yAxis, y1Var, y2Var, y3Var, y4Var, yText;
    var xDom, xScaleTemplate, xAutoscale, xAllowance;
    var yDom, yScaleTemplate, yAutoscale, yAllowance;
    var radius;
    var a;
    var dom = {lin:'linear',log:'log',time:'time'};
    var yCount;
    var firstData
    var svg = container.append('svg');
    var clipPath = svg.append("defs").append("clipPath").attr("id", "clip").append("rect");
    var br = svg.append("g").attr("class", "brush");
    var g = svg.append("g");    // main svg item group (axes, point groups)
    var g1, g2, g3, g4;         // point groups
    var brush = d3.brush().on("end", brushended);
    var idleTimeout;
    var idleDelay = 350;
    var tParse;

    function setup() {
        xDom = dom.lin;         // dom.lin | dom.log | dom.time 
        yDom = dom.lin;         // dom.lin | dom.log | dom.time 
        yCount = 1;             // 1..4, set to 1 for only 1 y-Value
        margin = { top: 20, right: 20, bottom: 50, left: 90 };  // physical margins in px
        switch (xDom) {
        case dom.lin:
            xScaleTemplate = d3.scaleLinear();
            xMin = 1e100, xMax = -1e100;                // autoscale defaults
            xAutoscale = true;
            xAllowance = 0.05;
            xTickCount = 10;
            xTickFormatSpecifier = null;      
        break;
        case dom.log:
            xScaleTemplate = d3.scaleLog().nice();
            xMin = 1e100, xMax = 1e-100;                // autoscale defaults
            xAutoscale = true;
            xAllowance = 0.33;
            xTickCount = 10;
            xTickFormatSpecifier = ".0s";      
        break;
        case dom.time:
            tParse = d3.timeParse("%d.%m.%Y %H:%M:%S.%L");
            xScaleTemplate = d3.scaleTime();
            xMin = tParse("01.01.2300 00:00:00.000");   // autoscale defaults
            xMax = tParse("01.01.1900 00:00:00.000");   // autoscale defaults
            xAutoscale = true;
            xAllowance = 0.05;
            xTickCount = 8;
            xTickFormatSpecifier = "%I:%M %p";
            /*
            %Y - for year boundaries, such as 2011.
            %B - for month boundaries, such as February.
            %b %d - for week boundaries, such as Feb 06.
            %a %d - for day boundaries, such as Mon 07.
            %I %p - for hour boundaries, such as 01 AM.
            %I:%M - for minute boundaries, such as 01:23.
            :%S - for second boundaries, such as :45.
            .%L - milliseconds for all other times, such as .012.
            */
        break;
        };
        switch (yDom) {
        case dom.lin:
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
            yScaleTemplate = d3.scaleLog().nice();
            yMin = 1e100, yMax = 1e-100;                // autoscale defaults
            yAutoscale = true;
            yAllowance = 0.33;  // cannot be <= 0
            yTickCount = 10;
            yTickFormatSpecifier = ".0s";      
        break;
        case dom.time:
            tParse = d3.timeParse("%d.%m.%Y %H:%M:%S.%L");
            yScaleTemplate = d3.scaleTime();
            yMin = tParse("01.01.2300 00:00:00.000");   // autoscale defaults
            yMax = tParse("01.01.1900 00:00:00.000");   // autoscale defaults
            yAutoscale = true;
            yAllowance = 0.05;
            yTickCount = 8;
            yTickFormatSpecifier = "%I:%M %p";
            /*
            %Y - for year boundaries, such as 2011.
            %B - for month boundaries, such as February.
            %b %d - for week boundaries, such as Feb 06.
            %a %d - for day boundaries, such as Mon 07.
            %I %p - for hour boundaries, such as 01 AM.
            %I:%M - for minute boundaries, such as 01:23.
            :%S - for second boundaries, such as :45.
            .%L - milliseconds for all other times, such as .012.
            */
        break;
        };
        radius = 3;             // circle radius
        a = 3;                  // half of square edge size
    }

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
        firstData = true;
        brush.extent([[0,0],[2000,2000]]);
    }

    function idled() {
        idleTimeout = null;
    }

    function brushended() {
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
            svg.select(".brush").call(brush.move, null);
        }
        resize(width, height);
    }

    var idVal = function(d) {
        return d.id;
    }

    function parse(domain, str) {
        if (domain == dom.time) { 
            return tParse(str.substr(0,23));
        } else {
            return parseFloat(str);
        }
    }

    var xVal = function(d) {
        return parse(xDom, d[xVar]);
    }

    var y1Val = function(d) {
        return parse(yDom, d[y1Var]);
    }

    var y2Val = function(d) {
        return parse(yDom, d[y2Var]);
    }

    var y3Val = function(d) {
        return parse(yDom, d[y3Var]);
    }

    var y4Val = function(d) {
        return parse(yDom, d[y4Var]);
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

    var circleAttrs1 = function(d) { 
        return {
            cx: xScale(xVal(d)),
            cy: yScale(y1Val(d)),
            r: radius,
            'clip-path': 'url(#clip)'
        };
    };

    var circleAttrs3 = function(d) { 
        return {
            cx: xScale(xVal(d)),
            cy: yScale(y3Val(d)),
            r: radius,
            'clip-path': 'url(#clip)'
        };
    };

    var squareAttrs2 = function(d) { 
        return {
            x: xScale(xVal(d))-a,
            y: yScale(y2Val(d))-a,
            width: a+a,
            height: a+a,
            'clip-path': 'url(#clip)'
        };
    };

    var squareAttrs4 = function(d) { 
        return {
            x: xScale(xVal(d))-a,
            y: yScale(y4Val(d))-a,
            width: a+a,
            height: a+a,
            'clip-path': 'url(#clip)'
        };
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

    var circleStyles1 = function(d) { 
        var obj = {
            fill: "steelblue"
        };
        return obj;
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

    function xGrow(xMinNew,xMaxNew) {
        var res = false;
        if (xMinNew < xMinFull) {
            xMinFull = xMinNew;
        }
        if (xMaxNew > xMaxFull) {
            xMaxFull = xMaxNew;
        }
        if (xAutoscale && (xDom == dom.log)) {
            if (xMinNew < xMin) {
                xMin = xMinNew * xAllowance;
                res = true;
            }
            if (xMaxNew > xMax) {
                xMax = xMaxNew / xAllowance;
                res = true;
            }
        }
        if (xAutoscale && (xDom != dom.log)) {
            if (xMinNew < xMin) {
                xMin = xMinNew - xAllowance * (xMaxNew-xMinNew);
                res = true;
            }
            if (xMaxNew > xMax) {
                xMax = xMaxNew + xAllowance * (xMaxNew-xMinNew);
                res = true;
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
        if (yAutoscale && (yDom == dom.log)) {
            if (yMinNew < yMin) {
                yMin = yMinNew * yAllowance; 
                res = true;
            }
            if (yMaxNew > yMax) {
                yMax = yMaxNew / yAllowance;
                res = true;
            }
        }
        if (yAutoscale && (yDom != dom.log)) {
            if (yMinNew < yMin) {
                yMin = yMinNew - yAllowance * (yMaxNew-yMinNew);
                res = true;
            }
            if (yMaxNew > yMax) {
                yMax = yMaxNew + yAllowance * (yMaxNew-yMinNew);
                res = true;
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
        if (firstData === false) {
            rescale();
            xAxisGroup.remove();
            xAxisGroup = g.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(0," + cHeight + ")")
                .call(xAxis);

            xAxisGroup.append("text")
                .attr("x", cWidth)
                .attr("dx", "-0.71em")
                .attr("dy", "-0.71em")
                .attr('font-size', '1.5em')
                .attr('fill','#000')
                .style("text-anchor", "end")
                .text(xText);

            yAxisGroup.transition().call(yAxis);  // Update Y-Axis

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
            xScale = xScaleTemplate.domain([xMin-1, xMax+1]).range([0, cWidth]);
        } else {
            xScale = xScaleTemplate.domain([xMin, xMax]).range([0, cWidth]);
        }

        if (yMin >= yMax) {
            yScale = yScaleTemplate.domain([yMin-1, yMax+1]).range([cHeight, 0]);
        } else {
            yScale = yScaleTemplate.domain([yMin, yMax]).range([cHeight, 0]);
        }

        xAxis = d3.axisBottom(xScale).ticks(xTickCount, xTickFormatSpecifier);
        yAxis = d3.axisLeft(yScale).ticks(yTickCount, yTickFormatSpecifier);          
    }

    setup();
    init();
    resize(width, height);
    br.call(brush);

    return {

        on_data: function(data) {

            if (data.length === 0) {return;}

            if (firstData) {
                xVar = xAxisVar(data[0]);
                xText = xVar;
                xVar = xVar + '_1';
                y1Var = yAxisVar(1,data[0]) 
                yText = y1Var;
                y1Var = y1Var + '_2';
                if (yCount >= 2) {
                    y2Var = yAxisVar(2,data[0]);
                    yText = yText + ' / ' + y2Var;
                    y2Var = y2Var + '_3';
                };
                if (yCount >= 3) {
                    y3Var = yAxisVar(3,data[0]);
                    yText = yText + ' / ' + y3Var;
                    y3Var = y3Var + '_4';
                };
                if (yCount >= 4) {
                    y4Var = yAxisVar(4,data[0]);
                    yText = yText + ' / ' + y4Var;
                    y4Var = y4Var + '_5';
                };
            }
            var xG = xGrow(Math.min(xMin, d3.min(data, xVal)), Math.max(xMax, d3.max(data, xVal)));
            var yG = yGrow(Math.min(yMin, d3.min(data, y1Val)), Math.max(yMax, d3.max(data, y1Val)));
            if (yCount >= 2) {
                yG = (yG || yGrow(Math.min(yMin, d3.min(data, y2Val)), Math.max(yMax, d3.max(data, y2Val))));
            }; 
            if (yCount >= 3) {
                yG = (yG || yGrow(Math.min(yMin, d3.min(data, y3Val)), Math.max(yMax, d3.max(data, y3Val))));
            }; 
            if (yCount >= 4) {
                yG = (yG || yGrow(Math.min(yMin, d3.min(data, y4Val)), Math.max(yMax, d3.max(data, y4Val))));
            }; 

            rescale();

            if (firstData) {

                firstData = false;

                xAxisGroup = g.append("g")
                    .attr("class", "x axis")
                    .attr("transform", "translate(0," + cHeight + ")")
                    .call(xAxis);

                xAxisGroup.append("text")
                    .attr("x", cWidth)
                    .attr("dx", "-0.71em")
                    .attr("dy", "-0.71em")
                    .attr('font-size', '1.5em')
                    .attr('fill','#000')
                    .style("text-anchor", "end")
                    .text(xText);

                yAxisGroup = g.append("g")
                    .attr("class", "y axis")
                    .call(yAxis);

                yAxisGroup.append("text")
                    .attr("transform", "rotate(-90)")
                    .attr("y", 6)
                    .attr("dx", "-0.71em")
                    .attr("dy", ".71em")
                    .attr('font-size', '1.5em')
                    .attr('fill','#000')                    
                    .style("text-anchor", "end")
                    .text(yText);

            } else {
                if (xG) {
                    xAxisGroup
                    .attr("transform", "translate(0," + cHeight + ")")
                    .transition().call(xAxis);  // Update X-Axis
                }
                if (yG) {
                    yAxisGroup.transition().call(yAxis);  // Update Y-Axis
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
                .append("title")
                .html(pointTitle)
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
                    .append("title")
                    .html(pointTitle)
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
                    .append("title")
                    .html(pointTitle)
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
                    .append("title")
                    .html(pointTitle)
                    ;
            };
        },

        on_resize: function(w, h) {
            resize(w, h);
        },

        on_reset: function() {
            setup();
            init(); 
        }
    };
}