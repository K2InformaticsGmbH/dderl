function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are (container, width, height)

    // container: d3 selection of the contaner div for the graph
    // width: width of the container
    // height: height of the container

    /*
        select    0.01 * item as x
                , 0.01 * item + 0.1 * sin(0.1 * item) as y1
                , 1.0 + 0.01 * item * cos(0.1 * item) as y2 
        from integer where item >= 0 and item <= 270
    */

    // The function must then return an object with the following callbacks:

    var margin = { top: 20, right: 20, bottom: 30, left: 50 }; 	// physical margins in px
    var cWidth, cHeight;							// main physical content size in px
    var xScale, yScale;
    var xAxisGroup, xMinFull, xMaxFull;
    var yAxisGroup, yMinFull, yMaxFull;

    var xMin = 1e100, xMax = -1e100;    // autoscale defaults
    var yMin = 1e100, yMax = -1e100;    // autoscale defaults

    // xMin = ..., xMax = ....;         // set fixed initial values here
    var xTickCount = 10;
    var xTickFormatSpecifier = null;      
    /*
    "%"         // percentage, "12%"
    ".0%"       // rounded percentage, "12%"
    "($.2f"     // localized fixed-point currency, "(£3.50)"
    "+20"       // space-filled and signed, "                 +42"
    ".^20"      // dot-filled and centered, ".........42........."
    ".2s"       // SI-prefix with two significant digits, "42M"
    "#x"        // prefixed lowercase hexadecimal, "0xbeef"
    ",.2r"      // grouped thousands with two significant digits, "4,200"

    */
    // yMin = ..., yMax = ....;         // set fixed initial values here
    var yTickCount = 10;
    var yTickFormatSpecifier = "%";      
    var xAutoscale = true;
    var yAutoscale = true;
    var xAllowance = 0.05;
    var yAllowance = 0.05;
    var xAxis, xText = "x-Value";
    var yAxis, yText = "y-Value";
    var radius = 3;     // circle radius
    var a = 3;          // half of square edge size

    var firstData = true;
    var svg = container.append('svg');

    var br = svg.append("g").attr("class", "brush");
    var g = svg.append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var brush = d3.brush().on("end", brushended);
    var idleTimeout;
    var idleDelay = 350;

    brush.extent([[0,0],[2000,2000]]);

    /*
    g.on("mousedown",function() {

        }
    );
    */

    function setfull() {
        xMinFull = xMin;
        xMaxFull = xMax;
        yMinFull = yMin;
        yMaxFull = yMax;

    }

    function idled() {
        idleTimeout = null;
    }

    function brushended() {
        var s = d3.event.selection;
        if (!s) {
            if (!idleTimeout) return idleTimeout = setTimeout(idled, idleDelay);
            // seems not to be called. Is doubleclick cought in html ?????
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

    var xVal = function(d) {
        return parseFloat(d.x_1);
    }

    var y1Val = function(d) {
        return parseFloat(d.y1_2);
    }

    var y2Val = function(d) {
        return parseFloat(d.y2_3);
    }

    var circleTitle = function(d) { 
        var res = '';
        for (prop in d) {
            var sp = prop.split('_');
            if (prop != 'id' && prop != 'op' && sp[1] != '3') {
                res += sp[0] + ': ' + d[prop] + '\n';
            }
        }
        return res;
    };

    var circleAttrs = function(d) { 
        return {
            cx: xScale(xVal(d)),
            cy: yScale(y1Val(d)),
            r: radius
        };
    };

    var squareAttrs = function(d) { 
        return {
            x: xScale(xVal(d))-a,
            y: yScale(y2Val(d))-a,
            width: a+a,
            height: a+a
        };
    };

    var squareTitle = function(d) { 
        var res = '';
        for (prop in d) {
            var sp = prop.split('_');
            if (prop != 'id' && prop != 'op' && sp[1] != '2') {
                res += sp[0] + ': ' + d[prop] + '\n';
            }
        }
        return res;
    };

    var circleStyles = function(d) { 
        var obj = {
            fill: "steelblue"
        };
        return obj;
    };

    var squareStyles = function(d) { 
        var obj = {
            fill: "red"
        };
        return obj;
    };


    function xGrow(xMinNew,xMaxNew) {
        if (xMinNew < xMinFull) {
            xMinFull = xMinNew;
        }
        if (xMaxNew > xMaxFull) {
            xMaxFull = xMaxNew;
        }
        if (xAutoscale) {
            if (xMinNew < xMin) {
                xMin = xMinNew - xAllowance * (xMaxNew-xMinNew);
            }
            if (xMaxNew > xMax) {
                xMax = xMaxNew + xAllowance * (xMaxNew-xMinNew);
            }
        }
        return;
    }

    function yGrow(yMinNew,yMaxNew) {
        if (yMinNew < yMinFull) {
            yMinFull = yMinNew;
        }
        if (yMaxNew > yMaxFull) {
            yMaxFull = yMaxNew;
        }
        if (yAutoscale) {
            if (yMinNew < yMin) {
                yMin = yMinNew - yAllowance * (yMaxNew-yMinNew);
            }
            if (yMaxNew > yMax) {
                yMax = yMaxNew + yAllowance * (yMaxNew-yMinNew);
            }
        }
        return;
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
                .style("text-anchor", "end")
                .style('stroke', 'Black')
                .text(xText);

            yAxisGroup.transition().call(yAxis);  // Update Y-Axis

            g.selectAll("circle").transition().attrs(circleAttrs);

            g.selectAll("rect").transition().attrs(squareAttrs);
        }
        // svg.selectAll(".domain").style("display", "none");
        // br.call(brush);
        // g.call(brush);        
    }

    function rescale() {
        if (xMin >= xMax) {
            xScale = d3.scaleLinear().domain([xMin-1, xMax+1]).range([0, cWidth]);
        } else {
            xScale = d3.scaleLinear().domain([xMin, xMax]).range([0, cWidth]);
        }

        if (yMin >= yMax) {
            yScale = d3.scaleLinear().domain([yMin-1, yMax+1]).range([cHeight, 0]);
        } else {
            yScale = d3.scaleLinear().domain([yMin, yMax]).range([cHeight, 0]);
        }

        xAxis = d3.axisBottom(xScale).ticks(xTickCount, xTickFormatSpecifier);
        yAxis = d3.axisLeft(yScale).ticks(yTickCount, yTickFormatSpecifier);          
    }

    setfull();
    resize(width, height);
    br.call(brush);

    return {

        on_data: function(data) {

            if (data.length === 0) {return;}

            xGrow(Math.min(xMin, d3.min(data, xVal))
                , Math.max(xMax, d3.max(data, xVal))
                );
            yGrow(Math.min(yMin, d3.min(data, y1Val), d3.min(data, y2Val))
                , Math.max(yMax, d3.max(data, y1Val), d3.max(data, y2Val))
                );

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
                    .style("text-anchor", "end")
                    .style('stroke', 'Black')
                    .text(xText);

                yAxisGroup = g.append("g")
                    .attr("class", "y axis")
                    .call(yAxis);

                yAxisGroup.append("text")
                    .attr("transform", "rotate(-90)")
                    .attr("y", 6)
                    .attr("dx", "-0.71em")
                    .attr("dy", ".71em")
                    .style("text-anchor", "end")
                    .style('stroke', 'Black')
                    .text(yText);

            } else {
                xAxisGroup
                    .attr("transform", "translate(0," + cHeight + ")")
                    .transition().call(xAxis);  // Update X-Axis
                yAxisGroup.transition().call(yAxis);  // Update Y-Axis
            }

            var circles = g.selectAll("circle");

            circles.transition().attrs(circleAttrs);

            circles.data(data, idVal)
                .enter()
                .append("svg:circle")
                .attrs(circleAttrs)
                .styles(circleStyles)
                .append("title")
                .html(circleTitle)
                // .call(d3.helper.tooltip())
                ;

            var squares = g.selectAll("rect");

            squares.transition().attrs(squareAttrs);

            squares.data(data, idVal)
                .enter()
                .append("svg:rect")
                .attrs(squareAttrs)
                .styles(squareStyles)
                .append("title")
                .html(squareTitle)
                // .call(d3.helper.tooltip())                
                ;

        },

        on_resize: function(w, h) {
            resize(w, h);
        },

        on_reset: function() { 
            g.selectAll('svg > g > *').remove();
            firstData = true;
            xAutoscale = true;
            yAutoscale = true;
        }
    };
}