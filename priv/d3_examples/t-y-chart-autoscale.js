function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are (container, width, height)

    // container: d3 selection of the contaner div for the graph
    // width: width of the container
    // height: height of the container

    /*
       select time as x, memory as y1 from ddMonitor_86400@ where time > systimestamp - 5 / 24 / 60
    */

    // The function must then return an object with the following callbacks:

    var margin = { top: 20, right: 20, bottom: 50, left: 90 };  // physical margins in px
    var cWidth, cHeight;                            // main physical content size in px
    var xScale, yScale;
    var xAxisGroup;
    var yAxisGroup;

    var tParse = d3.timeParse("%d.%m.%Y %H:%M:%S.%L");
    var xMin = tParse("01.01.2300 00:00:00.000");    // autoscale defaults
    var xMax = tParse("01.01.1900 00:00:00.000");    // autoscale defaults
    var yMin = 1e100, yMax = -1e100;    // autoscale defaults

    // xMin = tParse("30.07.2016 16:00:00.000");    // autoscale override
    // xMax = tParse("30.07.2016 17:30:00.000");    // autoscale override

    var xTickCount = 8;
    var xTickFormatSpecifier = "%I:%M %p";
     
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

    // yMin = ..., yMax = ....;         // set fixed initial values here

    var yTickCount = 10;
    var yTickFormatSpecifier = "s";

    var xAutoscale = true;
    var yAutoscale = true;
    var xAllowance = 0.05;
    var yAllowance = 0.05;
    var xAxis, xText = "time";
    var yAxis, yText = "y-Value";
    var radius = 3;

    var firstData = true;
    var svg = container.append('svg');
    var g = svg.append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var idVal = function(d) {
        return d.id;
    }

    var xVal = function(d) {
        var tStr = d.x_1;   // DD.MM.YYYY hh:mi:ss.uuuuuu
        console.log("x rounded", tStr.substr(0,23));
        var res = tParse(tStr.substr(0,23));
        console.log("x result", res);
        return res;
    }

    var yVal = function(d) {
        return parseFloat(d.y1_2);
    }

    var circleAttrs = function(d) { 
        return {
            cx: xScale(xVal(d)),
            cy: yScale(yVal(d)),
            r: radius
        };
    };

    var circleStyles = function(d) { 
        var obj = {
            fill: "steelblue"
        };
        return obj;
    };

    function xGrow(xMinNew,xMaxNew) {
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

            // xAxisGroup
            //     .attr("transform", "translate(0," + cHeight + ")")
            //     .transition().call(xAxis);  // Update X-Axis
            // xAxisGroup.selectAll("text")
            //     .attr("x", cWidth);

            yAxisGroup.transition().call(yAxis);  // Update Y-Axis
            var circles = g.selectAll("circle");
            circles.transition()  
                .attr("cx", function(d) { return xScale(xVal(d)); })
                .attr("cy", function(d) { return yScale(yVal(d)); });
        }
    }

    function rescale() {
        if (xMin >= xMax) {
            xScale = d3.scaleTime()
                .domain([xMin-1, xMax+1])
                .range([0, cWidth]);
        } else {
            xScale = d3.scaleTime()
                .domain([xMin, xMax])
                .range([0, cWidth]);
        }

        if (yMin >= yMax) {
            yScale = d3.scaleLinear().domain([yMin-1, yMax+1]).range([cHeight, 0]);
        } else {
            yScale = d3.scaleLinear().domain([yMin, yMax]).range([cHeight, 0]);
        }

        xAxis = d3.axisBottom(xScale).ticks(xTickCount, xTickFormatSpecifier);
        yAxis = d3.axisLeft(yScale).ticks(yTickCount, yTickFormatSpecifier);      // , "%"        
    }

    resize(width, height);

    return {

        on_data: function(data) {

            if (data.length === 0) {return;}

            xGrow(Math.min(xMin, d3.min(data, xVal)), Math.max(xMax, d3.max(data, xVal)));
            yGrow(Math.min(yMin, d3.min(data, yVal)), Math.max(yMax, d3.max(data, yVal)));

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

            circles.transition()  
                .attr("cx", function(d) { return xScale(xVal(d)); })
                .attr("cy", function(d) { return yScale(yVal(d)); });

            circles.data(data, idVal)
                .enter()
                .append("svg:circle")
                .attrs(circleAttrs)
                .styles(circleStyles)
                ;

        },

        on_resize: function(w, h) {
            resize(w, h);
        },

        on_reset: function() { 
            g.selectAll('svg > g > *').remove();
            firstData = true;
        }
    };
}