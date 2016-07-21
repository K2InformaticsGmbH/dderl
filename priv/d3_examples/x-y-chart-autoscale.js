function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are (container, width, height)

    // container: d3 selection of the contaner div for the graph
    // width: width of the container
    // height: height of the container

    /*
        select    0.01 * item as x
                , 0.01 * item + 0.1 * sin(0.1 * item) as y1 
        from integer where item >= 0 and item <= 200
    */

    // The function must then return an object with the following callbacks:

    var margin = { top: 20, right: 20, bottom: 30, left: 40 }; 	// physical margins in px
    var cWidth, cHeight;							// main physical content size in px
    var xScale, yScale;
    var xAxisGroup;
    var yAxisGroup;

    var xMin = 1e100, xMax = -1e100;
    var yMin = 1e100, yMax = -1e100;
    var xAllowance = 0.05;
    var yAllowance = 0.05;
    var xAxis, xText = "x-Value";
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
        return parseFloat(d.x_1);
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
        if (xMinNew < xMin) {
            xMin = xMinNew - xAllowance * (xMaxNew-xMinNew);
        }
        if (xMaxNew > xMax) {
            xMax = xMaxNew + xAllowance * (xMaxNew-xMinNew);
        }
        return;
    }

    function yGrow(yMinNew,yMaxNew) {
        if (yMinNew < yMin) {
            yMin = yMinNew - yAllowance * (yMaxNew-yMinNew);
        }
        if (yMaxNew > yMax) {
            yMax = yMaxNew + yAllowance * (yMaxNew-yMinNew);
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
            xScale = d3.scaleLinear().domain([xMin-1, xMax+1]).range([0, cWidth]);
        } else {
            xScale = d3.scaleLinear().domain([xMin, xMax]).range([0, cWidth]);
        }

        if (yMin >= yMax) {
            yScale = d3.scaleLinear().domain([yMin-1, yMax+1]).range([cHeight, 0]);
        } else {
            yScale = d3.scaleLinear().domain([yMin, yMax]).range([cHeight, 0]);
        }

        xAxis = d3.axisBottom(xScale).ticks(10);
        yAxis = d3.axisLeft(yScale).ticks(10);      // , "%"        
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