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
    var xMin = 1e100, xMax = -1e100;
    var yMin = 1e100, yMax = -1e100;
    var xScale, yScale;
    var xAxis, xText = "x-Value";
    var yAxis, yText = "y-Value";
    var radius = 3;
    var svg = container.append('svg');
    var xAxisGroup;
    var yAxisGroup;
    var firstData = true;

    var idVal = function(d) {
        return d.id;
    }

    var xVal = function(d) {
        return parseFloat(d.x_1);
    }

    var yVal = function(d) {
        return parseFloat(d.y1_2);
    }

    var g = svg.append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    function resize(w, h) {
        console.log("resize called");
        width = w;
        height = h;
        cWidth = width - margin.left - margin.right;
        cHeight = height - margin.top - margin.bottom;
        svg.attr('width', width).attr('height', height);
    }

    resize(width, height);

    return {

        on_data: function(data) {

            if (data.length === 0) {return;}

            xMin = Math.min(xMin, d3.min(data, xVal));
            xMax = Math.max(xMax, d3.max(data, xVal));
            yMin = Math.min(yMin, d3.min(data, yVal));
            yMax = Math.max(yMax, d3.max(data, yVal));

            xScale = d3.scaleLinear().domain([xMin, xMax]).range([0, cWidth]);
            yScale = d3.scaleLinear().domain([yMin, yMax]).range([cHeight, 0]);
            xAxis = d3.axisBottom(xScale).ticks(10);
            yAxis = d3.axisLeft(yScale).ticks(10);      // , "%"

            if (firstData) {

                firstData = false;

                xAxisGroup = g.append("g")
                    .attr("class", "x axis")
                    .attr("transform", "translate(0," + cHeight + ")")
                    .style('stroke', 'Black')
                    .style('fill', 'none')
                    .style('stroke-width', '1px')
                    .call(xAxis);
                xAxisGroup.append("text")
                    .attr("x", xScale(xMax))
                    .attr("dx", "-0.71em")
                    .attr("dy", "-0.71em")
                    .style("text-anchor", "end")
                    .text(xText);

                yAxisGroup = g.append("g")
                    .attr("class", "y axis")
                    .style('stroke', 'Black')
                    .style('fill', 'none')
                    .style('stroke-width', '1px')
                    .call(yAxis);

                yAxisGroup.append("text")
                    .attr("transform", "rotate(-90)")
                    .attr("y", 6)
                    .attr("dx", "-0.71em")
                    .attr("dy", ".71em")
                    .style("text-anchor", "end")
                    .text(yText);

            } else {
                xAxisGroup.transition().call(xAxis);  // Update X-Axis
                yAxisGroup.transition().call(yAxis);  // Update Y-Axis
            }

            var circles = g.selectAll("circle");
            circles.data(data, idVal)
                .enter()
                .append("svg:circle")
                .attr("cx", function(d) { return xScale(xVal(d)); })
                .attr("cy", function(d) { return yScale(yVal(d)); })
                .attr("r", radius)
                .style("fill", "steelblue");

            circles.transition()  // Gives the fly out from the center effect
                .attr("cx", function(d) { return xScale(xVal(d)); })
                .attr("cy", function(d) { return yScale(yVal(d)); });

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