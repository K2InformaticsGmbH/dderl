function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are (container, width, height)

    // container: d3 selection of the contaner div for the graph
    // width: width of the container
    // height: height of the container

    // create table gsampChartTxtFloat (x binstr , y1 float , y2 float , c1 binstr , c2 binstr )

    // The function must then return an object with the following callbacks:

    var margin = { top: 20, right: 20, bottom: 30, left: 40 }; 	// physical margins in px
    var cWidth, cHeight;							// main physical content size in px
    var xMin = 0, xMax = 100;
    var yMin = 0, yMax = 0.3;
    var xScale, yScale;
    var xAxis, xText = "Time";
    var yAxis, yText = "Memory";
    var svg = container.append('svg');

    function idVal(d) {
        return d.id;
    }

    function xVal(d) {
        return d3.timeParse("%d%m%y")(d.time_1);
    }

    function yVal(d) {
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
        xScale = d3.scaleTime().range([0, cWidth]);
        yScale = d3.scaleLinear().domain([yMin, yMax]).range([cHeight, 0]);
        xAxis = d3.axisBottom(xScale).ticks(10);
        yAxis = d3.axisLeft(yScale).ticks(10, "%");
        svg.attr('width', width)
            .attr('height', height);

    }

    resize(width, height);

    return {

        on_data: function(data) {
            console.log("new data arrived", data);

            //g.selectAll('svg > g > *').remove(); // every data block 

            g.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(0," + cHeight + ")")
                .style('stroke', 'Black')
                .style('fill', 'none')
                .style('stroke-width', '1px')
                .call(xAxis)
                .append("text")
                .attr("x", xScale(xMax))
                .attr("dx", "-0.71em")
                .attr("dy", "-0.71em")
                .style("text-anchor", "end")
                .text(xText);

            g.append("g")
                .attr("class", "y axis")
                .style('stroke', 'Black')
                .style('fill', 'none')
                .style('stroke-width', '1px')
                .call(yAxis)
                .append("text")
                .attr("transform", "rotate(-90)")
                .attr("y", 6)
                .attr("dx", "-0.71em")
                .attr("dy", ".71em")
                .style("text-anchor", "end")
                .text(yText);

            g.selectAll("scatter-dots")
                .data(data, function(d) { return idVal(d); })
                .enter()
                .append("svg:circle")
                .attr("cx", function(d) { return xScale(xVal(d)); })
                .attr("cy", function(d) { return yScale(yVal(d)); })
                .attr("r", 3)
                .style("fill", "steelblue");
        },

        on_resize: function(w, h) {
            resize(w, h);
        },

        on_reset: function() { }
    };
}