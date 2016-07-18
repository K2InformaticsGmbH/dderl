function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are (container, width, height)

    // container: d3 selection of the contaner div for the graph
    // width: width of the container
    // height: height of the container

    // create table gsampChartTxtFloat (x binstr , y1 float , y2 float , c1 binstr , c2 binstr )
    // copy csv data (without header line) from ord-bar-chart.csv
    // paste the data into the table

    var margin = { top: 20, right: 20, bottom: 30, left: 40 };  // physical margins in px
    var cWidth, cHeight;                            // main physical content size in px
    var xScale, yScale;
    var xAxis, xText = "character";
    var yAxis, yText = "frequency";
    var yMax;
    var svg = container.append('svg');

    function idVal(d) {
        return d.id;
    }

    function xVal(d) {
        return d.x_1;
    }

    function yVal(d) {
        return parseFloat(d.y1_2);
    }

    var g = svg.append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    function resize(w, h) {
        console.log("resize called", w, h);
        svg.attr('width', w).attr('height', h);
        
        width = w;
        height = h;
        cWidth = width - margin.left - margin.right;
        cHeight = height - margin.top - margin.bottom;
        xScale = d3.scaleBand().rangeRound([0, cWidth]).paddingInner(0.1); // padding 0.1
        yScale = d3.scaleLinear().range([cHeight, 0]);
        xAxis = d3.axisBottom(xScale);
        yAxis = d3.axisLeft(yScale).ticks(10, "%");
    }

    resize(width, height);

    // The function must then return an object with the following callbacks:
    return {

        on_data: function(data) {
            console.log("new data arrived", data);

            xScale.domain(data.map(function(d) { return xVal(d); }));
            yMax = d3.max(data, function(d) { return yVal(d); });
            yScale.domain([0, yMax]);

            g.selectAll('svg > g > *').remove(); // every data block 

            g.selectAll(".bar")
                .data(data, function(d) { return idVal(d); })
                .enter()
                .append("rect")
                .attr("class", "bar")
                .attr("x", function(d) { return xScale(xVal(d)); })
                .attr("width", xScale.bandwidth())
                .attr("y", function(d) { return yScale(yVal(d)); })
                .attr("height", function(d) { return cHeight - yScale(yVal(d)); })
                .style("fill", "steelblue");

            g.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(0," + cHeight + ")")
                .style('stroke', 'Black')
                .style('fill', 'none')
                .style('stroke-width', '1px')
                .call(xAxis)
                .append("text")
                .attr("x", cWidth)
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
        },

        on_resize: function(w, h) {
            resize(w, h);
        },

        on_reset: function() { }
    };
}
