	// This code is executed once and it should initialize the graph, the
	// available parameters are (container, width, height)

	// container: d3 selection of the contaner div for the graph
	// width: width of the container
	// height: height of the container

	// create table gsampChartTxtFloat (x binstr , y1 float , y2 float , c1 binstr , c2 binstr )

	// The function must then return an object with the following callbacks:

	var margin = {top: 20, right: 20, bottom: 30, left: 40}; 	// physical margins in px
	var cWidth, cHeight;							// main physical content size in px
	var xScale, yScale;
	var xAxis, yAxis;    
	var svg;

	function idVal(d) {
	  	return d.id;
	}

	function xVal(d) {
	  	return d.x_1;
	}

	function yVal(d) {
	  	return parseFloat(d.y1_2);
	}

	function resize(w,h) {
	  	console.log("resize called");
	    width = w;
	    height = h;
	    cWidth = width - margin.left - margin.right;
	    cHeight = height - margin.top - margin.bottom;
	    xScale = d3.scale.ordinal().rangeRoundBands([0, cWidth], 0.1);
	    yScale = d3.scale.linear().range([cHeight, 0]);
	    xAxis = d3.svg.axis().scale(xScale).orient("bottom");
	    yAxis = d3.svg.axis().scale(yScale).orient("left").ticks(10, "%");
	    svg = container.append('svg')
	    	.attr('width', width)
	    	.attr('height', height)
			.append("g")
			.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
	}

	resize(width,height);

	return {

	  on_data: function(data) {
	  	console.log("new data arrived", data);
		
		xScale.domain(data.map(function(d) { return xVal(d); }));
		yScale.domain([0, d3.max(data, function(d) { return yVal(d); })]);

	    svg.append("g")
	    	.attr("class", "x axis")
	    	.attr("transform", "translate(0," + cHeight + ")")
			.style({ 'stroke': 'Black', 'fill': 'none', 'stroke-width': '1px'})
	     	.call(xAxis);

	    svg.append("g")
	    	.attr("class", "y axis")
			.style({ 'stroke': 'Black', 'fill': 'none', 'stroke-width': '1px'})
	    	.call(yAxis)
	    	.append("text")
	    	.attr("transform", "rotate(-90)")
	    	.attr("y", 6)
	    	.attr("dy", ".71em")
	    	.style("text-anchor", "end")
	    	.text("Frequency");

		svg.selectAll(".bar")
		   	.data(data, function(d) { return idVal(d); })
		   	.enter()
		   	.append("rect")
		   	.attr("class", "bar")
	    	.attr("x", function(d) { return xScale(xVal(d)); })
	      	.attr("width", xScale.rangeBand())
	      	.attr("y", function(d) { return yScale(yVal(d)); })
	      	.attr("height", function(d) { return cHeight - yScale(yVal(d)); } )
	      	.style("fill","steelblue");
	    },

	    on_resize: function(w, h) {
	    	resize(w,h)
	    },

	    on_reset: function() {}
	};
