function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // To create the skvh table execute:
    // create  imem_dal_skvh  table force_example()

    // Run the query:
    // select * from force_example

    // Copy and paste the following data in the table:
    /*****

center1	{"color":"#ccffcc","linkedTo":[]}	IS9VH
center2	{"color":"#ccffcc","linkedTo":[]}	1M1WKR
a	{"color":"olive","linkedTo":["center2"]}	1PTC7Y
b	{"color":"red","linkedTo":["center2"]}	8XMU8
c	{"color":"#ffcc99","linkedTo":["center2"]}	1U1MLL
d	{"color":"yellow","linkedTo":["center2"]}	J17TF
e	{"color":"green","linkedTo":["center1"]}	V7JL3
f	{"color":"teal","linkedTo":["center1"]}	REACA
g	{"color":"navy","linkedTo":["center1"]}	72Q5E
h	{"color":"green","linkedTo":["center1"]}	GQU6N
i	{"color":"green","linkedTo":["center1"]}	22FNZY
j	{"color":"#ffcc99","linkedTo":["center1"]}	1EUXCI

    *****/

    var getKey = function(row) {
        return row.ckey_1;
    };

    var getValue = function(row) {
        return JSON.parse(row.cvalue_2);
    };

    var extractLinksNodes = function(rows) {
        var nodes = [];
        var links = [];
        var colorSet = new Set();
        rows.forEach(function(row) {
            var key = getKey(row);
            var value = getValue(row);

            nodes.push({ id: key, color: value.color });
            value.linkedTo.forEach(function(target) {
                links.push({
                    source: key,
                    target: target
                });
            });
            colorSet.add(value.color);
        });
        var colors = [];
        colorSet.forEach(function(c) {
            colors.push({value: c});
        });
        return { nodes: nodes, links: links, colors: colors };
    };

    var getLinkId = function(l) {
        return l.source + "-" + l.target;
    }

    var filter = container
        .append('select');

    var svg = container
        .append('svg')
        .attr('width', width)
        .attr('height', height);

    var old;

    filter.on("change", function(d) {
        if(old) { old.attr("r", 15); }
        var value = d3.select(this).property("value");
        old = svg.selectAll("." + value.substring(1)).attr("r", 30);
    });

    var links = svg.selectAll(".link");
    var nodes = svg.selectAll(".node");

    var simulation = d3.forceSimulation()
        .force("charge", d3.forceManyBody().strength(-300))
        .force("link", d3.forceLink().id(function(d) { return d.id; }).distance(100))
        .force("x", d3.forceX(width / 2))
        .force("y", d3.forceY(height / 2).strength(0.3))
        .on("tick", function() {
            links.attr("x1", function(d) { return d.source.x; })
                .attr("y1", function(d) { return d.source.y; })
                .attr("x2", function(d) { return d.target.x; })
                .attr("y2", function(d) { return d.target.y; });

            nodes.attr("transform", function(d) { return "translate(" + d.x + ", " + d.y + ")"; });
        });

    var dragEvents = d3.drag();
    dragEvents.on("start", function(d) {
        if(!d3.event.active) {
            simulation.alphaTarget(0.3).restart()
        };
        d.fx = d.x;
        d.fy = d.y;
    });

    dragEvents.on("drag", function(d) {
        d.fx = d3.event.x;
        d.fy = d3.event.y;
    });

    dragEvents.on("end", function(d) {
        if(!d3.event.active) {
            simulation.alphaTarget(0)
        };
        d.fx = null;
        d.fy = null;
    });

    return {
        on_data: function(data) {
            console.log("new data", data);
            var graph = extractLinksNodes(data);

            simulation.nodes(graph.nodes);
            simulation.force("link").links(graph.links);


            links = links.data(graph.links, getLinkId)
                .enter().append("line")
                .attr("class", "link")
                .styles({ stroke: "#999", "stroke-width": "1.5px" });

            nodes = nodes.data(graph.nodes, function(d) { return d.id; })
                .enter().append("g")
                .attr("class", "node")
                .style("cursor", "move")
                .call(dragEvents);

            nodes.append("circle")
                .attr("r", 15)
                .style("fill", function(d) { return d.color; })
                .attr("class", function(d) {return d.color.substring(1); });

            nodes.append("text").text(function(d) { return d.id; });

            filter.selectAll("option")
                .data(graph.colors, function(d) {return d.id; })
                .enter().append("option")
                .attr("value", function(d) { return d.value; })
                .text(function(d) { return d.value; })
        },
        on_resize: function(w, h) {
            svg.attr('width', w)
                .attr('height', h);
        },
        on_reset: function() {
            svg.selectAll("*").remove();
        }
    };
}
