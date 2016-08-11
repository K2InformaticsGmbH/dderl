function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // To create the skvh table execute:
    // create imem_dal_skvh table arc_example()

    // Run the query:
    // select * from arc_example

    // Copy and paste the following data in the table:
    /*****

["prod", "platform1"]	{"enabled": true}	IS9VH
["prod", "platform1", "10.0.0.1"]	{"status":"idle"}	1M1WKR
["prod", "platform2"]	{"enabled": true}	1PTC7Y
["prod", "platform3"]	{"enabled": false}	8XMU8
["prod", "platform3", "10.0.0.1"]	{"status":"idle"}	1U1MLL
["prod", "platform3", "10.0.0.2"]	{"status":"idle"}	J17TF
["prod", "platform3", "10.0.0.3"]	{"status":"idle"}	V7JL3
["prod", "platform4"]	{"enabled": true}	REACA
["prod", "platform5"]	{"enabled": false}	72Q5E
["prod", "platform6"]	{"enabled": true}	GQU6N
["prod", "platform6", "10.0.0.1"]	{"status":"idle"}	22FNZY
["prod", "platform6", "10.0.0.2"]	{"status":"idle"}	1EUXCI
["prod", "platform7"]	{"enabled": true}	IS9VH
["prod", "platform7", "10.0.0.3"]	{"status":"error"}	1M1WKR
["prod", "platform8"]	{"enabled": true}	1PTC7Y
["prod", "platform8", "10.0.0.2"]	{"status":"idle"}	8XMU8
["stag", "platform9"]	{"enabled": true}	1U1MLL
["stag", "platform9", "192.168.0.2"]	{"status":"idle"}	J17TF
["stag", "platform6"]	{"enabled": true}	V7JL3
["stag", "platform6", "192.168.0.1"]	{"status":"idle"}	REACA
["stag", "platform10"]	{"enabled": true}	72Q5E
["stag", "platform11"]	{"enabled": true}	GQU6N
["stag", "platform11", "192.168.0.1"]	{"status":"idle"}	22FNZY
["stag", "platform11", "192.168.0.2"]	{"status":"idle"}	1EUXCI

    *****/

    /** Helper functions for data extraction */
    var getKey = function(row) {
        return JSON.parse(row.ckey_1);
    };

    var getValue = function(row) {
        return JSON.parse(row.cvalue_2);
    };

    var extractLinksNodes = function(rows) {
        var links = [];
        var nodes = [];
        var nlset = new Set();
        rows.forEach(function(row) {
            var key = getKey(row);
            var value = getValue(row);

            // TODO: This will ignore information, we need
            // to implement small nodes for each different ip links
            if(key.length === 3) {
                var id = key[0] + '_' + key[1];
                if(!nlset.has(id)) {
                    nlset.add(id);
                    links.push({
                        id: id,
                        source: key[0],
                        target: key[1],
                        legend: key[2],
                        status: value.status
                    });
                }
            } else if(key.length === 2) {
                if(!nlset.has(key[1])) {
                    nlset.add(key[1]);
                    nodes.push({
                        id: key[1],
                        enabled: value.enabled
                    });
                }
            }
        });
        return { links: links, nodes: nodes };
    };
    /** End data extraction functions */

    var margin = { top: 10, right: 10, bottom: 10, left: 10 }; 	// physical margins in px

    var colorStatus = {
        idle: 'green',
        error: 'red',
        synced: 'green',
        cleaning: 'lightsteelblue',
        cleaned: 'blue',
        refreshing: 'lightyellow',
        refreshed: 'purple',
        stopped: 'red'
    };

    // virtual coordinates drawing arc radius
    var vArcRadius = 1000;
    // node radius in virtual coordinates
    var nradius = 50;
    // To see the complete circle when drawing negative coordinates
    // and width and height for the virtual coordinates
    var vBox = {
        x: -1 * (vArcRadius + nradius),
        y: -1 * nradius,
        w: vArcRadius * 2 + nradius * 2,
        h: vArcRadius + nradius
    };

    var centerRelative = function(rx, ry) {
        return {
            x: nradius + rx,
            y: vArcRadius - nradius + ry
        };
    };

    var centerNodes = [
        // Position relative to the bottom center after margin.
        { id: 'prod', position: centerRelative(0, -2 * nradius), status: 'idle' },
        { id: 'stag', position: centerRelative(-3 * nradius, -15), status: 'idle' }
    ];

    var svg = container
        .append('svg')
        .attr('viewBox', vBox.x + ' ' + vBox.y + ' ' + vBox.w + ' ' + vBox.h)
        .attr('preserveAspectRatio', 'xMidYMax meet')
        .style('margin-top', margin.top + 'px')
        .style('margin-right', margin.right + 'px')
        .style('margin-bottom', margin.bottom + 'px')
        .style('margin-left', margin.left + 'px');

    function resize(w, h) {
        var cheight = h - (margin.top + margin.bottom);
        var cwidth = w - (margin.left + margin.right);
        svg.attr('width', cwidth)
            .attr('height', cheight);
    }

    resize(width, height);

    var tootipDiv = d3.select("body").append('div')
        .styles({
            position: "absolute",
            "text-align": "center",
            width: "60px",			
            height: "30px",
            padding: "2px",				
            font: "12px sans-serif",
            background: "lightsteelblue",
            border: "0px",		
            "border-radius": "8px",			
            "pointer-events": "none",
            opacity: 0,
            "z-index": 99996
        });

    function showTooltip(d) {
        tootipDiv
            .html(d.id)
            .transition()
            .duration(200)
            .style('opacity', 0.95);
    }

    function moveTooltip() {
        tootipDiv
            .style('left', (d3.event.pageX-30) + "px")
            .style('top', (d3.event.pageY-40) + "px");
    }

    function hideTooltip() {
        tootipDiv.transition()
            .duration(500)
            .style('opacity', 0);
    }

    var firstData = true;
    return {
        on_data: function(data) {
            if(data.length === 0) {
                return;
            }

            if(firstData) {
                firstData = false;
                // Add center nodes
                svg.selectAll('circle')
                    .data(centerNodes, function(d) { return d.id; })
                    .enter()
                    .append('circle')
                    .attr('r', nradius)
                    .attr('cx', function(d) { return d.position.x; })
                    .attr('cy', function(d) { return d.position.y; })
                    .attr('id', function(d) { return d.id; })
                    .style('fill', function(d) {
                        return colorStatus[d.status];
                    })
                    .on('mouseover', showTooltip)
                    .on('mousemove', moveTooltip)
                    .on('mouseout', hideTooltip);
            }

            var graph = extractLinksNodes(data);
            console.log('the links', graph.links);
            console.log('the nodes', graph.nodes);

            svg.selectAll('circle')
                .data(graph.nodes, function(d) {
                    return d.id;
                })
                .enter()
                .append('circle')
                .attr('r', nradius)
                .attr('id', function(d) {
                    return d.id;
                })
                .on('mouseover', showTooltip)
                .on('mousemove', moveTooltip)
                .on('mouseout', hideTooltip);

            var allPoints = svg.selectAll('circle')
                .filter(function(d) {
                    return !d.position;
                });

            var angle = Math.PI / (allPoints.size() + 1);

            allPoints
                .transition()
                .attr('cx', function(d, i) {
                    return vArcRadius * Math.cos((i + 1) * angle) * -1;
                })
                .attr('cy', function(d, i) {
                    return vArcRadius * (1 - Math.sin((i + 1) * angle));
                })
                .style('fill', function(d) {
                    return d.enabled ? 'black' : 'lightgrey';
                });

            svg.selectAll('line')
                .data(graph.links, function(d) {
                    return d.id;
                })
                .enter()
                .insert('line', 'circle')
                .attr('stroke-width', 4)
                .attr('id', function(d) {
                    return d.id;
                })
                .on('mouseover', showTooltip)
                .on('mousemove', moveTooltip)
                .on('mouseout', hideTooltip);

            var allLinks = svg.selectAll('line');

            // Adding connecting links
            setTimeout(function() {
                allLinks
                    .transition()
                    .attr('x1', function(d) {
                        var s = document.getElementById(d.source);
                        return s ? s.cx.baseVal.value : 0;
                    })
                    .attr('y1', function(d) {
                        var s = document.getElementById(d.source);
                        return s ? s.cy.baseVal.value : 0;
                    })
                    .attr('x2', function(d) {
                        var s = document.getElementById(d.target);
                        return s ? s.cx.baseVal.value : 0;
                    })
                    .attr('y2', function(d) {
                        var s = document.getElementById(d.target);
                        return s ? s.cy.baseVal.value : 0;
                    })
                    .attr('stroke', function(d) {
                        return colorStatus[d.status];
                    });
            }, 500);
        },
        on_resize: resize,
        on_reset: function() {
            svg.selectAll('svg > *').remove();
            firstData = true;
        }
    };
}
