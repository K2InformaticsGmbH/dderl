function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // To create the skvh table execute:
    // create imem_dal_skvh table arc_example()

    // Run the query:
    // select * from arc_example

    // Copy and paste the following data in the table:
    /*****

["prod","job01"]	{"platform": "platform01", "direction":"pull", "args": {"group": "03"}, "enabled": true}	ZSPGI
["prod","job01","10.0.0.1"]	{"status":"idle"}	1UUXWW
["prod","job02"]	{"platform": "platform02", "direction":"pull", "args": {"group":"03"}, "enabled": true}	11TOKT
["prod","job03"]	{"platform": "platform02", "direction":"pull", "args": {"group":"03"}, "enabled": true}	1SF3PY
["prod","job03","10.0.0.1"]	{"status":"idle"}	KG4FV
["prod","job03","10.0.0.2"]	{"status":"idle"}	TZ1G7
["prod","job03","10.0.0.3"]	{"status":"idle"}	22ES62
["prod","job04"]	{"platform": "platform04", "direction":"pull", "args": {"group":"03"}, "enabled": false}	16DMNI
["prod","job05"]	{"platform": "platform05", "direction":"pull", "args": {"group":"02"}, "enabled": false}	13ZI63
["prod","job06"]	{"platform": "platform06", "direction":"pull", "args": {"group":"01"}, "enabled": true}	T6SIK
["prod","job06","10.0.0.1"]	{"status":"idle"}	R9WKE
["prod","job06","10.0.0.2"]	{"status":"idle"}	1YPVKM
["prod","job07"]	{"platform": "platform07", "direction":"pull", "args": {"group":"02"}, "enabled": true}	21GJH
["prod","job07","10.0.0.3"]	{"status":"error"}	1C0VIR
["prod","job08"]	{"platform": "platform08", "direction":"pull", "args": {"group":"03"}, "enabled": true}	1G8553
["prod","job08","10.0.0.2"]	{"status":"idle"}	1K5JDG
["prod","job09"]	{"platform": "platform09", "direction":"push", "args": {"group":"02"}, "enabled": true}	174V1F
["prod","job09","10.0.0.1"]	{"status":"idle"}	K6X90
["prod","job10"]	{"platform": "platform10", "direction":"pull", "args": {"group":"02"}, "enabled": true}	203DBM
["prod","job10","10.0.0.1"]	{"status": "refreshed"}	XC5I1
["stag","job11"]	{"platform": "platform11", "direction":"pull", "args": {"group":"01"}, "enabled": true}	1UQK0V
["stag","job11","192.168.0.1"]	{"status":"idle"}	IOW3P
["stag","job11","192.168.0.2"]	{"status":"idle"}	1YAQRX
["stag","job12"]	{"platform": "platform12", "direction":"pull", "args": {"group":"01"}, "enabled": true}	1NOQXI
["stag","job13"]	{"platform": "platform13", "direction":"pull", "args": {"group":"01"}, "enabled": false}	JPIZY
["stag","job14"]	{"platform": "platform14", "direction":"pull", "args": {"group":"01"}, "enabled": true}	2FBUU
["stag","job14","192.168.0.1"]	{"status": "idle"}	208ASX
["stag","job15"]	{"platform": "platform15", "direction":"pull", "args": {"group":"01"}, "enabled": false}	1EX091
["stag","job16"]	{"platform": "platform16", "direction":"pull", "args": {"group":"01"}, "enabled": true}	VORM4
["stag","job16","192.168.0.1"]	{"status": "cleaned"}	XYI25
["stag","job17"]	{"platform": "platform17", "direction":"pull", "args": {"group":"01"}, "enabled": true}	1LVEOW
["stag","job17","192.168.0.1"]	{"status": "idle"}	1DEQ20
["stag","job18"]	{"platform": "platform06", "direction":"pull", "args": {"group":"01"}, "enabled": true}	23SE1K
["stag","job18","192.168.0.1"]	{"status":"idle"}	PBG1Y
    *****/

    /** Size & positioning parameters */
    
    // virtual coordinates drawing arc radius
    var vArcRadius = 1000;
    // node radius in virtual coordinates
    var nradius = 90; // TODO: 90 for 17 nodes seems ok, formula ?

    var centerNodes = [
        // Position relative to the bottom center after margin.
        { id: 'stag', position: { x: -1.9 * nradius, y: 0.3 * nradius }, status: 'idle' },
        { id: 'prod', position: { x: 0, y: -nradius }, status: 'idle' }
    ];

    /** Helper functions for data extraction */
    var getKey = function(row) {
        return JSON.parse(row.ckey_1);
    };

    var getValue = function(row) {
        return JSON.parse(row.cvalue_2);
    };

    var extractLinksNodes = function(rows) {
        var links = [];
        var nlset = new Set();
        // Add already included center nodes to the set
        centerNodes.forEach(function(n) {
            nlset.add(n.id);
        });
        var nodes = [];
        var status = [];
        rows.forEach(function(row) {
            var key = getKey(row);
            var value = getValue(row);

            if(key.length === 3) {
                var triangleId = key[0] + '_' + key[1] + '_' + key[2];
                var linkId = key[0] + '_' + key[1];
                if(!nlset.has(triangleId)) {
                    nlset.add(triangleId);
                    status.push({
                        id: triangleId,
                        link: linkId,
                        status: value.status
                    });
                }
            } else if(key.length === 2) {
                var nodeId = value.platform;
                var group = value.args.group;
                if(!nlset.has(nodeId)) {
                    nlset.add(nodeId);
                    nodes.push({
                        id: nodeId,
                        group: group
                    });
                }
                var linkId = key[0] + '_' + key[1];
                if(!nlset.has(linkId)) {
                    nlset.add(linkId);
                    links.push({
                        id: linkId,
                        source: key[0],
                        target: nodeId,
                        legend: key[1],
                        enabled: value.enabled,
                        direction: value.direction
                    });
                }

            }
        });
        return { links: links, nodes: nodes, status: status };
    };
    /** End data extraction functions */

    var margin = { top: 10, right: 10, bottom: 10, left: 10 }; 	// physical margins in px

    var colorStatus = {
        idle: 'green',
        error: 'red',
        synced: 'green',
        cleaning: 'lightsteelblue',
        cleaned: 'blue',
        refreshing: 'cornflowerblue',
        refreshed: 'purple',
        stopped: 'red'
    };
    // To see the complete circle when drawing negative coordinates
    // and width and height for the virtual coordinates
    var vBox = {
        x: -1 * (vArcRadius + nradius),
        y: -1 * (vArcRadius + nradius),
        w: vArcRadius * 2 + nradius * 2,
        h: vArcRadius + 3 * nradius
    };

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
            font: "18px sans-serif",
            border: "0px",		
            "border-radius": "8px",			
            "pointer-events": "none",
            opacity: 0,
            "z-index": 99996
        });

    function showTooltip(d) {
        tootipDiv
            .html(JSON.stringify(d))
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

    function openView(d) {
        helper.browse('parameterized_d', {
            ':boolean_read' : {typ: "boolean", val: d.enabled.toString()}
        });
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

            var newNodes = svg.selectAll('.node')
                .data(graph.nodes, function(d) {
                    return d.id;
                })
                .enter()
                .append('g')
                .attr("class", "node");

            newNodes.append('circle')
                .attr('r', nradius)
                .attr('id', function(d) {
                    return d.id;
                })
                .style('stroke', 'black')
                .attr('stroke-width', 5)
                .style('fill', 'white')
                .on('mouseover', showTooltip)
                .on('mousemove', moveTooltip)
                .on('mouseout', hideTooltip)
                .on('click', openView);

            newNodes.append('text')
                .text(function(d) {
                    return d.id;
                })
                .style('font-size', '24px')
                .style('text-anchor', 'middle');

            var allPoints = svg.selectAll('circle')
                .filter(function(d) {
                    return !d.position;
                });

            var nData = [];
            svg.selectAll('.node').each(function (d) {
                nData.push(d);
            });

            nData.sort(function(a, b) {
                if(a.group == b.group) {
                    return a.id.localeCompare(b.id);
                }
                return a.group.localeCompare(b.group);
            });

            var angle = Math.PI / (nData.length + 1);

            var positions = {};
            for(var i = 0; i < nData.length; ++i) {
                var r = vArcRadius - 2 * nradius * (i % 2);
                var x = -r * Math.cos((i + 1) * angle);
                var y = -r * Math.sin((i + 1) * angle);
                positions[nData[i].id] = {x: x, y: y};
            }
            // Append center node positions.
            for(var i = 0; i < centerNodes.length; ++i) {
                positions[centerNodes[i].id] = centerNodes[i].position;
            }

            allPoints
                .transition()
                .duration(500)
                .attr('cx', function(d) {
                    return positions[d.id].x;
                })
                .attr('cy', function(d, i) {
                    return positions[d.id].y;
                });

            svg.selectAll('text')
                .transition()
                .duration(500)
                .attr('x', function(d, i) {
                    return positions[d.id].x;
                })
                .attr('y', function(d, i) {
                    return positions[d.id].y;
                })

            svg.selectAll('line')
                .data(graph.links, function(d) {
                    return d.id;
                })
                .enter()
                .insert('line', 'circle')
                .attr('stroke-width', 6)
                .attr('id', function(d) {
                    return d.id;
                })
                .on('mouseover', showTooltip)
                .on('mousemove', moveTooltip)
                .on('mouseout', hideTooltip);

            var allLinks = svg.selectAll('line');

            // Adding connecting links
            allLinks
                .transition()
                .duration(500)
                .attr('x1', function(d) {
                    if(!positions[d.source]) { return 0; }
                    return positions[d.source].x;
                })
                .attr('y1', function(d) {
                    if(!positions[d.source]) { return 0; }
                    return positions[d.source].y;
                })
                .attr('x2', function(d) {
                    if(!positions[d.target]) { return 0; }
                    return positions[d.target].x;
                })
                .attr('y2', function(d) {
                    if(!positions[d.target]) { return 0; }
                    return positions[d.target].y;
                })
                .attr('stroke', function(d) {
                    return d.enabled ? 'green' : 'lightgrey';
                });

            var linksMid = {};
            allLinks.each(function(d) {
                if(!positions[d.source] || !positions[d.target]) {
                    return;
                }
                var midX = 0.5 * (positions[d.source].x + positions[d.target].x);
                var midY = 0.5 * (positions[d.source].y + positions[d.target].y);
                var dirX = positions[d.source].x - positions[d.target].x;
                var dirY = positions[d.source].y - positions[d.target].y;
                if(d.direction === "pull") {
                    dirX = -dirX;
                    dirY = -dirY;
                }
                linksMid[d.id] = {mid: {x: midX, y: midY}, direction: {x: dirX, y: dirY}};
            });

            svg.selectAll('polygon')
                .data(graph.status, function(d) {
                    return d.id;
                })
                .enter()
                .append('polygon')
                .attr('id', function(d) {
                    return d.id;
                })
                .on('mouseover', showTooltip)
                .on('mousemove', moveTooltip)
                .on('mouseout', hideTooltip);
            
            // TODO: Triangles should be created in origin and then translated.
            svg.selectAll('polygon')
                .attr('points', function(d) {
                    var x = linksMid[d.link].mid.x;
                    var y = linksMid[d.link].mid.y;
                    var p1 = x + "," + y;
                    var p2 = (x - 15) + "," + (y + 45);
                    var p3 = (x + 15) + "," + (y + 45);
                    return p1 + " " + p2 + " " + p3;
                })
                .attr('transform', function(d) {
                    var dx = linksMid[d.link].direction.x;
                    var dy = linksMid[d.link].direction.y;
                    var angle = -1 * Math.atan2(dx, dy) * 180 / Math.PI;
                    var x = linksMid[d.link].mid.x;
                    var y = linksMid[d.link].mid.y;
                    return "rotate(" + angle + " " + x + " " + y + ")";
                })
                .style('stroke', 'black')
                .style('stroke-width', 3)
                .style('fill', function(d) {
                    return colorStatus[d.status];
                });
        },
        on_resize: resize,
        on_reset: function() {
            svg.selectAll('svg > *').remove();
            firstData = true;
        }
    };
}
