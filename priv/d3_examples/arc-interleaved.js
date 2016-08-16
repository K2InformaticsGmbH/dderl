function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // To create the skvh table execute:
    // create imem_dal_skvh table arc_example()

    // Run the query:
    // select * from arc_example

    // Copy and paste the following data in the table:
    /*****

["prod","platform01"]	{"enabled": true}	ZSPGI
["prod","platform01","10.0.0.1"]	{"status":"idle"}	1UUXWW
["prod","platform02"]	{"enabled": true}	11TOKT
["prod","platform03"]	{"enabled": true}	1SF3PY
["prod","platform03","10.0.0.1"]	{"status":"idle"}	KG4FV
["prod","platform03","10.0.0.2"]	{"status":"idle"}	TZ1G7
["prod","platform03","10.0.0.3"]	{"status":"idle"}	22ES62
["prod","platform04"]	{"enabled": false}	16DMNI
["prod","platform05"]	{"enabled": false}	13ZI63
["prod","platform06"]	{"enabled": true}	T6SIK
["prod","platform06","10.0.0.1"]	{"status":"idle"}	R9WKE
["prod","platform06","10.0.0.2"]	{"status":"idle"}	1YPVKM
["prod","platform07"]	{"enabled": true}	21GJH
["prod","platform07","10.0.0.3"]	{"status":"error"}	1C0VIR
["prod","platform08"]	{"enabled": true}	1G8553
["prod","platform08","10.0.0.2"]	{"status":"idle"}	1K5JDG
["prod","platform09"]	{"enabled": true}	174V1F
["prod","platform09","10.0.0.1"]	{"status":"idle"}	K6X90
["prod","platform10"]	{"enabled": true}	203DBM
["prod","platform10","10.0.0.1"]	{"status": "refreshed"}	XC5I1
["stag","platform06"]	{"enabled": true}	23SE1K
["stag","platform06","192.168.0.1"]	{"status":"idle"}	PBG1Y
["stag","platform11"]	{"enabled": true}	1UQK0V
["stag","platform11","192.168.0.1"]	{"status":"idle"}	IOW3P
["stag","platform11","192.168.0.2"]	{"status":"idle"}	1YAQRX
["stag","platform12"]	{"enabled": true}	1NOQXI
["stag","platform13"]	{"enabled": false}	JPIZY
["stag","platform14"]	{"enabled": true}	2FBUU
["stag","platform14","192.168.0.1"]	{"status": "idle"}	208ASX
["stag","platform15"]	{"enabled": false}	1EX091
["stag","platform16"]	{"enabled": true}	VORM4
["stag","platform16","192.168.0.1"]	{"status": "cleaned"}	XYI25
["stag","platform17"]	{"enabled": true}	1LVEOW
["stag","platform17","192.168.0.1"]	{"status": "idle"}	1DEQ20

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
        var n0 = [];
        var n1 = [];
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
                    if(key[0] === centerNodes[0].id) {
                        n0.push({
                            id: key[1],
                            enabled: value.enabled
                        });
                    } else if(key[0] === centerNodes[1].id) {
                        n1.push({
                            id: key[1],
                            enabled: value.enabled
                        });
                    }
                }
            }
        });
        return { links: links, nodes: n0.concat(n1) };
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
                    // Interleave smaller radius to allow more nodes.
                    var r = vArcRadius - 2 * nradius * (i % 2);
                    return -r * Math.cos((i + 1) * angle);
                })
                .attr('cy', function(d, i) {
                    var r = vArcRadius - 2 * nradius * (i % 2);
                    return -r * Math.sin((i + 1) * angle);
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
                .attr('stroke-width', 6)
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
