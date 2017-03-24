function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // To create the skvh table execute:
    // create imem_dal_skvh table arc_example()

    // Run the query:
    // select * from arc_example

    // Copy and paste the following data in the table:
    /*****

["baseid","prod","job01"]	{"platform": "platform01", "direction":"pull", "args": {"group": "03"}, "enabled": true}	ZSPGI
["baseid","prod","job01","10.0.0.1"]	{"status":"idle"}	1UUXWW
["baseid","prod","job02"]	{"platform": "platform02", "direction":"push", "args": {"group":"03"}, "enabled": true}	11TOKT
["baseid","prod","job02","10.0.0.1"]	{"status":"idle"}	BKEK9
["baseid","prod","job03"]	{"platform": "platform02", "direction":"pull", "args": {"group":"03"}, "enabled": true}	1SF3PY
["baseid","prod","job03","10.0.0.1"]	{"status":"idle"}	KG4FV
["baseid","prod","job03","10.0.0.2"]	{"status":"idle"}	TZ1G7
["baseid","prod","job03","10.0.0.3"]	{"status":"idle"}	22ES62
["baseid","prod","job04"]	{"platform": "platform04", "direction":"pull", "args": {"group":"03"}, "enabled": false}	16DMNI
["baseid","prod","job05"]	{"platform": "platform05", "direction":"pull", "args": {"group":"02"}, "enabled": false}	13ZI63
["baseid","prod","job06"]	{"platform": "platform06", "direction":"pull", "args": {"group":"015"}, "enabled": true}	T6SIK
["baseid","prod","job06","10.0.0.1"]	{"status":"idle"}	R9WKE
["baseid","prod","job06","10.0.0.2"]	{"status":"idle"}	1YPVKM
["baseid","prod","job07"]	{"platform": "platform07", "direction":"pull", "args": {"group":"02"}, "enabled": true}	21GJH
["baseid","prod","job07","10.0.0.3"]	{"status":"error"}	1C0VIR
["baseid","prod","job08"]	{"platform": "platform08", "direction":"pull", "args": {"group":"03"}, "enabled": true}	1G8553
["baseid","prod","job08","10.0.0.2"]	{"status":"idle"}	1K5JDG
["baseid","prod","job09"]	{"platform": "platform09", "direction":"push", "args": {"group":"02"}, "enabled": true}	174V1F
["baseid","prod","job09","10.0.0.1"]	{"status":"idle"}	K6X90
["baseid","prod","job10"]	{"platform": "platform10", "direction":"pull", "args": {"group":"02"}, "enabled": true}	203DBM
["baseid","prod","job10","10.0.0.1"]	{"status": "refreshed"}	XC5I1
["baseid","stag","job11"]	{"platform": "platform11", "direction":"pull", "args": {"group":"01"}, "enabled": true}	1UQK0V
["baseid","stag","job11","192.168.0.1"]	{"status":"idle"}	IOW3P
["baseid","stag","job11","192.168.0.2"]	{"status":"idle"}	1YAQRX
["baseid","stag","job12"]	{"platform": "platform12", "direction":"pull", "args": {"group":"01"}, "enabled": true}	1NOQXI
["baseid","stag","job13"]	{"platform": "platform13", "direction":"pull", "args": {"group":"01"}, "enabled": false}	JPIZY
["baseid","stag","job14"]	{"platform": "platform14", "direction":"pull", "args": {"group":"01"}, "enabled": true}	2FBUU
["baseid","stag","job14","192.168.0.1"]	{"status": "idle"}	208ASX
["baseid","stag","job15"]	{"platform": "platform15", "direction":"pull", "args": {"group":"01"}, "enabled": false}	1EX091
["baseid","stag","job16"]	{"platform": "platform16", "direction":"pull", "args": {"group":"01"}, "enabled": true}	VORM4
["baseid","stag","job16","192.168.0.1"]	{"status": "cleaned"}	XYI25
["baseid","stag","job17"]	{"platform": "platform17", "direction":"pull", "args": {"group":"01"}, "enabled": true}	1LVEOW
["baseid","stag","job17","192.168.0.1"]	{"status": "idle"}	1DEQ20
["baseid","stag","job18"]	{"platform": "platform06", "direction":"pull", "args": {"group":"01"}, "enabled": true}	23SE1K
["baseid","stag","job18","192.168.0.1"]	{"status":"idle"}	PBG1Y
    *****/

    /** Size & positioning parameters */
    
    // virtual coordinates drawing arc radius
    var vArcRadius = 1000;
    // node radius in virtual coordinates
    var nradius = 100;
    var animDuration = 500;

    var initGraph = function() {
        return {
            links: {},
            nodes: {},
            status: {},
            center: {
                // Position relative to the bottom center after margin.
                stag: { position: { x: -1.9 * nradius, y: 0.3 * nradius }, status: 'idle' },
                prod: { position: { x: 0, y: -nradius }, status: 'idle' }
            }
        };
    }

    /** Helper functions for data extraction */
    var parseError = function(term) {
        return new Error(term + " is not a valid json term");
    }
    var getKey = function(row) {
        var k = [];
        try {
            k = JSON.parse(row.ckey_1);
        } catch (e) {
            throw parseError(row.ckey_1);
        }
        return k;
    };

    var getValue = function(row) {
        var v = {};
        try {
            v = JSON.parse(row.cvalue_2);
        } catch (e) {
            throw parseError(row.cvalue_2);
        }
        return v;
    };

    var extractLinksNodes = function(rows, graph) {
        var links = graph.links;
        var nodes = graph.nodes;
        var status = graph.status;
        var center = graph.center;
        rows.forEach(function(row) {
            var key = getKey(row);
            var value = getValue(row);

            if (key.length == 3 && key[1] == "heartbeat") {
                // Ignored heartbeat entries.
            } else if(key.length === 4 && key[2] != "error") {
                var triangleId = key[1] + '_' + key[2] + '_' + key[3];
                var jobId = key[1] + '_' + key[2];
                if (row.op === "del") {
                    delete status[triangleId];
                } else { 
                    status[triangleId] = {
                        id: triangleId,
                        job: jobId,
                        status: value.status
                    };
                }
            } else if(key.length === 4 && key[2] == "error") {
                if (row.op === "del") {
                    delete center[key[1]][key[3] + "_error"];
                    var still_errors = false;
                    var centerOtherKeys = Object.keys(center[key[1]]);
                    for(var kIdx = 0; kIdx < centerOtherKeys.length; ++kIdx) {
                        if(centerOtherKeys[kIdx].includes("_error")) {
                            still_errors = true;
                            break;
                        }
                    }
                    if (!still_errors) {
                        center[key[1]].status = "idle";
                    }
                } else {
                    console.log("Setting status error");
                    center[key[1]].status = "error";
                    center[key[1]][key[3] + "_error"] = value;
                }
            } else if(key.length === 3 && key[2] != "error") {
                var nodeId = value.platform;
                var group = value.args.group;
                if(!group) {
                    group = "01";
                }
                if(!center.hasOwnProperty(nodeId)) {
                    nodes[nodeId] = {
                        id: nodeId,
                        group: group
                    };
                }
                var linkId = key[1] + '_' + nodeId;
                var jobId = key[1] + '_' + key[2];
                var jobs = {};
                if(links.hasOwnProperty(linkId)) {
                    jobs = links[linkId].jobs;
                    enabled = links[linkId].enabled;
                }
                jobs[jobId] = {
                    id: jobId,
                    legend: key[2],
                    enabled: value.enabled,
                    direction: value.direction
                };
                var enabled = false;
                for(var jId in jobs) {
                    if(jobs[jId].enabled) {
                        enabled = true;
                        break;
                    }
                }

                links[linkId] = {
                    id: linkId,
                    source: key[1],
                    target: nodeId,
                    enabled: enabled,
                    jobs: jobs
                };
            }
        });
        return { links: links, nodes: nodes, status: status, center: center};
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
        stopped: 'lightgrey'
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

    var tooltipDiv = d3.select("body").append('div')
        .styles({
            position: "absolute",
            "text-align": "left",
            padding: "2px",
            font: "14px courier",
            border: "0px",
            "border-radius": "8px",
            "pointer-events": "none",
            opacity: 0,
            "z-index": 99996,
            "background-color": "lightsteelblue",
        });

    function showTooltip(d) {
        var txt = JSON.stringify(d, null, 2);
        var html = txt.split('\n').join('<br>').split(' ').join('&nbsp;');
        tooltipDiv
            .html(html)
            .transition()
            .duration(200)
            .style('opacity', 0.95);
    }

    function moveTooltip() {
        // Position the tooltip without letting it go outside the window.
        var availableHeight = document.documentElement.clientHeight;
        var availableWidth = document.documentElement.clientWidth;

        var d = tooltipDiv.node();
        var tooltipHeight = d.scrollHeight;
        var tooltipWidth = d.scrollWidth;

        var left = d3.event.pageX + 15;
        if(left + tooltipWidth + 5 > availableWidth) {
            left = Math.max(availableWidth-tooltipWidth-5, 0);
        }
        var top = d3.event.pageY + 15;
        if(top + tooltipHeight + 5 > availableHeight) {
            top = Math.max(availableHeight-tooltipHeight-5, 0);
        }

        tooltipDiv
            .style('left', left + "px")
            .style('top', top + "px");
    }

    function hideTooltip() {
        tooltipDiv.transition()
            .duration(animDuration)
            .style('opacity', 0);
    }

    function openView(d) {
        helper.browse('parameterized_d', {
            ':boolean_read' : {typ: "boolean", val: d.enabled.toString()}
        });
    }

    function entries(obj) {
        var res = [];
        for(var k in obj) {
            obj[k].id = k;
            res.push(obj[k]);
        }
        return res;
    }

    var graph = initGraph();
    var firstData = true;
    return {
        on_data: function(data) {
            if(data.length === 0) {
                return;
            }

            if(firstData) {
                firstData = false;

                // Add center nodes
                var NewCenterNodesDom = svg.selectAll('.center-nodes')
                    .data(entries(graph.center), function(d) {
                        return d.id;
                    })
                    .enter()
                    .append('g')
                    .attr("class", "center-nodes");

                NewCenterNodesDom.append('circle')
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

                NewCenterNodesDom.append('text')
                    .text(function(d) {
                        return d.id;
                    })
                    .style('font-size', '24px')
                    .style('text-anchor', 'middle');
            }

            graph = extractLinksNodes(data, graph);

            // Note: entries adds the id to the values in the original object
            var nodes = entries(graph.nodes);
            var links = entries(graph.links);
            var status = entries(graph.status);
            var center = entries(graph.center);

            var numberNodes = nodes.length;
            vArcRadius = Math.max((numberNodes*nradius*1.8)/Math.PI, 1000);
            vBox = {
                x: -1 * (vArcRadius + nradius),
                y: -1 * (vArcRadius + nradius),
                w: vArcRadius * 2 + nradius * 2,
                h: vArcRadius + 3 * nradius
            }
            svg.attr('viewBox', vBox.x + ' ' + vBox.y + ' ' + vBox.w + ' ' + vBox.h);

            console.log("center", center);

            svg.selectAll('.center-nodes')
                .data(center, function(d) {
                    return d.id;
                })
                .selectAll('circle')
                .style('fill', function(d) {
                    return colorStatus[d.status];
                });

            var newNodes = svg.selectAll('.node')
                .data(nodes, function(d) {
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
                .on('mouseout', hideTooltip);

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
            for(var k in graph.center) {
                positions[k] = graph.center[k].position;
            }

            allPoints
                .transition()
                .duration(animDuration)
                .attr('cx', function(d) {
                    return positions[d.id].x;
                })
                .attr('cy', function(d) {
                    return positions[d.id].y;
                });

            svg.selectAll('text')
                .transition()
                .duration(animDuration)
                .attr('x', function(d) {
                    console.log("Positions : ", positions);
                    console.log("ID : ", d);
                    return positions[d.id].x;
                })
                .attr('y', function(d) {
                    return positions[d.id].y;
                });

            svg.selectAll('line')
                .data(links, function(d) {
                    return d.id;
                })
                .enter()
                .insert('line', '.center-nodes')
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
                .duration(animDuration)
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
                    var jobsId = Object.keys(d.jobs);
                    for(var i = 0; i < status.length; ++i) {
                        if(d.jobs.hasOwnProperty(status[i].job)) {
                            if(status[i].status == "error") {
                                return 'red';
                            }
                        }
                    }
                    return d.enabled ? 'green' : 'lightgrey';
                });

            var linksMid = {};
            allLinks.each(function(d) {
                if(!positions[d.source] || !positions[d.target]) {
                    return;
                }
                var dirX = positions[d.source].x - positions[d.target].x;
                var dirY = positions[d.source].y - positions[d.target].y;
                var dirM = Math.sqrt(dirX*dirX + dirY*dirY);
                if(dirM > 0.01) {
                    dirX /= dirM;
                    dirY /= dirM;
                } else {
                    dirX = 0;
                    dirY = -1;
                }

                var jobsId = Object.keys(d.jobs);
                var step;
                if(jobsId.length !== 0) {
                    step = 0.8/jobsId.length;
                }

                var pullJobs = [];
                var pushJobs = [];
                for(var i = 0; i < jobsId.length; ++i) {
                    if(d.jobs[jobsId[i]].direction === "pull") {
                        pullJobs.push(d.jobs[jobsId[i]]);
                    } else {
                        pushJobs.push(d.jobs[jobsId[i]]);
                    }
                }

                // TODO: How to merge this too loops in one function... 
                var start = 0.225;
                var dir = {x: dirX, y: dirY};
                for(var i = 0; i < pushJobs.length; ++i) {
                    var pct = start + i * step;
                    var midX = (1 - pct) * positions[d.source].x + pct * positions[d.target].x;
                    var midY = (1 - pct) * positions[d.source].y + pct * positions[d.target].y;
                    linksMid[jobsId[i]] = {mid: {x: midX, y: midY}, direction: dir};
                }

                dir = {x: -dirX, y: -dirY};
                for(var i = 0; i < pullJobs.length; ++i) {
                    var pct = start + i * step;
                    var midX = pct * positions[d.source].x + (1 - pct) * positions[d.target].x;
                    var midY = pct * positions[d.source].y + (1 - pct) * positions[d.target].y;
                    linksMid[jobsId[i]] = {mid: {x: midX, y: midY}, direction: dir};
                }
            });

            var groupStatus = {};
            var statusPos = {};
            status.forEach(function(s) {
                if(!groupStatus.hasOwnProperty(s.job)) {
                    groupStatus[s.job] = 0;
                }
                statusPos[s.id] = groupStatus[s.job];
                groupStatus[s.job] = groupStatus[s.job] + 1;
            });

            var polySelection = svg.selectAll('polygon')
                .data(status, function(d) {
                    return d.id;
                });

            polySelection.exit().remove();

            polySelection.enter()
                .append('polygon')
                .attr('id', function(d) {
                    return d.id;
                })
                .attr('points', '0,0 -15,40 15,40')
                .on('mouseover', showTooltip)
                .on('mousemove', moveTooltip)
                .on('mouseout', hideTooltip);

            svg.selectAll('polygon')
                .transition()
                .duration(animDuration)
                .attr('transform', function(d) {
                    if(!linksMid[d.job]) {
                        console.log("Moving outside the visible area as we don't have a position yet", JSON.stringify(d.job));
                        return "translate(0, 250) rotate(180)"
                    } else {
                        var dx = linksMid[d.job].direction.x;
                        var dy = linksMid[d.job].direction.y;
                        var angle = -1 * Math.atan2(dx, dy) * 180 / Math.PI;
                        var x = linksMid[d.job].mid.x + statusPos[d.id] * dx * 20;
                        var y = linksMid[d.job].mid.y + statusPos[d.id] * dy * 20;
                        return "translate(" + x + ", " + y + ") rotate(" + angle + ")";
                    }
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
            graph = initGraph();
            firstData = true;
        },
        on_close: function() {
            tooltipDiv.remove();
        }
    };
}
