function init(container, width, height) {
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // To create the skvh table execute:
    // create imem_dal_skvh table arc_example()

    // Run the query:
    // select * from arc_example

    // Copy and paste the following data in the table:
    /*****

["prod", "platform1"]	{"color":"#ccffcc","linkedTo":[]}	IS9VH
["prod", "platform1", "10.0.0.1"]	{"color":"#ccffcc","linkedTo":[]}	1M1WKR
["prod", "platform2"]	{"color":"olive","linkedTo":["center2"]}	1PTC7Y
["prod", "platform3"]	{"color":"red","linkedTo":["center2"]}	8XMU8
["prod", "platform3", "10.0.0.1"]	{"color":"#ffcc99","linkedTo":["center2"]}	1U1MLL
["prod", "platform3", "10.0.0.2"]	{"color":"yellow","linkedTo":["center2"]}	J17TF
["prod", "platform3", "10.0.0.3"]	{"color":"green","linkedTo":["center1"]}	V7JL3
["prod", "platform4"]	{"color":"teal","linkedTo":["center1"]}	REACA
["prod", "platform5"]	{"color":"navy","linkedTo":["center1"]}	72Q5E
["prod", "platform6"]	{"color":"green","linkedTo":["center1"]}	GQU6N
["prod", "platform6", "10.0.0.1"]	{"color":"green","linkedTo":["center1"]}	22FNZY
["prod", "platform6", "10.0.0.2"]	{"color":"#ffcc99","linkedTo":["center1"]}	1EUXCI
["prod", "platform7"]	{"color":"#ccffcc","linkedTo":[]}	IS9VH
["prod", "platform7", "10.0.0.3"]	{"color":"#ccffcc","linkedTo":[]}	1M1WKR
["prod", "platform8"]	{"color":"olive","linkedTo":["center2"]}	1PTC7Y
["prod", "platform8", "10.0.0.2"]	{"color":"red","linkedTo":["center2"]}	8XMU8
["stag", "platform9"]	{"color":"#ffcc99","linkedTo":["center2"]}	1U1MLL
["stag", "platform9", "192.168.0.2"]	{"color":"yellow","linkedTo":["center2"]}	J17TF
["stag", "platform6"]	{"color":"green","linkedTo":["center1"]}	V7JL3
["stag", "platform6", "192.168.0.1"]	{"color":"teal","linkedTo":["center1"]}	REACA
["stag", "platform10"]	{"color":"navy","linkedTo":["center1"]}	72Q5E
["stag", "platform11"]	{"color":"green","linkedTo":["center1"]}	GQU6N
["stag", "platform11", "192.168.0.1"]	{"color":"green","linkedTo":["center1"]}	22FNZY
["stag", "platform11", "192.168.0.2"]	{"color":"#ffcc99","linkedTo":["center1"]}	1EUXCI

    *****/
    var margin = { top: 10, right: 10, bottom: 10, left: 10 }; 	// physical margins in px
    var getKey = function (row) {
        return JSON.parse(row.ckey_1);
    };

    var getValue = function (row) {
        return JSON.parse(row.cvalue_2);
    };

    var colorStatus = {
        idle: "black",
        error: "red",
        ok: "green"
    };

    var centerNodes = {
        // Position relative to the bottom center after margin.
        prod: { position: { x: 20, y: -30 } },
        stag: { position: { x: -40, y: -10 } }
    };

    // virtual coordinates drawing arc radius
    var vArcRadius = 1000;
    // node radius in virtual coordinates
    var nradius = 30;
    // To see the complete circle when drawing negative coordinates
    // and width and height for the virtual coordinates
    var vBox = {
        x: -1 * (vArcRadius + nradius),
        y: -1 * nradius,
        w: vArcRadius*2 + nradius*2,
        h: vArcRadius + nradius
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

    return {
        on_data: function (data) {
            console.log("the new data arrived", data);

            var angle = Math.PI/(data.length + 1);

            var points = svg
                .selectAll('circle')
                .data(data, function (d) { return d.id; })
                .enter()
                .append('circle');

            points
                .attr('r', function (d) { return nradius; })
                .attr('cx', function (d, i) {
                    return vArcRadius * Math.cos((i+1) * angle) * -1; })
                .attr('cy', function (d, i) {
                    return vArcRadius * (1 - Math.sin((i+1) * angle));
                });
        },
        on_resize: resize,
        on_reset: function () { }
    };
}
