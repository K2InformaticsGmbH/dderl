function initGraph(container, width, height) {
    // sample query 
    // SELECT SYSDATE, CURRENT_TIMESTAMP FROM DUAL

    var viewName = 'test_interval_req';

    // Add the interval input 
    var intervalDiv = container
        .append('div')
        .style("margin", "10px");

    var inputId = "interval_" + Math.random().toString(36).substr(2, 14);

    var label = intervalDiv
        .append('label')
        .attr("for", inputId)
        .text("Refresh interval (Seconds): ");

    var intervalInput = intervalDiv
        .append('input')
        .attr("id", inputId)
        .style("border", "solid 1px black")
        .style("border-radius", "3px")
        .style("padding", "1px 0px 1px 4px");

    var runBtn = intervalDiv
        .append('button')
        .text('Start');

    var tableDiv = container
        .append('div')
        .style('padding', '0px 10px')
        .style('overflow-y', 'auto');

    var table = tableDiv.append('table')
        .style('border-collapse', 'collapse')
        .style('border', '1px solid black')
        .style('width', '100%')
        .style('font-family', 'Courier New,Courier,monospace')
        .style('-webkit-user-select', 'text')
        .style('-moz-user-select', 'text')
        .style('-ms-user-select', 'text')
        .style('user-select', 'text');

    var thead = table.append('thead');
    var tbody = table.append('tbody');
    var timeoutId;

    intervalInput.on("keypress", function() {
        if(d3.event.keyCode == 13) {
            startInterval();
        }
    });

    runBtn.on("click", function() {
        if(runBtn.text() == "Start") {
            startInterval();
        } else {
            stopInterval();
        }
    })

    function stopInterval() {
        clearTimeout(timeoutId);
        timeoutId = null;
        runBtn.text("Start");
    }

    function startInterval() {
        var inp = intervalInput.node();
        console.log("the value", inp.value);
        stopInterval();
        var interval = parseInt(inp.value);

        if(isNaN(interval)) {
            helper.openDialog("Error", "<span style='color: red;'>Refresh interval must be an integer</span>", {x: 100, y: 100});
        } else {
            var cb = function(data, closeFn) {
                // Close the stmt.
                closeFn();
                appendRows(data);
                timeoutId = setTimeout(function() {
                    helper.runView(viewName, cb);
                }, interval * 1000);
            }
            runBtn.text("Stop");
            helper.runView('test_interval_req', cb);
        }
    }

    var firstData = true;
    var rowCounter = 0;

    function appendRows(data) {
        var rows = tbody.selectAll('tr')
            .data(data, function(d) {
                rowCounter += 1;
                return rowCounter;
            })
            .enter()
            .append('tr');

        tbody.selectAll('tr')
            .style('background-color', function(d, i) {
                console.log("the i", i);
                    if(i % 2 == 0) {
                        return '#f2f2f2';
                    } else {
                        return 'white';
                    }
                });

        rows.selectAll('td')
            .data(function(row) {
                var result = [];
                for(k in row) {
                    if(k != 'id' && k != 'op') {
                        result.push({value: row[k]});
                    }
                }
                return result;
            })
            .enter()
            .append('td')
            .text(function(d) { return d.value; })
            .style('padding', '2px 4px')
            .style('white-space', 'pre');
    }

    return {
        on_data: function(data) {
            // Process data and add draw here.
            console.log("the new data arrived", data);

            //If empty data we don't do anything.
            if(data.length === 0) { return; }

            if(firstData) {
                firstData = false;
            }

            appendRows(data);
        },
        on_resize: function(w, h) {},
        on_reset: function() {
            // Called when the button clear the graph is clicked.
            stopInterval();
            firstData = true;
            rowCounter = 0;
            tbody.selectAll('tr').remove();
        },
        on_close: function() {
            clearTimeout(timeoutId);
            // This should cleanup event listeners and element added
            // outside the container, the container itself will be removed
            // after this function call.
        }
    };
}
