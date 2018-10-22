function initGraph(container, width, height) {
    // sample query 
    // SELECT SYSDATE, CURRENT_TIMESTAMP FROM DUAL

    var viewName = 'test_interval_req';

    // Add the interval input 
    var intervalDiv = container
        .append('div')
        .style("margin", "10px");

    var inputId = "interval_" + Math.random().toString(36).substr(2, 14);

    intervalDiv
        .append('label')
        .attr("for", inputId)
        .text("Refresh interval (Seconds): ");

    var intervalInput = intervalDiv
        .append('input')
        .attr("id", inputId)
        .attr("value", "1")
        .style("border", "solid 1px black")
        .style("border-radius", "3px")
        .style("padding", "1px 0px 1px 4px");

    var runBtn = intervalDiv
        .append('button')
        .text('Start');

    var tableDiv = container
        .append('div')
        .style('padding', '0px 10px')
        .style('overflow-y', 'auto')
        .style('height', 'calc(100% - 50px)');

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
    var tbody = table.append('tbody')
        .attr('tabIndex', '1');
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
        stopInterval();
        var interval = parseInt(inp.value);

        if(isNaN(interval)) {
            helper.openDialog("Error", "<span style='color: red;'>Refresh interval must be an integer</span>", {x: 100, y: 100});
        } else {
            var cb = function(data, closeFn) {
                // Close the stmt.
                closeFn();

                if(data.length > 0) { appendRows(data); }

                // Check if view is still waiting for the results.
                if(document.body.contains(inp)) {
                    timeoutId = setTimeout(function() {
                        helper.runView(viewName, cb);
                    }, interval * 1000);
                } else {
                    console.log("Interval request dom element not found stopping.")
                }

            }
            runBtn.text("Stop");
            helper.runView(viewName, cb);
        }
    }

    tbodyElement = tbody.node();
    tbodyElement.onkeydown = function(e) {
        var c = e.keyCode;
        var ctrlDown = e.ctrlKey || e.metaKey;

        // Check for ctrl+c
        if(ctrlDown && c === 67) {
            console.log("ctrl+C detected inside interval graph");
            var selection = window.getSelection();
            // Validate is a valid selection and not collapsed.
            if(selection.rangeCount != 1) { return; }
            var range = selection.getRangeAt(0);
            if(range.collapsed) { return; }

            var start = range.startOffset;
            var length = range.endOffset - start;
            if(length < 1) {
                console.log("invalid length on selection...", length);
                return;
            }

            if(range.startContainer.parentNode.nodeName != 'TD' ||
            range.endContainer.parentNode.nodeName != 'TD') {
                console.log("Selection should be only on the table, something went wrong...");
                return;
            }

            // User parentNode for the test as textNodes do not count
            // on IE11 for contains test.
            if(!tbodyElement.contains(range.startContainer.parentNode) ||
            !tbodyElement.contains(range.endContainer.parentNode)) {
                console.log("selection starts or end outside of tbody");
                return;
            }

            e.stopPropagation();
            if(range.startContainer != range.endContainer) {
                console.log("Making selection 2D");
                //TODO: This only works if there is one column...
                //      multiple column support could be added calculating
                //      td index using node.previousElementSibling if ever required.
                var currentEl = range.startContainer;
                var endEl = range.endContainer;
                var start = range.startOffset;
                var end = range.endOffset;

                // Commented out for now, own highlight / selection is required as
                // there is no support for multiple range selection in chrome or I.E
                // selection.removeAllRanges();
                var textContent = "";
                while(currentEl != endEl && currentEl) {
                    /*var newRange = document.createRange();
                    newRange.setStart(currentEl, start);
                    newRange.setEnd(currentEl, end);
                    // this doesn't work on ie or chrome or IE...
                    selection.addRange(newRange); */
                    var line = currentEl.textContent;
                    textContent += (line.substring(start, end) + "\r\n");
                    var currentTR = currentEl.parentNode.parentNode.nextElementSibling;
                    currentEl = currentTR.firstChild.firstChild;
                }
                // Add last line
                if(currentEl) {
                    var line = currentEl.textContent;
                    // Note: Here a new line can be added to the end of the copy.
                    textContent += line.substring(start, end);
                }
                copyText(textContent, selection, range);
            }
        }
    }

    function createCopyTextBox(innerText) {
        var ta = document.createElement('textarea');
        ta.style.position = 'absolute';
        ta.style.left = '-1000px';
        ta.style.top = document.body.scrollTop + 'px';
        ta.value = innerText;
        document.body.appendChild(ta);
        ta.select();

        return ta;
    }

    function copyText(innerText, selection, range) {
        var focusElement = document.activeElement;
        selection.removeAllRanges();
        var ta = createCopyTextBox(innerText);
        ta.focus();

        setTimeout(function() {
            document.body.removeChild(ta);
            // restore focus
            if (focusElement) {
                focusElement.focus();
                selection.addRange(range);
            }
        }, 100);
    }

    var firstData = true;
    var rowCounter = 0;
    var longestLine = 0;

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
                if(i % 2 == 0) { return '#f2f2f2'; }
                else { return 'white'; }
            });

        rows.selectAll('td')
            .data(function(row) {
                var result = [];
                var pad = '';
                for(k in row) {
                    if(k != 'id' && k != 'op') {
                        if(row[k].length >= longestLine) {
                            longestLine = row[k].length;
                            result.push({value: row[k]});
                        } else {
                            pad = new Array(longestLine - row[k].length + 1).join(' ');
                            result.push({value: row[k] + pad});
                        }
                    }
                }
                return result;
            })
            .enter()
            .append('td')
            .text(function(d) { return d.value; })
            .style('padding', '2px 4px')
            .style('white-space', 'pre');

        var tableDivElement = tableDiv.node();
        tableDivElement.scrollTop = tableDivElement.scrollHeight;
    }

    //Data fetch is started by default
    startInterval();
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
            longestLine = 0;
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
