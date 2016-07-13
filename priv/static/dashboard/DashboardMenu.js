import $ from 'jquery';
import {dderlState, alert_jq, addDashboard} from '../scripts/dderl'; 

var dashboardList;

export function create(container, selectionCallback) {
    var saveButton, dashboardListObj, defaultOption;

    // Check to only create the elements once.
    if(document.getElementById("dashboard-list")) {
        return;
    }

    // Button creation
    saveButton = document.createElement("input");
    saveButton.type = "button";
    saveButton.id = "dashboard-save";
    saveButton.value = "Save this dash";
    saveButton.onclick = function() {
        checkTablesNotSaved();
    };

    // Default option creation
    defaultOption = document.createElement("option");
    defaultOption.value = "default";
    defaultOption.textContent = "default";

    // Dashboard list creation
    dashboardList = document.createElement("select");
    dashboardList.id = "dashboard-list";
    dashboardList.appendChild(defaultOption);

    // Add elements to the dom
    container.appendChild(dashboardList);
    container.appendChild(saveButton);

    // Convert the select to combobox
    dashboardListObj = $('#dashboard-list').combobox();

    // Add the handler for selection
    dashboardListObj.change(function() {
        console.log("Inside the change function");
        var dashId;
        dashId = dashboardListObj.val();
        if($('#dashboard-list-input').is(":focus")) {
            $('#dashboard-list-input').blur();
        }
        if(!isNaN(parseInt(dashId)) && isFinite(dashId)) {
            selectionCallback(parseInt(dashId));
        }
    });
}

export function add(id, name) {
    if(dashboardList && dashboardList.appendChild) {
        dashboardList.appendChild(new Option(name, id));
    }
}

export function save() {
    var name, dashboard, dashViews;

    name = document.getElementById("dashboard-list-input").value;
    if(name === "default") {
        alert_jq("Please select a name for the dashboard");
        return;
    }

    dashboard = findDashboard(name);
    dashViews = getCurrentViews();

    if(dashboard === null) {
        dashboard = new DDerl.Dashboard(-1, name, dashViews);
        dashboard.save(function() {
            addDashboard(dashboard);
        });
    } else {
        dashboard.updateViews(dashViews);
        dashboard.save();
    }
}

function checkTablesNotSaved() {
    var tablesNotSaved, notSavedTitles, message;
    tablesNotSaved = [];
    notSavedTitles = "";
    message = "";
    if(dderlState.currentWindows.length === dderlState.currentViews.length) {
        save();
    } else {
        notSavedTitles += "<ul>";
        for(var i = 0; i < dderlState.currentWindows.length; ++i) {
            if(!dderlState.currentWindows[i]._viewId) {
                tablesNotSaved.push(dderlState.currentWindows[i]);
                notSavedTitles += "<li>" + dderlState.currentWindows[i].options.title + "</li>";
            }
        }
        notSavedTitles += "</ul>";
        message = "The following tables are not saved as views: <br>" + notSavedTitles;
        $('<div><p><span class="ui-icon ui-icon-alert" style="float: left; margin: 0 7px 20px 0;"></span>'+ message +'</p></div>').appendTo(document.body).dialog({
            resizable: false,
            width: 450,
            height:220,
            modal: true,
            appendTo: "#main-body",
            buttons: {
                "Create views": function() {
                    $( this ).dialog( "close" );
                    dderlState.saveDashboardCounter += tablesNotSaved.length;
                    for(var i = 0; i < tablesNotSaved.length; ++i) {
                        tablesNotSaved[i].saveView();
                    }
                },
                "Ignore tables": function() {
                    $( this ).dialog( "close" );
                    save();
                },
                Cancel: function() {
                    $( this ).dialog( "close" );
                }
            },
            open: function() {
                $(this)
                    .dialog("widget")
                    .draggable("option","containment","#main-body");
            },
            close : function() {
                $(this).dialog('destroy');
                $(this).remove();
            }
        });
    }
}

function findDashboard(name) {
    for(var i = 0; i < dderlState.dashboards.length; ++i) {
        if(dderlState.dashboards[i].getName() === name) {
            return dderlState.dashboards[i];
        }
    }
    return null;
}

function getCurrentViews() {
    var resultViews, id, x, y, w, h;
    resultViews = [];
    for (var i = 0; i < dderlState.currentViews.length; ++i) {
        id = dderlState.currentViews[i]._viewId;
        x = dderlState.currentViews[i]._dlg.dialog('widget').position().left;
        y = dderlState.currentViews[i]._dlg.dialog('widget').position().top;
        w = dderlState.currentViews[i]._dlg.dialog('widget').width();
        h = dderlState.currentViews[i]._dlg.dialog('widget').height();
        resultViews.push(new DDerl.DashView(id, x, y, w, h));
    }
    return resultViews;
}