(function ($) {
    $.extend(true, window, {
        DDerl: {
            Dashboard: DashboardCreator
        }
    });

    function DashboardCreator(id, name, origViews) {
        var views;

        function init() {
            views = origViews.slice(0);
            origViews = null;
        }

        function getId() {
            return id;
        }

        function getName() {
            return name;
        }

        function getViews() {
            return views;
        }

        function getAsObject() {
            var result, viewsAsObject;
            result = {};
            viewsAsObject = [];
            for(var i = 0; i < views.length; ++i) {
                viewsAsObject.push(views[i].getAsObject());
            }
            result = {
                id: id,
                name: name,
                views: viewsAsObject
            };
            return result;
        }

        // Adding a view with same id replaces the old one
        function addView(view) {
            for(var i = 0; i < views.length; ++i) {
                if(views[i].getId() === view.getId()) {
                    views.splice(i, 1, view);
                    return;
                }
            }
            views.push(view);
        }

        function removeView(viewId) {
            for(var i = 0; i < views.length; ++i) {
                if(views[i].getId() === viewId) {
                    views.splice(i, 1);
                    return;
                }
            }
        }

        function updateViews(newViews) {
            views = newViews.slice(0);
            newViews = null;
        }

        function openViews() {
            for(var i = 0; i < views.length; ++i) {

                var openViewData = {open_view: {
                    connection: dderlState.connection,
                    view_id: views[i].getId(),
                    conn_id: dderlState.connectionSelected.connection
                }};

                ajaxCall(null, '/app/open_view', openViewData, 'open_view', (function(pos) { return function(viewResult) {
                    if(viewResult.hasOwnProperty('error')) {
                        alert_jq(viewResult.error);
                    } else {
                        $('<div>')
                            .appendTo(document.body)
                            .table({
                                autoOpen    : false,
                                dderlConn   : dderlState.connection,
                                dderlAdapter: dderlState.adapter,
                                title       : viewResult.name,
                                dderlTbllay : views[pos].getLayout(),
                            })
                            .table('openView', viewResult);
                    }
                }})(i));
            }
        }

        function save(cbSuccess) {
            var url = '/app/save_dashboard';
            var data = {dashboard: getAsObject()};
            ajaxCall(null, url, data, 'save_dashboard', function(result) {
                if(result.hasOwnProperty('error')) {
                    alert_jq('<strong>save dashboard failed!</strong><br><br>' + result.error);
                } else if(!isNaN(parseInt(result)) && isFinite(result)) {
                    if(id === -1) {
                        // Only update the id if we are creating a new dashboard
                        id = parseInt(result);
                    }
                    if(cbSuccess) {
                        cbSuccess();
                    }
                }
            });
        }

        $.extend(this, {
            "getId": getId,
            "getName": getName,
            "getViews": getViews,
            "addView": addView,
            "removeView": removeView,
            "updateViews": updateViews,
            "save": save,
            "getAsObject": getAsObject,
            "openViews": openViews
        });

        init();
    }
}(jQuery));
