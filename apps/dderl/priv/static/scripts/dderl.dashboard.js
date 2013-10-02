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

        $.extend(this, {
            "getId": getId,
            "getName": getName,
            "getViews": getViews,
            "addView": addView,
            "removeView": removeView
        });

        init();
    }
}(jQuery));
