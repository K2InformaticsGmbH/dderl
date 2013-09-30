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

        $.extend(this, {
            "getId": getId,
            "getName": getName,
            "getViews": getViews
        });

        init();
    }
}(jQuery));
