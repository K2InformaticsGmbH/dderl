(function ($) {
    $.extend(true, window, {
        DDerl: {
            DashView: DashViewCreator
        }
    });

    // Create a new instance of a DashView
    function DashViewCreator(id, x, y, width, height) {
        var position, size, layout;

        function init() {
            position = {"x": x, "y": y};
            size = {"width": width, "height": height};
            layout = {"x": x, "y": y, "width": width, "height": height};
        }

        function getId() {
            return id;
        }

        function getPos() {
            return position;
        }

        function getSize() {
            return size;
        }

        function getLayout() {
            return layout;
        }

        $.extend(this, {
            "getId": getId,
            "getPos": getPos,
            "getSize": getSize,
            "getLayout": getLayout
        });

        init();
    }
}(jQuery));
