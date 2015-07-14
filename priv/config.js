var path = require('path');

exports.config = {
    paths: {
        public: "static/public",
        watched: [
            'static',
            'bower_components/jquery-ui/themes/smoothness/jquery-ui.css'
        ]
    },

    modules: {
        wrapper: false,
        definition: false
    },

    conventions: {
        ignored: 'static/public',
        assets: /^static(\\|\/)assets(\\|\/)/

    },

    files: {
        javascripts: {
            joinTo: {
                'js/vendor.js': /^bower_components/,
                'js/app.js': /^static/
            }
        },
        stylesheets: {
            joinTo: {
                'css/app.css': /^static/,
                'css/vendor.css': [
                    'bower_components/jquery-ui/themes/smoothness/jquery-ui.css'
                ]
            }
        }
    }
};
