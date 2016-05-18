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
        ignored: 'static/public/**',
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
                'css/app.css': [
                    'static/styles/slick.grid.css',
                    'static/styles/slick.columnpicker.css',
                    'static/styles/dropdown.css',
                    'static/styles/dderl.sql.css',
                    'static/styles/dderl.connect.css',
                    'static/styles/dderl.css'
                ],
                'css/vendor.css': [
                    'bower_components/jquery-ui/themes/smoothness/jquery-ui.css',
                    'bower_components/font-awesome/css/font-awesome.css'
                ]
            }
        }
    },
    
    npm: {
        enabled: false
    }
};
