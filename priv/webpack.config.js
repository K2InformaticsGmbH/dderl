const path = require('path');

// Here we define 
const PATHS = {
    app: path.join(__dirname, 'static', 'dderl.js'),
    public: path.join(__dirname, 'public'),
    static: path.join(__dirname, 'static')
}

module.exports = {
    entry: {
        app: PATHS.app
    },
    output: {
        path: PATHS.public,
        filename: '[name].js'
    },
    devtool: 'source-map',
    module: {
        loaders: [
            {
                test: /\.js$/,
                loader: 'babel-loader',
                include: PATHS.static,
                query: {
                    presets: ["es2015"],  
                }
            }
        ]
    },
    debug: true
};

