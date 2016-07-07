const path = require('path');
const webpack = require('webpack');

const PATHS = {
    app: path.join(__dirname, 'static', 'index.js'),
    index: path.join(__dirname, 'static', 'index.html'),
    public: path.join(__dirname, 'public'),
    static: path.join(__dirname, 'static')
}

module.exports = {
    entry: {
        app: PATHS.app,
        // This is the only html, maybe we should just generate it.
        index: PATHS.index
    },
    output: {
        path: PATHS.public,
        publicPath: "public/",
        filename: '[name].js'
    },
    devtool: 'source-map',
    module: {
        loaders: [
            {
                test: /\.(png|jpe?g|gif|svg|woff|woff2|ttf|eot|ico|mp3|ogg)$/,
                loader: 'file?name=[path][name].[hash:6].[ext]'
            },
            {
                test: /\.css$/,
                loader: 'style!css'
            },
            {
                // Remove this loader when the html is generated.
                test: PATHS.index,
                loaders: [
                    'file?name=[name].[ext]',
                    'extract',
                    'html?attrs=img:src link:href source:src'
                ]
            },
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
    plugins: [
        new webpack.ProvidePlugin({
            $: 'jquery'
        })

    ],
    debug: true
};

