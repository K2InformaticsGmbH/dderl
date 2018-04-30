const path = require('path');
const webpack = require('webpack');

const PATHS = {
    app: path.join(__dirname, 'static', 'index.js'),
    index: path.join(__dirname, 'static', 'index.html'),
    public: path.join(__dirname, '../public'),
    static: path.join(__dirname, 'static')
}

module.exports = {
    entry: {
        app: PATHS.app,
        // This is the only html, maybe we should just generate it.
        index: PATHS.index,
        "babel-polyfill": "babel-polyfill",
        // Added web workers for monaco editor.
        "editor.worker": 'monaco-editor/esm/vs/editor/editor.worker.js',
        "json.worker": 'monaco-editor/esm/vs/language/json/json.worker',
        "css.worker": 'monaco-editor/esm/vs/language/css/css.worker',
        "html.worker": 'monaco-editor/esm/vs/language/html/html.worker',
        "ts.worker": 'monaco-editor/esm/vs/language/typescript/ts.worker'
    },
    output: {
        path: PATHS.public,
        publicPath: "public/",
        filename: '[name].js'
    },
    devtool: 'source-map',
    module: {
        rules: [
            {
                test: /\.(png|jpe?g|gif|svg|woff|woff2|ttf|eot|ico|mp3|ogg)$/,
                loader: 'file-loader',
                options: {
                    name: '[path][name].[hash:6].[ext]'
                }
            },
            {
                // For font awesome the version is required.
                test: /\.(svg|woff|woff2|ttf|eot)(\?v=[0-9]\.[0-9]\.[0-9])$/,
                loader: 'file-loader',
                options: {
                    name: 'font-awesome/fonts/[name].[hash:6].ext'
                }
            },
            {
                test: /\.css$/,
                use: [
                    'style-loader',
                    'css-loader'
                ]
            },
            {
                // Remove this loader when the html is generated.
                test: PATHS.index,
                use: [
                    {
                        loader: 'file-loader',
                        options: {
                            name: '[name].[ext]'
                        }
                    },
                    {
                        loader: 'extract-loader'
                    },
                    {
                        loader: 'html-loader',
                        options: {
                            attrs: 'img:src link:href source:src'
                        }
                    }
                ]
            },
            {
                test: /\.js$/,
                loader: 'babel-loader',
                include: PATHS.static,
                query: {
                    presets: ["env"],
                    compact: false
                }
            }
        ]
    },
    plugins: [
        new webpack.IgnorePlugin(/^((fs)|(path)|(os)|(crypto)|(source-map-support))$/, /vs(\\|\/)language(\\|\/)typescript(\\|\/)lib/)
    ]
};

