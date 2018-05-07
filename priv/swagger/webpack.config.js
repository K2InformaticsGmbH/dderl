const pkg = require('./package.json');
const path = require('path');
const webpack = require('webpack');

const HtmlWebpackPlugin = require('html-webpack-plugin');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const FaviconsWebpackPlugin = require('favicons-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
    entry: { swagger: './index.js' },
    optimization: {
        splitChunks: {
            cacheGroups: {
                vendor: {
                    test: /node_modules/,
                    chunks: "initial",
                    name: "vendor",
                    priority: 10,
                    enforce: true
                }
            }
        }
    },
    output: {
        filename: '[name][hash:6].js',
        path: path.resolve(__dirname, '../public/swagger')
    },
    plugins: [
        new CleanWebpackPlugin(['../public/swagger']),
        new webpack.ProvidePlugin({
            $: 'jquery',
            jQuery: 'jquery',
            'window.jQuery': 'jquery'
        }),
        new HtmlWebpackPlugin({
            title: pkg.description + ' ' + pkg.version
        }),
        new FaviconsWebpackPlugin('./swagger-32x32.png'),
        new CopyWebpackPlugin(
            [{ from: 'brand.json', to: '../swagger' },
            { from: './logo.png', to: '../swagger/' }],
            { debug: true }
        )
    ],
    module: {
        rules: [
            {
                test: /\.(png|jpe?g|gif|svg|woff|woff2|ttf|eot|ico|mp3|ogg)$/,
                loader: 'file-loader',
                options: {
                    name: '[name][hash:6].[ext]'
                }
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            },
            {
                test: /\.html$/, loader: 'html-loader',
                options: {
                    minimize: false,
                    attrs: 'img:src link:href source:src'
                }
            }
        ]
    }
};