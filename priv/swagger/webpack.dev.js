const merge = require('webpack-merge');
const common = require('./webpack.config.js');
const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = merge(common, {
    mode: "development",
    devtool: 'inline-source-map',
    devServer: {
        contentBase: '../public/swagger'
    },
    plugins: [
        new webpack.DefinePlugin({
            'process.env.NODE_ENV': JSON.stringify('development')
        }),
        new CopyWebpackPlugin(
            [{ from: '../dderlrest.json', to: '../swagger/dderlrest/0.0.1/' }],
            { debug: true }
        )
    ]
});