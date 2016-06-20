module.exports = {
    entry: [
        './src/main'
    ],
    output: {
        publicPath: 'static/public',
        filename: 'app.js'
    },
    devtool: 'source-map',
    module: {
        loaders: [
            {
                test: /\.js$/,
                include: path.join(__dirname, 'src'),
                loader: 'babel-loader',
                query: {
                    presets: ["es2015"],  
                }
            }
        ]
    },
    debug: true
};

