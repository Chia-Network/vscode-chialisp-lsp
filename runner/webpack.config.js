var path = require('path');
var webpack = require('webpack');

module.exports = {
    entry: './src/runner.js',
    output: {
        path: path.resolve(__dirname, 'build'),
        filename: 'runner.js'
    },
    target: 'node',
    loader: {
        "foo": 'babel-loader'
    },
    module: {
    },
    stats: {
        colors: true
    },
    devtool: 'source-map',
    experiments: {
	asyncWebAssembly: true
    }
};
