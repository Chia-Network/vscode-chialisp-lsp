var path = require('path');
var webpack = require('webpack');

module.exports = {
    entry: {
        runner: './src/runner.js',
        gdb_runner: './src/gdb_runner.js'
    },
    output: {
        path: path.resolve(__dirname, 'build'),
        filename: '[name].js'
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
