const HtmlWebpackPlugin = require('html-webpack-plugin');
const enabledSourceMap = true

module.exports = {

  entry: './src/js/app.js',

  module: {
    rules: [{
      test: /\.pug$/,
      loader: 'pug-loader',
    }, {
      test: /\.sass$/,
      use: [
        // style-loader inserts a link tag
        "style-loader",
        {
          loader: 'css-loader',
          options: {
            url: false,
            sourceMap: enabledSourceMap,
            // 2: postcss-loader & sass-loader
            importLoaders: 2
          }
        }, {
          loader: "sass-loader",
          options: {
            sourceMap: enabledSourceMap
          }
        },
      ]
    }],
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: './src/html/index.pug',
    }),
  ],

  mode: 'development',
  //mode: 'production',
};
