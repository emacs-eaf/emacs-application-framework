# strip-css-comments [![Build Status](https://travis-ci.org/sindresorhus/strip-css-comments.svg?branch=master)](https://travis-ci.org/sindresorhus/strip-css-comments)

> Strip comments from CSS

Also available as a [gulp](https://github.com/sindresorhus/gulp-strip-css-comments)/[grunt](https://github.com/sindresorhus/grunt-strip-css-comments)/[broccoli](https://github.com/sindresorhus/broccoli-strip-css-comments) plugin.


## Usage

```
$ npm install --save strip-css-comments
```

```js
var stripCssComments = require('strip-css-comments');

// by default important comments `/*!` are preserved
stripCssComments('/*! <copyright> */ body { /* unicorns */color: hotpink; }');
//=> '/*! <copyright> */ body { color: hotpink; }'

// `preserve: false` will strip all comments including `/*!`
stripCssComments(
	'/*! <copyright> */ body { /* unicorns */color: hotpink; }',
	{preserve: false}
);
//=> 'body { color: hotpink; }'

// preserve comments based on a regex
stripCssComments(
	'/*# preserved */ body { /* unicorns */color: hotpink; }',
	{preserve: /^#/}
);
//=> '/*# preserved */ body { color: hotpink; }'

// preserve comments based on the return value of the supplied function
stripCssComments(
	'/*# preserved */ body { /* unicorns */color: hotpink; }',
	{
		preserve: function (comment) {
			return comment.charAt(0) === '#';
		}
	}
);
//=> '/*# preserved */ body { color: hotpink; }'
```


## API

### stripCssComments(input, [options])

## input

*Required*  
Type: `string`

String with CSS.

## options

### preserve

Type: `boolean`, `RegExp`, `function`  
Default: `true`

- `true` - Preserve comments that use the `/*! */` syntax
- `false` - Strip all comments
- `RegExp` - Preserve comments where the comment body matches a regular expression.
- `Function` - Preserve comments for which a function returns `true`. The function is called on each comment, gets the comment body as the first argument, and is expected to return a boolean of whether to preserve the comment.


## Benchmark

```
$ npm run bench
```


## Related

- [strip-css-comments-cli](https://github.com/sindresorhus/strip-css-comments-cli) - CLI for this module
- [strip-json-comments](https://github.com/sindresorhus/strip-json-comments) - Strip comments from JSON


## License

MIT © [Sindre Sorhus](http://sindresorhus.com)
