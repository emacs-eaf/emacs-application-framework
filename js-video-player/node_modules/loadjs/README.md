# LoadJS

<img src="https://www.muicss.com/static/images/loadjs.svg" width="250px">

LoadJS is a tiny async loader for modern browsers (899 bytes).

[![Dependency Status](https://david-dm.org/muicss/loadjs.svg)](https://david-dm.org/muicss/loadjs)
[![devDependency Status](https://david-dm.org/muicss/loadjs/dev-status.svg)](https://david-dm.org/muicss/loadjs?type=dev)
[![CDNJS](https://img.shields.io/cdnjs/v/loadjs.svg)](https://cdnjs.com/libraries/loadjs)

## Introduction

LoadJS is a tiny async loading library for modern browsers (IE9+). It has a simple yet powerful dependency management system that lets you fetch JavaScript, CSS and image files in parallel and execute code after the dependencies have been met. The recommended way to use LoadJS is to include the minified source code of [loadjs.js](https://raw.githubusercontent.com/muicss/loadjs/master/dist/loadjs.min.js) in your &lt;html&gt; (possibly in the &lt;head&gt; tag) and then use the `loadjs` global to manage JavaScript dependencies after pageload.

LoadJS is based on the excellent [$script](https://github.com/ded/script.js) library by [Dustin Diaz](https://github.com/ded). We kept the behavior of the library the same but we re-wrote the code from scratch to add support for success/error callbacks and to optimize the library for modern browsers. LoadJS is 899 bytes (minified + gzipped).

Here's an example of what you can do with LoadJS:

```javascript
// define a dependency bundle and execute code when it loads
loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar');

loadjs.ready('foobar', function() {
  /* foo.js & bar.js loaded */
});
```

You can also use more advanced syntax for more options:
```javascript
// define a dependency bundle with advanced options
loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar', {
  before: function(path, scriptEl) { /* execute code before fetch */ },
  async: true,  // load files synchronously or asynchronously (default: true)
  numRetries: 3  // see caveats about using numRetries with async:false (default: 0),
  returnPromise: false  // return Promise object (default: false)
});

loadjs.ready('foobar', {
  success: function() { /* foo.js & bar.js loaded */ },
  error: function(depsNotFound) { /* foobar bundle load failed */ },
});
```

The latest version of LoadJS can be found in the `dist/` directory in this repository:
 * [https://cdn.rawgit.com/muicss/loadjs/4.2.0/dist/loadjs.js](https://cdn.rawgit.com/muicss/loadjs/4.2.0/dist/loadjs.js) (for development)
 * [https://cdn.rawgit.com/muicss/loadjs/4.2.0/dist/loadjs.min.js](https://cdn.rawgit.com/muicss/loadjs/4.2.0/dist/loadjs.min.js) (for production)

You can also use it as a CJS or AMD module:

```bash
$ npm install --save loadjs
```

```javascript
var loadjs = require('loadjs');

loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar');

loadjs.ready('foobar', function() {
  /* foo.js & bar.js loaded */
});
```

## Browser Support

 * IE9+ (`async: false` support only works in IE10+)
 * Opera 12+
 * Safari 5+
 * Chrome
 * Firefox
 * iOS 6+
 * Android 4.4+

LoadJS also detects script load failures from AdBlock Plus and Ghostery in:

 * Safari
 * Chrome

Note: LoadJS treats empty CSS files as load failures in IE9-11 and uses `rel="preload"` to load CSS files in Edge (to get around lack of support for onerror events on `<link rel="stylesheet">` tags)

## Documentation

1. Load a single file

    ```javascript
    loadjs('/path/to/foo.js', function() {
      /* foo.js loaded */
    });
    ```

1. Fetch files in parallel and load them asynchronously

    ```javascript
    loadjs(['/path/to/foo.js', '/path/to/bar.js'], function() {
      /* foo.js and bar.js loaded */
    });
    ```

1. Fetch JavaScript, CSS and image files

    ```javascript
    loadjs(['/path/to/foo.css', '/path/to/bar.png', 'path/to/thunk.js'], function() {
      /* foo.css, bar.png and thunk.js loaded */
    });
    ```

1. Force treat file as CSS stylesheet

    ```javascript
    loadjs(['css!/path/to/cssfile.custom'], function() {
      /* cssfile.custom loaded as stylesheet */
    });
    ```

1. Force treat file as image

    ```javascript
    loadjs(['img!/path/to/image.custom'], function() {
      /* image.custom loaded */
    });
    ```

1. Add a bundle id

    ```javascript
    loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar', function() {
      /* foo.js & bar.js loaded */
    });
    ```

1. Use .ready() to define bundles and callbacks separately

    ```javascript
    loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar');

    loadjs.ready('foobar', function() {
      /* foo.js & bar.js loaded */
    });
    ```

1. Use multiple bundles in .ready() dependency lists

    ```javascript
    loadjs('/path/to/foo.js', 'foo');
    loadjs(['/path/to/bar1.js', '/path/to/bar2.js'], 'bar');

    loadjs.ready(['foo', 'bar'], function() {
      /* foo.js & bar1.js & bar2.js loaded */
    });
    ```

1. Chain .ready() together

    ```javascript
    loadjs('/path/to/foo.js', 'foo');
    loadjs('/path/to/bar.js', 'bar');

    loadjs
      .ready('foo', function() {
        /* foo.js loaded */
      })
      .ready('bar', function() {
        /* bar.js loaded */
      });
    ```

1. Use Promises to register callbacks

   ```javascript
   loadjs(['/path/to/foo.js', '/path/to/bar.js'], {returnPromise: true})
     .then(function() { /* foo.js & bar.js loaded */ })
     .catch(function(pathsNotFound) { /* at least one didn't load */ });
   ```

1. Check if bundle has already been defined

    ```javascript
    if (!loadjs.isDefined('foobar')) {
      loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar', function() {
        /* foo.js & bar.js loaded */
      });
    }
    ```

1. Fetch files in parallel and load them in series

    ```javascript
    loadjs(['/path/to/foo.js', '/path/to/bar.js'], {
      success: function() { /* foo.js and bar.js loaded in series */ },
      async: false
    });
    ```

1. Add an error callback

    ```javascript
    loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar', {
      success: function() { /* foo.js & bar.js loaded */ },
      error: function(pathsNotFound) { /* at least one path didn't load */ }
    });
    ```

1. Retry files before calling the error callback

    ```javascript
    loadjs(['/path/to/foo.js', '/path/to/bar.js'], 'foobar', {
      success: function() { /* foo.js & bar.js loaded */ },
      error: function(pathsNotFound) { /* at least one path didn't load */ },
      numRetries: 3
    });
    
    // NOTE: Using `numRetries` with `async: false` can cause files to load out-of-sync on retries
    ```

1. Execute a callback before script tags are embedded

    ```javascript
    loadjs(['/path/to/foo.js', '/path/to/bar.js'], {
      success: function() {},
      error: function(pathsNotFound) {},
      before: function(path, scriptEl) {
        /* called for each script node before being embedded */
        if (path === '/path/to/foo.js') scriptEl.crossOrigin = true;
      }
    });
    ```

1. Bypass LoadJS default DOM insertion mechanism (DOM `<head>`)

    ```javascript
    loadjs(['/path/to/foo.js'], {
      success: function() {},
      error: function(pathsNotFound) {},
      before: function(path, scriptEl) {
        document.body.appendChild(scriptEl);
      
        /* return `false` to bypass default DOM insertion mechanism */
        return false;
      }
    });
    ```

1. Use bundle ids in error callback

    ```javascript
    loadjs('/path/to/foo.js', 'foo');
    loadjs('/path/to/bar.js', 'bar');
    loadjs(['/path/to/thunkor.js', '/path/to/thunky.js'], 'thunk');

    // wait for multiple depdendencies
    loadjs.ready(['foo', 'bar', 'thunk'], {
      success: function() {
        // foo.js & bar.js & thunkor.js & thunky.js loaded
      },
      error: function(depsNotFound) {
        if (depsNotFound.indexOf('foo') > -1) {};  // foo failed
        if (depsNotFound.indexOf('bar') > -1) {};  // bar failed
        if (depsNotFound.indexOf('thunk') > -1) {};  // thunk failed
      }
    });
    ```
  
1. Use .done() for more control

    ```javascript
    loadjs.ready(['dependency1', 'dependency2'], function() {
      /* run code after dependencies have been met */
    });

    function fn1() {
      loadjs.done('dependency1');
    }
  
    function fn2() {
      loadjs.done('dependency2');
    }
    ```

1. Reset dependency trackers

    ```javascript
    loadjs.reset();
    ```

1. Implement a require-like dependency manager

    ```javascript
    var bundles = {
      'bundleA': ['/file1.js', '/file2.js'],
      'bundleB': ['/file3.js', '/file4.js']
    };

    function require(bundleIds, callbackFn) {
      bundleIds.forEach(function(bundleId) {
        if (!loadjs.isDefined(bundleId)) loadjs(bundles[bundleId], bundleId);
      });
      loadjs.ready(bundleIds, callbackFn);
    }

    require(['bundleA'], function() { /* bundleA loaded */ });
    require(['bundleB'], function() { /* bundleB loaded */ });
    require(['bundleA', 'bundleB'], function() { /* bundleA and bundleB loaded */ });
    ```

## Directory structure

<pre>
loadjs/
├── dist
│   ├── loadjs.js
│   ├── loadjs.min.js
│   └── loadjs.umd.js
├── examples
├── gulpfile.js
├── LICENSE.txt
├── package.json
├── README.md
├── src
│   └── loadjs.js
├── test
└── umd-templates
</pre>

## Development Quickstart

1. Install dependencies

    * [nodejs](http://nodejs.org/)
    * [npm](https://www.npmjs.org/)
    * http-server (via npm)

1. Clone repository

    ```bash
    $ git clone git@github.com:muicss/loadjs.git
    $ cd loadjs
    ```

1. Install node dependencies using npm

    ```bash
    $ npm install
    ```

1. Build examples

    ```bash
    $ npm run build-examples
    ```

    To view the examples you can use any static file server. To use the `nodejs` http-server module:

    ```bash
    $ npm install http-server
    $ npm run http-server -- -p 3000
    ```

    Then visit [http://localhost:3000/examples](http://localhost:3000/examples)

1. Build distribution files

    ```bash
    $ npm run build-dist
    ```

    The files will be located in the `dist` directory.

1. Run tests

     To run the browser tests first build the `loadjs` library:

     ```bash
     $ npm run build-tests
     ```

     Then visit [http://localhost:3000/test](http://localhost:3000/test)

1. Build all files

     ```bash
     $ npm run build-all
     ```

