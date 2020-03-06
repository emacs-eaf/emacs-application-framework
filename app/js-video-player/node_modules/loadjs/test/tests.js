/**
 * loadjs tests
 * @module test/tests.js
 */

var pathsLoaded = null,  // file register
    testEl = null,
    assert = chai.assert,
    expect = chai.expect;


describe('LoadJS tests', function() {

  beforeEach(function() {
    // reset register
    pathsLoaded = {};

    // reset loadjs dependencies
    loadjs.reset();
  });

  // ==========================================================================
  // JavaScript file loading tests
  // ==========================================================================

  describe('JavaScript file loading tests', function() {

    it('should call success callback on valid path', function(done) {
      loadjs(['assets/file1.js'], {
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          done();
        }
      });
    });


    it('should call error callback on invalid path', function(done) {
      loadjs(['assets/file-doesntexist.js'], {
        success: function() {
          throw "Executed success callback";
        },
        error: function(pathsNotFound) {
          assert.equal(pathsNotFound.length, 1);
          assert.equal(pathsNotFound[0], 'assets/file-doesntexist.js');
          done();
        }
      });
    });


    it('should call before callback before embedding into document', function(done) {
      var scriptTags = [];

      loadjs(['assets/file1.js', 'assets/file2.js'], {
        before: function(path, el) {
          scriptTags.push({
            path: path,
            el: el
          });

          // add cross origin script for file2
          if (path === 'assets/file2.js') {
            el.crossOrigin = 'anonymous';
          }
        },
        success: function() {
          assert.equal(scriptTags[0].path, 'assets/file1.js');
          assert.equal(scriptTags[1].path, 'assets/file2.js');

          assert.equal(scriptTags[0].el.crossOrigin, undefined);
          assert.equal(scriptTags[1].el.crossOrigin, 'anonymous');

          done();
        }
      });
    });


    it('should bypass insertion if before returns `false`', function(done) {
      loadjs(['assets/file1.js'], {
        before: function(path, el) {
          // append to body (instead of head)
          document.body.appendChild(el);

          // return `false` to bypass default DOM insertion
          return false;
        },
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          
          // verify that file was added to body
          var els = document.body.querySelectorAll('script'),
              el;

          for (var i=0; i < els.length; i++) {
            el = els[i];
            if (el.src.indexOf('assets/file1.js') !== -1) done();
          }
        }
      });
    });


    it('should call success callback on two valid paths', function(done) {
      loadjs(['assets/file1.js', 'assets/file2.js'], {
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(pathsLoaded['file2.js'], true);
          done();
        }
      });
    });


    it('should call error callback on one invalid path', function(done) {
      loadjs(['assets/file1.js', 'assets/file-doesntexist.js'], {
        success: function() {
          throw "Executed success callback";
        },
        error: function(pathsNotFound) {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(pathsNotFound.length, 1);
          assert.equal(pathsNotFound[0], 'assets/file-doesntexist.js');
          done();
        }
      });
    });


    it('should support async false', function(done) {
      this.timeout(5000);

      var numCompleted = 0,
          numTests = 20,
          paths = ['assets/asyncfalse1.js', 'assets/asyncfalse2.js'];

      // run tests sequentially
      var testFn = function(paths) {
        // add cache busters
        var pathsUncached = paths.slice(0);
        pathsUncached[0] += '?_=' + Math.random();
        pathsUncached[1] += '?_=' + Math.random();

        loadjs(pathsUncached, {
          success: function() {
            var f1 = paths[0].replace('assets/', '');
            var f2 = paths[1].replace('assets/', '');

            // check load order
            assert.isTrue(pathsLoaded[f1]);
            assert.isFalse(pathsLoaded[f2]);

            // increment tests
            numCompleted += 1;

            if (numCompleted === numTests) {
              // exit
              done();
            } else {
              // reset register
              pathsLoaded = {};

              // run test again
              paths.reverse();
              testFn(paths);
            }
          },
          async: false
        });
      };

      // run tests
      testFn(paths);
    });


    it('should support multiple tries', function(done) {
      loadjs('assets/file-numretries.js', {
        error: function() {
          // check number of scripts in document
          var selector = 'script[src="assets/file-numretries.js"]',
              scripts = document.querySelectorAll(selector);
          if (scripts.length === 2) done();
        },
        numRetries: 1
      });
    });


    // Un-'x' this for testing ad blocked scripts.
    //   Ghostery: Disallow "Google Adservices"
    //   AdBlock Plus: Add "www.googletagservices.com/tag/js/gpt.js" as a
    //   custom filter under Options
    //
    xit('it should report ad blocked scripts as missing', function(done) {
      var s1 = 'https://www.googletagservices.com/tag/js/gpt.js',
          s2 = 'https://munchkin.marketo.net/munchkin-beta.js';

      loadjs([s1, s2, 'assets/file1.js'], {
        success: function() {
          throw new Error('Executed success callback');
        },
        error: function(pathsNotFound) {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(pathsNotFound.length, 2);
          assert.equal(pathsNotFound[0], s1);
          assert.equal(pathsNotFound[1], s2);
          done();
        }
      });
    });
  });


  // ==========================================================================
  // CSS file loading tests
  // ==========================================================================

  describe('CSS file loading tests', function() {

    before(function() {
      // add test div to body for css tests
      testEl = document.createElement('div');
      testEl.className = 'test-div mui-container';
      testEl.style.display = 'inline-block';
      document.body.appendChild(testEl);
    });


    afterEach(function() {
      var els = document.getElementsByTagName('link'),
          i = els.length,
          el;

      // iteratete through stylesheets
      while (i--) {
        el = els[i];

        // remove test stylesheets
        if (el.href.indexOf('mocha.css') === -1) {
          el.parentNode.removeChild(el);
        }
      }
    });


    it('should load one file', function(done) {
      loadjs(['assets/file1.css'], {
        success: function() {
          assert.equal(testEl.offsetWidth, 100);
          done();
        }
      });
    });


    it('should load multiple files', function(done) {
      loadjs(['assets/file1.css', 'assets/file2.css'], {
        success: function() {
          assert.equal(testEl.offsetWidth, 200);
          done();
        }
      });
    });


    it('should call error callback on one invalid path', function(done) {
      loadjs(['assets/file1.css', 'assets/file-doesntexist.css'], {
        success: function() {
          throw new Error('Executed success callback');
        },
        error: function(pathsNotFound) {
          assert.equal(testEl.offsetWidth, 100);
          assert.equal(pathsNotFound.length, 1);
          assert.equal(pathsNotFound[0], 'assets/file-doesntexist.css');
          done();
        }
      });
    });


    it('should support mix of css and js', function(done) {
      loadjs(['assets/file1.css', 'assets/file1.js'], {
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(testEl.offsetWidth, 100);
          done();
        }
      });
    });


    it('should support forced "css!" files', function(done) {
      loadjs(['css!assets/file1.css'], {
        success: function() {
          // loop through files
          var els = document.getElementsByTagName('link'),
              i = els.length,
              el;

          while (i--) {
            if (els[i].href.indexOf('file1.css') !== -1) done();
          }
        }
      });
    });


    it('supports urls with query arguments', function(done) {
      loadjs(['assets/file1.css?x=x'], {
        success: function() {
          assert.equal(testEl.offsetWidth, 100);
          done();
        }
      });
    });


    it('supports urls with anchor tags', function(done) {
      loadjs(['assets/file1.css#anchortag'], {
        success: function() {
          assert.equal(testEl.offsetWidth, 100);
          done();
        }
      });
    });


    it('supports urls with query arguments and anchor tags', function(done) {
      loadjs(['assets/file1.css?x=x#anchortag'], {
        success: function() {
          assert.equal(testEl.offsetWidth, 100);
          done();
        }
      });
    });

    
    it('should load external css files', function(done) {
      this.timeout(0);

      loadjs(['//cdn.muicss.com/mui-0.6.8/css/mui.min.css'], {
        success: function() {
          var styleObj = getComputedStyle(testEl);

          assert.equal(styleObj.getPropertyValue('padding-left'), '15px');
          done();
        }
      });
    });


    it('should call error on missing external file', function(done) {
      this.timeout(0);

      loadjs(['//cdn.muicss.com/mui-0.6.8/css/mui-doesnotexist.min.css'], {
        success: function() {
          throw new Error('Executed success callback');
        },
        error: function(pathsNotFound) {
          var styleObj = getComputedStyle(testEl);

          assert.equal(styleObj.getPropertyValue('padding-left'), '0px');
          assert.equal(pathsNotFound.length, 1);
          done();
        }
      });
    });


    // teardown
    return after(function() {
      // remove test div
      testEl.parentNode.removeChild(testEl);
    });
  });


  // ==========================================================================
  // Image file loading tests
  // ==========================================================================

  describe('Image file loading tests', function() {

    function assertLoaded(src) {
      // loop through images
      var imgs = document.getElementsByTagName('img');

      Array.prototype.slice.call(imgs).forEach(function(img) {
        // verify image was loaded
        if (img.src === src) assert.equal(img.naturalWidth > 0, true);
      });
    }

    
    function assertNotLoaded(src) {
      // loop through images
      var imgs = document.getElementsByTagName('img');

      Array.prototype.slice.call(imgs).forEach(function(img) {
        // fail if image was loaded
        if (img.src === src) assert.equal(img.naturalWidth, 0);
      });
    }


    it('should load one file', function(done) {
      loadjs(['assets/flash.png'], {
        success: function() {
	  assertLoaded('assets/flash.png');
          done();
        }
      });
    });


    it('should load multiple files', function(done) {
      loadjs(['assets/flash.png', 'assets/flash.jpg'], {
        success: function() {
	  assertLoaded('assets/flash.png');
          assertLoaded('assets/flash.jpg');
          done();
        }
      });
    });


    it('detects png|gif|jpg|svg|webp extensions', function(done) {
      let files = [
        'assets/flash.png',
        'assets/flash.gif',
        'assets/flash.jpg',
        'assets/flash.svg',
        'assets/flash.webp'
      ];

      loadjs(files, function() {
        files.forEach(file => {assertLoaded(file);});
        done();
      });
    });

    
    it('supports urls with query arguments', function(done) {
      var src = 'assets/flash.png?' + Math.random();

      loadjs([src], {
        success: function() {
          assertLoaded(src);
          done();
        }
      });
    });


    it('supports urls with anchor tags', function(done) {
      var src = 'assets/flash.png#' + Math.random();

      loadjs([src], {
        success: function() {
          assertLoaded(src);
          done();
        }
      });
    });


    it('supports urls with query arguments and anchor tags', function(done) {
      var src = 'assets/flash.png';
      src += '?' + Math.random();
      src += '#' + Math.random();

      loadjs([src], {
        success: function() {
          assertLoaded(src);
          done();
        }
      });
    });
    
    
    it('should support forced "img!" files', function(done) {
      var src = 'assets/flash.png?' + Math.random();

      loadjs(['img!' + src], {
        success: function() {
          assertLoaded(src);
          done();
        }
      });
    });


    it('should call error callback on one invalid path', function(done) {
      var src1 = 'assets/flash.png?' + Math.random(),
          src2 = 'assets/flash-doesntexist.png?' + Math.random(); 
      
      loadjs(['img!' + src1, 'img!' + src2], {
        success: function() {
          throw new Error('Executed success callback');
        },
        error: function(pathsNotFound) {
          assert.equal(pathsNotFound.length, 1);
          assertLoaded(src1);
          assertNotLoaded(src2);
          done();
        }
      });
    });


    it('should support mix of img and js', function(done) {
      var src = 'assets/flash.png?' + Math.random();
      
      loadjs(['img!' + src, 'assets/file1.js'], {
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          assertLoaded(src);
          done();
        }
      });
    });


    it('should load external img files', function(done) {
      this.timeout(0);

      var src = 'https://www.muicss.com/static/images/mui-logo.png?';
      src += Math.random();

      loadjs(['img!' + src], {
        success: function() {
          assertLoaded(src);
          done();
        }
      });
    });


    it('should call error on missing external file', function(done) {
      this.timeout(0);

      var src = 'https://www.muicss.com/static/images/';
      src += 'mui-logo-doesntexist.png?' + Math.random();
      
      loadjs(['img!' + src], {
        success: function() {
          throw new Error('Executed success callback');
        },
        error: function(pathsNotFound) {
          assertNotLoaded(src);
          done();
        }
      });
    });
  });


  // ==========================================================================
  // API tests
  // ==========================================================================

  describe('API tests', function() {

    it('should throw an error if bundle is already defined', function() {
      // define bundle
      loadjs(['assets/file1.js'], 'bundle');

      // define bundle again
      var fn = function() {
        loadjs(['assets/file1.js'], 'bundle');
      };

      expect(fn).to.throw("LoadJS");
    });


    it('should create a bundle id and a callback inline', function(done) {
      loadjs(['assets/file1.js', 'assets/file2.js'], 'bundle', {
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(pathsLoaded['file2.js'], true);
          done();
        }
      });
    });


    it('should chain loadjs object', function(done) {
      function bothDone() {
        if (pathsLoaded['file1.js'] && pathsLoaded['file2.js']) done();
      }

      // define bundles
      loadjs('assets/file1.js', 'bundle1');
      loadjs('assets/file2.js', 'bundle2');

      loadjs
        .ready('bundle1', {
          success: function() {
            assert.equal(pathsLoaded['file1.js'], true);
            bothDone();
          }})
        .ready('bundle2', {
          success: function() {
            assert.equal(pathsLoaded['file2.js'], true);
            bothDone();
          }
        });
    });


    it('should handle multiple dependencies', function(done) {
      loadjs('assets/file1.js', 'bundle1');
      loadjs('assets/file2.js', 'bundle2');

      loadjs.ready(['bundle1', 'bundle2'], {
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(pathsLoaded['file2.js'], true);
          done();
        }
      });
    });


    it('should error on missing depdendencies', function(done) {
      loadjs('assets/file1.js', 'bundle1');
      loadjs('assets/file-doesntexist.js', 'bundle2');

      loadjs.ready(['bundle1', 'bundle2'], {
        success: function() {
          throw "Executed success callback";
        },
        error: function(depsNotFound) {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(depsNotFound.length, 1);
          assert.equal(depsNotFound[0], 'bundle2');
          done();
        }
      });
    });


    it('should execute callbacks on .done()', function(done) {
      // add handler
      loadjs.ready('plugin', {
        success: function() {
          done();
        }
      });

      // execute done
      loadjs.done('plugin');
    });


    it('should execute callbacks created after .done()', function(done) {
      // execute done
      loadjs.done('plugin');

      // add handler
      loadjs.ready('plugin', {
        success: function() {
          done();
        }
      });
    });


    it('should define bundles', function(done) {
      // define bundle
      loadjs(['assets/file1.js', 'assets/file2.js'], 'bundle');

      // use 1 second delay to let files load
      setTimeout(function() {
        loadjs.ready('bundle', {
          success: function() {
            assert.equal(pathsLoaded['file1.js'], true);
            assert.equal(pathsLoaded['file2.js'], true);
            done();
          }
        });
      }, 1000);
    });


    it('should allow bundle callbacks before definitions', function(done) {
      // define callback
      loadjs.ready('bundle', {
        success: function() {
          assert.equal(pathsLoaded['file1.js'], true);
          assert.equal(pathsLoaded['file2.js'], true);
          done();
        }
      });

      // use 1 second delay
      setTimeout(function() {
        loadjs(['assets/file1.js', 'assets/file2.js'], 'bundle');
      }, 1000);
    });


    it('should reset dependencies statuses', function() {
      loadjs(['assets/file1.js'], 'cleared');
      loadjs.reset();

      // define bundle again
      var fn = function() {
        loadjs(['assets/file1.js'], 'cleared');
      };

      expect(fn).not.to.throw("LoadJS");
    });


    it('should indicate if bundle has already been defined', function() {
      loadjs(['assets/file1/js'], 'bundle1');

      assert.equal(loadjs.isDefined('bundle1'), true);
      assert.equal(loadjs.isDefined('bundleXX'), false);
    });


    it('should accept success callback functions to loadjs()', function(done) {
      loadjs('assets/file1.js', function() {
        done();
      });
    });


    it('should accept success callback functions to .ready()', function(done) {
      loadjs.done('plugin');
      loadjs.ready('plugin', function() {
        done();
      });
    });


    it('should return Promise object if returnPromise is true', function() {
      var prom = loadjs(['assets/file1.js'], {returnPromise: true});

      // verify that response object is a Promise
      assert.equal(prom instanceof Promise, true);
    });


    it('Promise object should support resolutions', function(done) {
      var prom = loadjs(['assets/file1.js'], {returnPromise: true});

      prom.then(function() {
        assert.equal(pathsLoaded['file1.js'], true);
        done();
      });
    });


    it('Promise object should support rejections', function(done) {
      var prom = loadjs(['assets/file-doesntexist.js'], {returnPromise: true});

      prom.then(
        function(){},
        function(pathsNotFound) {
          assert.equal(pathsNotFound.length, 1);
          assert.equal(pathsNotFound[0], 'assets/file-doesntexist.js');
          done();
        }
      );
    });


    it('Promise object should support catches', function(done) {
      var prom = loadjs(['assets/file-doesntexist.js'], {returnPromise: true});

      prom
        .catch(function(pathsNotFound) {
          assert.equal(pathsNotFound.length, 1);
          assert.equal(pathsNotFound[0], 'assets/file-doesntexist.js');
          done();
        });
    });


    it('supports Promises and success callbacks', function(done) {
      var numCompleted = 0;

      function completedFn() {
        numCompleted += 1;
        if (numCompleted === 2) done();
      };
      
      var prom = loadjs('assets/file1.js', {
        success: completedFn,
        returnPromise: true
      });

      prom.then(completedFn);
    });


    it('supports Promises and bundle ready events', function(done) {
      var numCompleted = 0;

      function completedFn() {
        numCompleted += 1;
        if (numCompleted === 2) done();
      };
      
      loadjs('assets/file1.js', 'bundle1', {returnPromise: true})
        .then(completedFn);

      loadjs.ready('bundle1', completedFn);
    });
  });
});
