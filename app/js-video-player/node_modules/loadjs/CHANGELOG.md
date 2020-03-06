# LoadJS Changelog

## 4.2.0 - December 1, 2019

* Added support for .webp image extensions

## 4.0.1 - September 25, 2019

* Removed unnecessary console.log() statement

## 4.0.0 - September 25, 2019

* Added support for automatic image and css detection on urls with with query arguments and anchor tags

## 3.6.1 - April 11, 2019

* Upgraded devDependencies
* Fixed issue with source code documentation (https://github.com/muicss/loadjs/issues/88)

## 3.6.0 - March 14, 2019

* Bugfix for IE Edge CSS file cross-domain onerror detection
* Added support for Promises via `returnPromise` option

## 3.5.5 - December 8, 2018

* Bugfix for IE Edge CSS file cross-domain SecurityError

## 3.5.4 - March 28, 2018

* Added support for loading images

## 3.5.3 - March 21, 2018

* Updated documentation

## 3.5.2 - January 10, 2018

* Added support for success function as callback argument

## 3.5.1 - August 9, 2017

* Upgraded devDependencies and re-built payload

## 3.5.0 - March 28, 2017

* Added support for "css!" prefix to force treating file as stylesheet
* Added support for DOM insertion bypass if `before` callback returns `false`

## 3.4.0 - February 23, 2017

* Added isDefined() method to check if a bundle is already defined

## 3.3.1 - January 11, 2017

* Minor code cleanup

## 3.3.0 - January 9, 2017

* Added reset() method to reset dependency trackers

## 3.2.1 - December 18, 2016

* Minor code cleanup

## 3.2.0 - December 11, 2016

* Added `before` callback hook to modify script/link elements before adding
  them to the DOM

## 3.1.0 - December 9, 2016

* Added numRetries option

## 3.0.0 - August 25, 2016

* Changed 'fail' callback name to 'error'
* Fixed bug in main attribute of bower.json

## 2.1.2 - August 22, 2016

* Upgraded devDependencies, rebuilt packaged, saved a byte

## 2.1.1 - July 25, 2016

* Fixed bug causing issues with external css files

## 2.1.0 - June 19, 2016

* Added support for loading CSS files

## 2.0.0 - June 15, 2016

* Changed API to accept object with success/fail functions
* Added support for async: false

## 1.0.4 - May 25, 2016

* Added support for ad blocked script failures

## 1.0.3 - May 18, 2016

* Shaved off 3 more bytes (minified + gzipped)

## 1.0.2 - May 18, 2016

* Added bower.json
* Removed onload script deletion

## 1.0.1 - March 22, 2016

* Small improvement to internal code to save a few bytes

## 1.0.0 - March 21, 2016

* Added UMD support
