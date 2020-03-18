# RangeTouch

A super tiny library to make `<input type="range">` sliders work better on touch devices.

[Donate](#donate) - [Demo](https://rangetouch.com) - [![npm version](https://badge.fury.io/js/rangetouch.svg)](https://badge.fury.io/js/rangetouch)

## Why bother?

While building [plyr](https://plyr.io) I noticed how bad the experience was trying to use `<input type="range">` is on a touch device (particularly iOS). Touching the track on a desktop will jump the thumb handle to that point. However on some touch devices this simply focuses the input and to adjust the value you need to touch and drag the handle. This is something that I can't help but feel will eventually be fixed by the browser vendors but for now, you can use RangeTouch to fill that gap.

## Features

-   Less than 1KB minified and gzipped
-   No dependencies (written in "vanilla" JavaScript)
-   Uses event delgation so no need to re-run after DOM manipulation

## Quick setup

### 1. Include the lib

Either use the ES6 module:

```javascript
import RangeTouch from 'rangetouch';
```

...or include the script:

```html
<script src="https://rangetouch.com/1.0.6/rangetouch.js"></script>
```

### 2. Create instance(s)

#### Single instance

```javascript
const range = new RangeTouch('input[type="range"]', { ...options });
```

The first argument can either be:

-   a [valid CSS selector](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector)
-   an [Element](https://developer.mozilla.org/en-US/docs/Web/API/Element)

The second argument is for options.

This will return a reference to the single instance.

#### Multiple instances

To setup multiple inputs at one time, you can use the following static method:

```javascript
const ranges = RangeTouch.setup('input[type="range"]', { ...options });
```

The first argument can either be:

-   a [valid CSS selector](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelector)
-   an [Element](https://developer.mozilla.org/en-US/docs/Web/API/Element)
-   an [NodeList](https://developer.mozilla.org/en-US/docs/Web/API/NodeList)
-   an [Element](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array) of [Element](https://developer.mozilla.org/en-US/docs/Web/API/Element)

The second argument is for options.

This will return an array of RangeTouch instances that it setup.

## Options

| Property   | Type    | Default | Description                                                                                                                                                                                                                                                                                                                                                                    |
| ---------- | ------- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| addCSS     | Boolean | `true`  | Whether to inject CSS to improve the usability of the inputs. It's recommended you add this yourself if you don't want RangeTouch to take care of it.                                                                                                                                                                                                                          |
| thumbWidth | Integer | `15`    | This value is used as part of the calculation to determine the value the users selects when touching the range track. Unfortunately as JavaScript can't access the shadow DOM, this value can't be automatically determined. I would recommend customisation (and setting the size of the thumb) given all OS and browser combinations seem to render the control differently. |
| watch      | Boolean | `true`  | Watch for new elements added to the DOM that match your string selector. **Note**: This only applies when using the multiple instance `RangeTouch` setup method and also requires a string selector as the first argument.                                                                                                                                                     |

## API

| Method    | Arguments | Description                                              |
| --------- | --------- | -------------------------------------------------------- |
| destroy() | N/A       | Destroy the current instance and remove event listeners. |

To call an API method, you need a reference to the instance. For example:

```javascript
const range = new RangeTouch('input[type="range"]', { ...options });
range.destroy();
```

## Issues

If you find anything weird with RangeTouch, please let us know using the [GitHub issues tracker](https://github.com/sampotts/rangetouch/issues) and be descriptive on how to reproduce, expected result, the browser (and version) used, etc.

## Author

RangeTouch is developed by [@sam_potts](https://twitter.com/sam_potts) / [sampotts.me](http://sampotts.me)

## Donate

RangeTouch costs money to run for domains, hosting and more. Any help is appreciated...
[Donate to support RangeTouch](https://www.paypal.me/pottsy/20usd)

## Thanks

[![Fastly](https://cdn.plyr.io/static/fastly-logo.png)](https://www.fastly.com/)

Thanks to [Fastly](https://www.fastly.com/) for providing the CDN services.

## Copyright and License

[The MIT license](license.md).
