# custom-event-polyfill

A polyfill for creating CustomEvents on IE9+ if the native implementation is missing.

[![Sauce Test Status](https://saucelabs.com/browser-matrix/kumarharsh.svg)](https://saucelabs.com/u/kumarharsh)


## Install

```bash
# using yarn
$ yarn add custom-event-polyfill

# using npm
$ npm install --save custom-event-polyfill
```

## Usage

Just include this polyfill before using any code which creates a CustomEvent
object, ideally as early as possible.

Refer to the CustomEvent documentation for details on how to use it:
https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent

## Big Thanks

Cross-browser Testing Platform and Open Source <3 Provided by [Sauce Labs](https://saucelabs.com).

## License

MIT
