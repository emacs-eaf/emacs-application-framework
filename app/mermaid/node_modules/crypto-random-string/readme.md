# crypto-random-string [![Build Status](https://travis-ci.org/sindresorhus/crypto-random-string.svg?branch=master)](https://travis-ci.org/sindresorhus/crypto-random-string)

> Generate a [cryptographically strong](https://en.wikipedia.org/wiki/Strong_cryptography) random string

Can be useful for creating an identifier, slug, salt, PIN code, fixture, etc.

## Install

```
$ npm install crypto-random-string
```

## Usage

```js
const cryptoRandomString = require('crypto-random-string');

cryptoRandomString({length: 10});
//=> '2cf05d94db'

cryptoRandomString({length: 10, type: 'base64'});
//=> 'YMiMbaQl6I'

cryptoRandomString({length: 10, type: 'url-safe'});
//=> 'YN-tqc8pOw'

cryptoRandomString({length: 10, type: 'numeric'});
//=> '8314659141'

cryptoRandomString({length: 6, type: 'distinguishable'});
//=> 'CDEHKM'

cryptoRandomString({length: 10, characters: 'abc'});
//=> 'abaaccabac'
```

## API

### cryptoRandomString(options)

Returns a randomized string. [Hex](https://en.wikipedia.org/wiki/Hexadecimal) by default.

#### options

Type: `object`

##### length

*Required*\
Type: `number`

Length of the returned string.

##### type

Type: `string`\
Default: `'hex'`\
Values: `'hex' | 'base64' | 'url-safe' | 'numeric' | 'distinguishable'`

Use only characters from a predefined set of allowed characters.

Cannot be set at the same time as the `characters` option.

The `distinguishable` set contains only uppercase characters that are not easily confused: `CDEHKMPRTUWXY012458`. It can be useful if you need to print out a short string that you'd like users to read and type back in with minimal errors. For example, reading a code off of a screen that needs to be typed into a phone to connect two devices.

##### characters

Type: `string`\
Minimum length: `1`\
Maximum length: `65536`

Use only characters from a custom set of allowed characters.

Cannot be set at the same time as the `type` option.

## Related

- [random-int](https://github.com/sindresorhus/random-int) - Generate a random integer
- [random-float](https://github.com/sindresorhus/random-float) - Generate a random float
- [random-item](https://github.com/sindresorhus/random-item) - Get a random item from an array
- [random-boolean](https://github.com/arthurvr/random-boolean) - Get a random boolean
- [random-obj-key](https://github.com/sindresorhus/random-obj-key) - Get a random key from an object
- [random-obj-prop](https://github.com/sindresorhus/random-obj-prop) - Get a random property from an object
- [unique-random](https://github.com/sindresorhus/unique-random) - Generate random numbers that are consecutively unique

---

<div align="center">
	<b>
		<a href="https://tidelift.com/subscription/pkg/npm-crypto-random-string?utm_source=npm-crypto-random-string&utm_medium=referral&utm_campaign=readme">Get professional support for this package with a Tidelift subscription</a>
	</b>
	<br>
	<sub>
		Tidelift helps make open source sustainable for maintainers while giving companies<br>assurances about security, maintenance, and licensing for their dependencies.
	</sub>
</div>
