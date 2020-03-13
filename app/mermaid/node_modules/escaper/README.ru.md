Escaper
=======

Библиотека для «экранирования» литералов строк, регулярных выражений и комментариев в синтаксисе JavaScript.

[Документация на английском](https://github.com/kobezzza/Escaper/blob/master/README.md)

[![NPM version](http://img.shields.io/npm/v/escaper.svg?style=flat)](http://badge.fury.io/js/escaper)
[![NPM devDependencies](http://img.shields.io/david/dev/kobezzza/Escaper.svg?style=flat)](https://david-dm.org/kobezzza/Escaper?type=dev)
[![Build Status](http://img.shields.io/travis/kobezzza/Escaper.svg?style=flat&branch=master)](https://travis-ci.org/kobezzza/Escaper)
[![Coverage Status](http://img.shields.io/coveralls/kobezzza/Escaper.svg?style=flat)](https://coveralls.io/r/kobezzza/Escaper?branch=master)

**Поддерживаются литералы:**

* `' ... '`
* `" ... "`
* `` ` ... ` ``, `` ` ... ${...} ` ``
* `/ ... /`
* `// ...`, `//* ...`, `//! ...`, `//# ...`, `//@ ...`, `//$ ...`
* `/* ... */`, `/** ... */`, `/*! ... */`, `/*# ... */`, `/*@ ... */`, `/*$ ... */`

## Установка

https://raw.githubusercontent.com/kobezzza/Escaper/master/dist/escaper.min.js

или

```bash
npm install escaper
```

или

```bash
bower install escaper
```

или

```bash
git clone https://github.com/kobezzza/Escaper
```

## Использование

```js
var str = '"foo" 1 + /foo/ + 2 /* 1 */ 3',
    content = [];

// __ESCAPER_QUOT__0_ 1 + __ESCAPER_QUOT__1_ + 2 __ESCAPER_QUOT__2_ 3
str = Escaper.replace(str, true, content);

// "foo" 1 + /foo/ + 2 /* 1 */ 3
Escaper.paste(str, content);
```

## API
### Escaper.replace(str, opt_withComment, opt_content)

Заметить блоки вида `' ... '`, `" ... "`, `` ` ... ` ``, `/ ... /`, `// ...`, `/* ... */` на
`__ESCAPER_QUOT__номер_` в указанной строке и вернуть новую строку.

**Аргументы**

* `string` `str` — исходная строка;
* `(Object|boolean)=` `opt_withCommentsOrParams = false` — таблица вырезаемых последовательностей:

Если установить значение параметру `-1`, то последовательность будет удаляться,
т.е. без возможности обратной замены, иначе `true`/`false` — включить/исключить последовательность.

```js
{
   // Шаблон замены
  '@label'   : '__ESCAPER_QUOT__${pos}_',

  '@all'     : true, // Вырезаются все последовательности
  '@comments': true, // Вырезаются все виды комментариев
  '@strings' : true, // Вырезаются все виды литералов строк
  '@literals': true, // Вырезаются все виды литералов строк
                     // и регулярных выражений

  "'"        : true,
  '"'        : true,
  '`'        : true,
  '/'        : true,
  '//'       : true,
  '//*'      : true,
  '//!'      : true,
  '//#'      : true,
  '//@'      : true,
  '//$'      : true,
  '/*'       : true,
  '/**'      : true,
  '/*!'      : true,
  '/*#'      : true,
  '/*@'      : true,
  '/*$'      : true
}
```

ИЛИ если `opt_withCommentsOrParams` — логическое значение, то

```js
true  // Вырезаются литералы с комментариями
false // Вырезаются одни литералы
```

* `Array=` `opt_content = Escaper.content` — стек содержимого.

`@return {string}`

### Escaper.paste(str, opt_content)

Заметить `__ESCAPER_QUOT__номер_` в указанной строке на реальное содержимое и вернуть новую строку.

**Аргументы**

* `string` `str` — исходная строка;
* `Array=` `opt_content = Escaper.content` — стек содержимого;
* `RegExp=` `opt_rgxp` — регулярное выражение для поиска, например `/__ESCAPER_QUOT__(\d+)_/g`.

`@return {string}`

## [Лицензия](https://github.com/kobezzza/Escaper/blob/master/LICENSE)

The MIT License.
