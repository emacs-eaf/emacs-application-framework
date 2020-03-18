/*!
 * Escaper v2.5.3
 * https://github.com/kobezzza/Escaper
 *
 * Released under the MIT license
 * https://github.com/kobezzza/Escaper/blob/master/LICENSE
 *
 * Date: Tue, 23 Jan 2018 15:58:45 GMT
 */

(function (global, factory) {
	typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
	typeof define === 'function' && define.amd ? define('Escaper', ['exports'], factory) :
	(factory((global.Escaper = {})));
}(this, (function (exports) { 'use strict';

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) {
  return typeof obj;
} : function (obj) {
  return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj;
};

var Escaper = void 0;
var escaper = Escaper = {
	VERSION: [2, 5, 3],
	content: [],
	cache: {},
	snakeskinRgxp: null,
	symbols: null,
	replace: replace,
	paste: paste
};

var stringLiterals = {
	'"': true,
	'\'': true,
	'`': true
};

var literals = {
	'/': true
};

for (var key in stringLiterals) {
	if (!stringLiterals.hasOwnProperty(key)) {
		break;
	}

	literals[key] = true;
}

var singleComments = {
	'//': true,
	'//*': true,
	'//!': true,
	'//#': true,
	'//@': true,
	'//$': true
};

var multComments = {
	'/*': true,
	'/**': true,
	'/*!': true,
	'/*#': true,
	'/*@': true,
	'/*$': true
};

var keyArr = [];
var finalMap = {};

for (var _key in literals) {
	if (!literals.hasOwnProperty(_key)) {
		break;
	}

	keyArr.push(_key);
	finalMap[_key] = true;
}

for (var _key2 in singleComments) {
	if (!singleComments.hasOwnProperty(_key2)) {
		break;
	}

	keyArr.push(_key2);
	finalMap[_key2] = true;
}

for (var _key3 in multComments) {
	if (!multComments.hasOwnProperty(_key3)) {
		break;
	}

	keyArr.push(_key3);
	finalMap[_key3] = true;
}

var rgxpFlags = [];
var rgxpFlagsMap = {
	'g': true,
	'm': true,
	'i': true,
	'y': true,
	'u': true
};

for (var _key4 in rgxpFlagsMap) {
	if (!rgxpFlagsMap.hasOwnProperty(_key4)) {
		break;
	}

	rgxpFlags.push(_key4);
}

var escapeEndMap = {
	'-': true,
	'+': true,
	'*': true,
	'%': true,
	'~': true,
	'>': true,
	'<': true,
	'^': true,
	',': true,
	';': true,
	'=': true,
	'|': true,
	'&': true,
	'!': true,
	'?': true,
	':': true,
	'(': true,
	'{': true,
	'[': true
};

var escapeEndWordMap = {
	'return': true,
	'yield': true,
	'await': true,
	'typeof': true,
	'void': true,
	'instanceof': true,
	'delete': true,
	'in': true,
	'new': true,
	'of': true
};

/**
 * @param {!Object} obj
 * @param {!Object} p
 * @param {(boolean|number)} val
 */
function mix(obj, p, val) {
	for (var _key5 in obj) {
		if (!obj.hasOwnProperty(_key5)) {
			break;
		}

		if (_key5 in p === false) {
			p[_key5] = val;
		}
	}
}

var symbols = void 0;
var snakeskinRgxp = void 0;

var uSRgxp = /[^\s/]/;
var wRgxp = /[a-z]/;
var sRgxp = /\s/;
var nRgxp = /[\r\n]/;
var posRgxp = /\${pos}/g;

var objMap = {
	'object': true,
	'function': true
};

/**
 * Replaces all found blocks ' ... ', " ... ", ` ... `, / ... /, // ..., /* ... *\/ to
 * __ESCAPER_QUOT__number_ in a string and returns a new string
 *
 * @param {string} str - source string
 * @param {(Object<string, boolean>|boolean)=} [opt_withCommentsOrParams=false] - parameters:
 *
 *     (if a parameter value is set to -1, then all found matches will be removed from the final string,
 *          or if the value will be set to true/false they will be included/excluded)
 *
 *     *) @label    - template for replacement, e.g. __ESCAPER_QUOT__${pos}_
 *     *) @all      - replaces all found matches
 *     *) @comments - replaces all kinds of comments
 *     *) @strings  - replaces all kinds of string literals
 *     *) @literals - replaces all kinds of string literals and regular expressions
 *     *) `
 *     *) '
 *     *) "
 *     *) /
 *     *) //
 *     *) //*
 *     *) //!
 *     *) //#
 *     *) //@
 *     *) //$
 *     *) /*
 *     *) /**
 *     *) /*!
 *     *) /*#
 *     *) /*@
 *     *) /*$
 *
 *     OR if the value is boolean, then will be replaced all found comments (true) / literals (false)
 *
 * @param {Array=} [opt_content=Escaper.content] - array for matches
 * @param {?boolean=} [opt_snakeskin] - private parameter for using with Snakeskin
 * @return {string}
 */
function replace(str, opt_withCommentsOrParams, opt_content, opt_snakeskin) {
	symbols = symbols || Escaper.symbols || 'a-z';
	snakeskinRgxp = snakeskinRgxp || Escaper.snakeskinRgxp || new RegExp('[!$' + symbols + '_]', 'i');

	var _Escaper = Escaper,
	    cache = _Escaper.cache,
	    content = _Escaper.content;


	var isObj = Boolean(opt_withCommentsOrParams && objMap[typeof opt_withCommentsOrParams === 'undefined' ? 'undefined' : _typeof(opt_withCommentsOrParams)]);

	var p = isObj ? Object(opt_withCommentsOrParams) : {};

	function mark(pos) {
		if (p['@label']) {
			return p['@label'].replace(posRgxp, pos);
		}

		return '__ESCAPER_QUOT__' + pos + '_';
	}

	var withComments = false;
	if (typeof opt_withCommentsOrParams === 'boolean') {
		withComments = Boolean(opt_withCommentsOrParams);
	}

	if ('@comments' in p) {
		mix(multComments, p, p['@comments']);
		mix(singleComments, p, p['@comments']);
		delete p['@comments'];
	}

	if ('@strings' in p) {
		mix(stringLiterals, p, p['@strings']);
		delete p['@strings'];
	}

	if ('@literals' in p) {
		mix(literals, p, p['@literals']);
		delete p['@literals'];
	}

	if ('@all' in p) {
		mix(finalMap, p, p['@all']);
		delete p['@all'];
	}

	var cacheKey = '';
	for (var i = -1; ++i < keyArr.length;) {
		var el = keyArr[i];

		if (multComments[el] || singleComments[el]) {
			p[el] = withComments || p[el];
		} else {
			p[el] = p[el] || !isObj;
		}

		cacheKey += p[el] + ',';
	}

	var initStr = str,
	    stack = opt_content || content;

	if (stack === content && cache[cacheKey] && cache[cacheKey][initStr]) {
		return cache[cacheKey][initStr];
	}

	var begin = false,
	    end = true;

	var escape = false,
	    comment = false;

	var selectionStart = 0,
	    block = false;

	var templateVar = 0,
	    filterStart = false;

	var cut = void 0,
	    label = void 0;

	var part = '',
	    rPart = '';

	for (var _i = -1; ++_i < str.length;) {
		var _el = str.charAt(_i);

		var next = str.charAt(_i + 1),
		    word = str.substr(_i, 2),
		    extWord = str.substr(_i, 3);

		if (!comment) {
			if (!begin) {
				if (_el === '/') {
					if (singleComments[word] || multComments[word]) {
						if (singleComments[extWord] || multComments[extWord]) {
							comment = extWord;
						} else {
							comment = word;
						}
					}

					if (comment) {
						selectionStart = _i;
						continue;
					}
				}

				if (escapeEndMap[_el] || escapeEndWordMap[rPart]) {
					end = true;
					rPart = '';
				} else if (uSRgxp.test(_el)) {
					end = false;
				}

				if (wRgxp.test(_el)) {
					part += _el;
				} else {
					rPart = part;
					part = '';
				}

				var skip = false;
				if (opt_snakeskin) {
					if (_el === '|' && snakeskinRgxp.test(next)) {
						filterStart = true;
						end = false;
						skip = true;
					} else if (filterStart && sRgxp.test(_el)) {
						filterStart = false;
						end = true;
						skip = true;
					}
				}

				if (!skip) {
					if (escapeEndMap[_el]) {
						end = true;
					} else if (uSRgxp.test(_el)) {
						end = false;
					}
				}
			}

			// [] inside RegExp
			if (begin === '/' && !escape) {
				if (_el === '[') {
					block = true;
				} else if (_el === ']') {
					block = false;
				}
			}

			if (!begin && templateVar) {
				if (_el === '}') {
					templateVar--;
				} else if (_el === '{') {
					templateVar++;
				}

				if (!templateVar) {
					_el = '`';
				}
			}

			if (begin === '`' && !escape && word === '${') {
				_el = '`';
				_i++;
				templateVar++;
			}

			if (finalMap[_el] && (_el !== '/' || end) && !begin) {
				begin = _el;
				selectionStart = _i;
			} else if (begin && (_el === '\\' || escape)) {
				escape = !escape;
			} else if (finalMap[_el] && begin === _el && !escape && (begin !== '/' || !block)) {
				if (_el === '/') {
					for (var j = -1; ++j < rgxpFlags.length;) {
						if (rgxpFlagsMap[str.charAt(_i + 1)]) {
							_i++;
						}
					}
				}

				begin = false;
				end = false;

				if (p[_el]) {
					cut = str.substring(selectionStart, _i + 1);

					if (p[_el] === -1) {
						label = '';
					} else {
						label = mark(stack.length);
						stack.push(cut);
					}

					str = str.substring(0, selectionStart) + label + str.substring(_i + 1);
					_i += label.length - cut.length;
				}
			}
		} else if (nRgxp.test(next) && singleComments[comment] || multComments[_el + str.charAt(_i - 1)] && _i - selectionStart > 2 && multComments[comment]) {
			if (p[comment]) {
				cut = str.substring(selectionStart, _i + 1);

				if (p[comment] === -1) {
					label = '';
				} else {
					label = mark(stack.length);
					stack.push(cut);
				}

				str = str.substring(0, selectionStart) + label + str.substring(_i + 1);
				_i += label.length - cut.length;
			}

			comment = false;
		}
	}

	if (stack === content) {
		cache[cacheKey] = cache[cacheKey] || {};
		cache[cacheKey][initStr] = str;
	}

	return str;
}

var pasteRgxp = /__ESCAPER_QUOT__(\d+)_/g;

/**
 * Replaces all found blocks __ESCAPER_QUOT__number_ to real content in a string
 * and returns a new string
 *
 * @param {string} str - source string
 * @param {Array=} [opt_content=Escaper.content] - array of matches
 * @param {RegExp=} [opt_rgxp] - RegExp for searching, e.g. /__ESCAPER_QUOT__(\d+)_/g
 * @return {string}
 */
function paste(str, opt_content, opt_rgxp) {
	return str.replace(opt_rgxp || pasteRgxp, function (str, pos) {
		return (opt_content || Escaper.content)[pos];
	});
}

exports['default'] = escaper;
exports.replace = replace;
exports.paste = paste;

Object.defineProperty(exports, '__esModule', { value: true });

})));
