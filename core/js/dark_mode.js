(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){

    var DarkReader = require("./node_modules/darkreader/darkreader.js");
    DarkReader.enable({
        brightness: 100,
        contrast: 90,
        sepia: 10,
    });
},{"./node_modules/darkreader/darkreader.js":2}],2:[function(require,module,exports){
/**
 * Dark Reader v4.7.15
 * https://darkreader.org/
 */

(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
    typeof define === 'function' && define.amd ? define(['exports'], factory) :
    (global = global || self, factory(global.DarkReader = {}));
}(this, function (exports) { 'use strict';

    /*! *****************************************************************************
    Copyright (c) Microsoft Corporation. All rights reserved.
    Licensed under the Apache License, Version 2.0 (the "License"); you may not use
    this file except in compliance with the License. You may obtain a copy of the
    License at http://www.apache.org/licenses/LICENSE-2.0

    THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
    KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
    WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
    MERCHANTABLITY OR NON-INFRINGEMENT.

    See the Apache Version 2.0 License for specific language governing permissions
    and limitations under the License.
    ***************************************************************************** */

    var __assign = function() {
        __assign = Object.assign || function __assign(t) {
            for (var s, i = 1, n = arguments.length; i < n; i++) {
                s = arguments[i];
                for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
            }
            return t;
        };
        return __assign.apply(this, arguments);
    };

    function __awaiter(thisArg, _arguments, P, generator) {
        return new (P || (P = Promise))(function (resolve, reject) {
            function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
            function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
            function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
            step((generator = generator.apply(thisArg, _arguments || [])).next());
        });
    }

    function __generator(thisArg, body) {
        var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
        return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
        function verb(n) { return function (v) { return step([n, v]); }; }
        function step(op) {
            if (f) throw new TypeError("Generator is already executing.");
            while (_) try {
                if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
                if (y = 0, t) op = [op[0] & 2, t.value];
                switch (op[0]) {
                    case 0: case 1: t = op; break;
                    case 4: _.label++; return { value: op[1], done: false };
                    case 5: _.label++; y = op[1]; op = [0]; continue;
                    case 7: op = _.ops.pop(); _.trys.pop(); continue;
                    default:
                        if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                        if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                        if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                        if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                        if (t[2]) _.ops.pop();
                        _.trys.pop(); continue;
                }
                op = body.call(thisArg, _);
            } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
            if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
        }
    }

    if (!window.chrome) {
        window.chrome = {};
    }
    if (!window.chrome.runtime) {
        window.chrome.runtime = {};
    }
    var throwCORSError = function () {
        throw new Error('Access to some of your resources was blocked by cross-origin policy');
    };
    if (window.chrome.runtime.sendMessage) {
        var nativeSendMessage_1 = window.chrome.runtime.sendMessage;
        window.chrome.runtime.sendMessage = function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            if (args[0] && args[0].type === 'fetch') {
                throwCORSError();
            }
            nativeSendMessage_1.apply(window.chrome.runtime, args);
        };
    }
    else {
        window.chrome.runtime.sendMessage = throwCORSError;
    }
    if (!window.chrome.runtime.onMessage) {
        window.chrome.runtime.onMessage = {
            addListener: Function.prototype,
        };
    }

    var ThemeEngines = {
        cssFilter: 'cssFilter',
        svgFilter: 'svgFilter',
        staticTheme: 'staticTheme',
        dynamicTheme: 'dynamicTheme',
    };

    function parseURL(url) {
        var a = document.createElement('a');
        a.href = url;
        return a;
    }
    function getAbsoluteURL($base, $relative) {
        if ($relative.match(/^.*?\/\//) || $relative.match(/^data\:/)) {
            if ($relative.startsWith('//')) {
                return "" + location.protocol + $relative;
            }
            return $relative;
        }
        var b = parseURL($base);
        if ($relative.startsWith('/')) {
            var u_1 = parseURL(b.protocol + "//" + b.host + $relative);
            return u_1.href;
        }
        var pathParts = b.pathname.split('/').concat($relative.split('/')).filter(function (p) { return p; });
        var backwardIndex;
        while ((backwardIndex = pathParts.indexOf('..')) > 0) {
            pathParts.splice(backwardIndex - 1, 2);
        }
        var u = parseURL(b.protocol + "//" + b.host + "/" + pathParts.join('/'));
        return u.href;
    }

    function logInfo() {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
    }
    function logWarn() {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
    }

    function iterateCSSRules(rules, iterate) {
        Array.from(rules)
            .forEach(function (rule) {
            if (rule instanceof CSSMediaRule) {
                var media = Array.from(rule.media);
                if (media.includes('screen') || media.includes('all') || !(media.includes('print') || media.includes('speech'))) {
                    iterateCSSRules(rule.cssRules, iterate);
                }
            }
            else if (rule instanceof CSSStyleRule) {
                iterate(rule);
            }
            else if (rule instanceof CSSImportRule) {
                try {
                    iterateCSSRules(rule.styleSheet.cssRules, iterate);
                }
                catch (err) {
                    logWarn(err);
                }
            }
            else {
                logWarn("CSSRule type not supported", rule);
            }
        });
    }
    function iterateCSSDeclarations(style, iterate) {
        Array.from(style).forEach(function (property) {
            var value = style.getPropertyValue(property).trim();
            if (!value) {
                return;
            }
            iterate(property, value);
        });
    }
    function isCSSVariable(property) {
        return property.startsWith('--') && !property.startsWith('--darkreader');
    }
    function getCSSVariables(rules) {
        var variables = new Map();
        rules && iterateCSSRules(rules, function (rule) {
            rule.style && iterateCSSDeclarations(rule.style, function (property, value) {
                if (isCSSVariable(property)) {
                    variables.set(property, value);
                }
            });
        });
        return variables;
    }
    function getElementCSSVariables(element) {
        var variables = new Map();
        iterateCSSDeclarations(element.style, function (property, value) {
            if (isCSSVariable(property)) {
                variables.set(property, value);
            }
        });
        return variables;
    }
    var cssURLRegex = /url\((('.+?')|(".+?")|([^\)]*?))\)/g;
    var cssImportRegex = /@import (url\()?(('.+?')|(".+?")|([^\)]*?))\)?;?/g;
    function getCSSURLValue(cssURL) {
        return cssURL.replace(/^url\((.*)\)$/, '$1').replace(/^"(.*)"$/, '$1').replace(/^'(.*)'$/, '$1');
    }
    function getCSSBaseBath(url) {
        var cssURL = parseURL(url);
        return cssURL.protocol + "//" + cssURL.host + cssURL.pathname.replace(/\?.*$/, '').replace(/(\/)([^\/]+)$/i, '$1');
    }
    function replaceCSSRelativeURLsWithAbsolute($css, cssBasePath) {
        return $css.replace(cssURLRegex, function (match) {
            var pathValue = getCSSURLValue(match);
            return "url(\"" + getAbsoluteURL(cssBasePath, pathValue) + "\")";
        });
    }
    var cssCommentsRegex = /\/\*[\s\S]*?\*\//g;
    function removeCSSComments($css) {
        return $css.replace(cssCommentsRegex, '');
    }
    var fontFaceRegex = /@font-face\s*{[^}]*}/g;
    function replaceCSSFontFace($css) {
        return $css.replace(fontFaceRegex, '');
    }
    var varRegex = /var\((--[^\s,]+),?\s*([^\(\)]*(\([^\(\)]*\)[^\(\)]*)*\s*)\)/g;
    function replaceCSSVariables(value, variables) {
        var missing = false;
        var result = value.replace(varRegex, function (match, name, fallback) {
            if (variables.has(name)) {
                return variables.get(name);
            }
            else if (fallback) {
                return fallback;
            }
            else {
                logWarn("Variable " + name + " not found");
                missing = true;
            }
            return match;
        });
        if (missing) {
            return result;
        }
        if (result.match(varRegex)) {
            return replaceCSSVariables(result, variables);
        }
        return result;
    }

    function hslToRGB(_a) {
        var h = _a.h, s = _a.s, l = _a.l, _b = _a.a, a = _b === void 0 ? 1 : _b;
        if (s === 0) {
            var _c = [l, l, l].map(function (x) { return Math.round(x * 255); }), r_1 = _c[0], b_1 = _c[1], g_1 = _c[2];
            return { r: r_1, g: g_1, b: b_1, a: a };
        }
        var c = (1 - Math.abs(2 * l - 1)) * s;
        var x = c * (1 - Math.abs((h / 60) % 2 - 1));
        var m = l - c / 2;
        var _d = (h < 60 ? [c, x, 0] :
            h < 120 ? [x, c, 0] :
                h < 180 ? [0, c, x] :
                    h < 240 ? [0, x, c] :
                        h < 300 ? [x, 0, c] :
                            [c, 0, x]).map(function (n) { return Math.round((n + m) * 255); }), r = _d[0], g = _d[1], b = _d[2];
        return { r: r, g: g, b: b, a: a };
    }
    function rgbToHSL(_a) {
        var r255 = _a.r, g255 = _a.g, b255 = _a.b, _b = _a.a, a = _b === void 0 ? 1 : _b;
        var r = r255 / 255;
        var g = g255 / 255;
        var b = b255 / 255;
        var max = Math.max(r, g, b);
        var min = Math.min(r, g, b);
        var c = max - min;
        var l = (max + min) / 2;
        if (c === 0) {
            return { h: 0, s: 0, l: l, a: a };
        }
        var h = (max === r ? (((g - b) / c) % 6) :
            max === g ? ((b - r) / c + 2) :
                ((r - g) / c + 4)) * 60;
        if (h < 0) {
            h += 360;
        }
        var s = c / (1 - Math.abs(2 * l - 1));
        return { h: h, s: s, l: l, a: a };
    }
    function toFixed(n, digits) {
        if (digits === void 0) { digits = 0; }
        var fixed = n.toFixed(digits);
        if (digits === 0) {
            return fixed;
        }
        var dot = fixed.indexOf('.');
        if (dot >= 0) {
            var zerosMatch = fixed.match(/0+$/);
            if (zerosMatch) {
                if (zerosMatch.index === dot + 1) {
                    return fixed.substring(0, dot);
                }
                return fixed.substring(0, zerosMatch.index);
            }
        }
        return fixed;
    }
    function rgbToString(rgb) {
        var r = rgb.r, g = rgb.g, b = rgb.b, a = rgb.a;
        if (a != null && a < 1) {
            return "rgba(" + toFixed(r) + ", " + toFixed(g) + ", " + toFixed(b) + ", " + toFixed(a, 2) + ")";
        }
        return "rgb(" + toFixed(r) + ", " + toFixed(g) + ", " + toFixed(b) + ")";
    }
    function rgbToHexString(_a) {
        var r = _a.r, g = _a.g, b = _a.b, a = _a.a;
        return "#" + (a != null && a < 1 ? [r, g, b, Math.round(a * 255)] : [r, g, b]).map(function (x) {
            return "" + (x < 16 ? '0' : '') + x.toString(16);
        }).join('');
    }
    var rgbMatch = /^rgba?\([^\(\)]+\)$/;
    var hslMatch = /^hsla?\([^\(\)]+\)$/;
    var hexMatch = /^#[0-9a-f]+$/i;
    function parse($color) {
        var c = $color.trim().toLowerCase();
        if (c.match(rgbMatch)) {
            return parseRGB(c);
        }
        if (c.match(hslMatch)) {
            return parseHSL(c);
        }
        if (c.match(hexMatch)) {
            return parseHex(c);
        }
        if (knownColors.has(c)) {
            return getColorByName(c);
        }
        if (systemColors.has(c)) {
            return getSystemColor(c);
        }
        if ($color === 'transparent') {
            return { r: 0, g: 0, b: 0, a: 0 };
        }
        throw new Error("Unable to parse " + $color);
    }
    function getNumbersFromString(str, splitter, range, units) {
        var raw = str.split(splitter).filter(function (x) { return x; });
        var unitsList = Object.entries(units);
        var numbers = raw.map(function (r) { return r.trim(); }).map(function (r, i) {
            var n;
            var unit = unitsList.find(function (_a) {
                var u = _a[0];
                return r.endsWith(u);
            });
            if (unit) {
                n = parseFloat(r.substring(0, r.length - unit[0].length)) / unit[1] * range[i];
            }
            else {
                n = parseFloat(r);
            }
            if (range[i] > 1) {
                return Math.round(n);
            }
            return n;
        });
        return numbers;
    }
    var rgbSplitter = /rgba?|\(|\)|\/|,|\s/ig;
    var rgbRange = [255, 255, 255, 1];
    var rgbUnits = { '%': 100 };
    function parseRGB($rgb) {
        var _a = getNumbersFromString($rgb, rgbSplitter, rgbRange, rgbUnits), r = _a[0], g = _a[1], b = _a[2], _b = _a[3], a = _b === void 0 ? 1 : _b;
        return { r: r, g: g, b: b, a: a };
    }
    var hslSplitter = /hsla?|\(|\)|\/|,|\s/ig;
    var hslRange = [360, 1, 1, 1];
    var hslUnits = { '%': 100, 'deg': 360, 'rad': 2 * Math.PI, 'turn': 1 };
    function parseHSL($hsl) {
        var _a = getNumbersFromString($hsl, hslSplitter, hslRange, hslUnits), h = _a[0], s = _a[1], l = _a[2], _b = _a[3], a = _b === void 0 ? 1 : _b;
        return hslToRGB({ h: h, s: s, l: l, a: a });
    }
    function parseHex($hex) {
        var h = $hex.substring(1);
        switch (h.length) {
            case 3:
            case 4: {
                var _a = [0, 1, 2].map(function (i) { return parseInt("" + h[i] + h[i], 16); }), r = _a[0], g = _a[1], b = _a[2];
                var a = h.length === 3 ? 1 : (parseInt("" + h[3] + h[3], 16) / 255);
                return { r: r, g: g, b: b, a: a };
            }
            case 6:
            case 8: {
                var _b = [0, 2, 4].map(function (i) { return parseInt(h.substring(i, i + 2), 16); }), r = _b[0], g = _b[1], b = _b[2];
                var a = h.length === 6 ? 1 : (parseInt(h.substring(6, 8), 16) / 255);
                return { r: r, g: g, b: b, a: a };
            }
        }
        throw new Error("Unable to parse " + $hex);
    }
    function getColorByName($color) {
        var n = knownColors.get($color);
        return {
            r: (n >> 16) & 255,
            g: (n >> 8) & 255,
            b: (n >> 0) & 255,
            a: 1
        };
    }
    function getSystemColor($color) {
        var n = systemColors.get($color);
        return {
            r: (n >> 16) & 255,
            g: (n >> 8) & 255,
            b: (n >> 0) & 255,
            a: 1
        };
    }
    var knownColors = new Map(Object.entries({
        aliceblue: 0xf0f8ff,
        antiquewhite: 0xfaebd7,
        aqua: 0x00ffff,
        aquamarine: 0x7fffd4,
        azure: 0xf0ffff,
        beige: 0xf5f5dc,
        bisque: 0xffe4c4,
        black: 0x000000,
        blanchedalmond: 0xffebcd,
        blue: 0x0000ff,
        blueviolet: 0x8a2be2,
        brown: 0xa52a2a,
        burlywood: 0xdeb887,
        cadetblue: 0x5f9ea0,
        chartreuse: 0x7fff00,
        chocolate: 0xd2691e,
        coral: 0xff7f50,
        cornflowerblue: 0x6495ed,
        cornsilk: 0xfff8dc,
        crimson: 0xdc143c,
        cyan: 0x00ffff,
        darkblue: 0x00008b,
        darkcyan: 0x008b8b,
        darkgoldenrod: 0xb8860b,
        darkgray: 0xa9a9a9,
        darkgrey: 0xa9a9a9,
        darkgreen: 0x006400,
        darkkhaki: 0xbdb76b,
        darkmagenta: 0x8b008b,
        darkolivegreen: 0x556b2f,
        darkorange: 0xff8c00,
        darkorchid: 0x9932cc,
        darkred: 0x8b0000,
        darksalmon: 0xe9967a,
        darkseagreen: 0x8fbc8f,
        darkslateblue: 0x483d8b,
        darkslategray: 0x2f4f4f,
        darkslategrey: 0x2f4f4f,
        darkturquoise: 0x00ced1,
        darkviolet: 0x9400d3,
        deeppink: 0xff1493,
        deepskyblue: 0x00bfff,
        dimgray: 0x696969,
        dimgrey: 0x696969,
        dodgerblue: 0x1e90ff,
        firebrick: 0xb22222,
        floralwhite: 0xfffaf0,
        forestgreen: 0x228b22,
        fuchsia: 0xff00ff,
        gainsboro: 0xdcdcdc,
        ghostwhite: 0xf8f8ff,
        gold: 0xffd700,
        goldenrod: 0xdaa520,
        gray: 0x808080,
        grey: 0x808080,
        green: 0x008000,
        greenyellow: 0xadff2f,
        honeydew: 0xf0fff0,
        hotpink: 0xff69b4,
        indianred: 0xcd5c5c,
        indigo: 0x4b0082,
        ivory: 0xfffff0,
        khaki: 0xf0e68c,
        lavender: 0xe6e6fa,
        lavenderblush: 0xfff0f5,
        lawngreen: 0x7cfc00,
        lemonchiffon: 0xfffacd,
        lightblue: 0xadd8e6,
        lightcoral: 0xf08080,
        lightcyan: 0xe0ffff,
        lightgoldenrodyellow: 0xfafad2,
        lightgray: 0xd3d3d3,
        lightgrey: 0xd3d3d3,
        lightgreen: 0x90ee90,
        lightpink: 0xffb6c1,
        lightsalmon: 0xffa07a,
        lightseagreen: 0x20b2aa,
        lightskyblue: 0x87cefa,
        lightslategray: 0x778899,
        lightslategrey: 0x778899,
        lightsteelblue: 0xb0c4de,
        lightyellow: 0xffffe0,
        lime: 0x00ff00,
        limegreen: 0x32cd32,
        linen: 0xfaf0e6,
        magenta: 0xff00ff,
        maroon: 0x800000,
        mediumaquamarine: 0x66cdaa,
        mediumblue: 0x0000cd,
        mediumorchid: 0xba55d3,
        mediumpurple: 0x9370db,
        mediumseagreen: 0x3cb371,
        mediumslateblue: 0x7b68ee,
        mediumspringgreen: 0x00fa9a,
        mediumturquoise: 0x48d1cc,
        mediumvioletred: 0xc71585,
        midnightblue: 0x191970,
        mintcream: 0xf5fffa,
        mistyrose: 0xffe4e1,
        moccasin: 0xffe4b5,
        navajowhite: 0xffdead,
        navy: 0x000080,
        oldlace: 0xfdf5e6,
        olive: 0x808000,
        olivedrab: 0x6b8e23,
        orange: 0xffa500,
        orangered: 0xff4500,
        orchid: 0xda70d6,
        palegoldenrod: 0xeee8aa,
        palegreen: 0x98fb98,
        paleturquoise: 0xafeeee,
        palevioletred: 0xdb7093,
        papayawhip: 0xffefd5,
        peachpuff: 0xffdab9,
        peru: 0xcd853f,
        pink: 0xffc0cb,
        plum: 0xdda0dd,
        powderblue: 0xb0e0e6,
        purple: 0x800080,
        rebeccapurple: 0x663399,
        red: 0xff0000,
        rosybrown: 0xbc8f8f,
        royalblue: 0x4169e1,
        saddlebrown: 0x8b4513,
        salmon: 0xfa8072,
        sandybrown: 0xf4a460,
        seagreen: 0x2e8b57,
        seashell: 0xfff5ee,
        sienna: 0xa0522d,
        silver: 0xc0c0c0,
        skyblue: 0x87ceeb,
        slateblue: 0x6a5acd,
        slategray: 0x708090,
        slategrey: 0x708090,
        snow: 0xfffafa,
        springgreen: 0x00ff7f,
        steelblue: 0x4682b4,
        tan: 0xd2b48c,
        teal: 0x008080,
        thistle: 0xd8bfd8,
        tomato: 0xff6347,
        turquoise: 0x40e0d0,
        violet: 0xee82ee,
        wheat: 0xf5deb3,
        white: 0xffffff,
        whitesmoke: 0xf5f5f5,
        yellow: 0xffff00,
        yellowgreen: 0x9acd32,
    }));
    var systemColors = new Map(Object.entries({
        ActiveBorder: 0x3b99fc,
        ActiveCaption: 0x000000,
        AppWorkspace: 0xaaaaaa,
        Background: 0x6363ce,
        ButtonFace: 0xffffff,
        ButtonHighlight: 0xe9e9e9,
        ButtonShadow: 0x9fa09f,
        ButtonText: 0x000000,
        CaptionText: 0x000000,
        GrayText: 0x7f7f7f,
        Highlight: 0xb2d7ff,
        HighlightText: 0x000000,
        InactiveBorder: 0xffffff,
        InactiveCaption: 0xffffff,
        InactiveCaptionText: 0x000000,
        InfoBackground: 0xfbfcc5,
        InfoText: 0x000000,
        Menu: 0xf6f6f6,
        MenuText: 0xffffff,
        Scrollbar: 0xaaaaaa,
        ThreeDDarkShadow: 0x000000,
        ThreeDFace: 0xc0c0c0,
        ThreeDHighlight: 0xffffff,
        ThreeDLightShadow: 0xffffff,
        ThreeDShadow: 0x000000,
        Window: 0xececec,
        WindowFrame: 0xaaaaaa,
        WindowText: 0x000000,
        '-webkit-focus-ring-color': 0xe59700
    }).map(function (_a) {
        var key = _a[0], value = _a[1];
        return [key.toLowerCase(), value];
    }));

    function scale(x, inLow, inHigh, outLow, outHigh) {
        return (x - inLow) * (outHigh - outLow) / (inHigh - inLow) + outLow;
    }
    function clamp(x, min, max) {
        return Math.min(max, Math.max(min, x));
    }
    function multiplyMatrices(m1, m2) {
        var result = [];
        for (var i = 0; i < m1.length; i++) {
            result[i] = [];
            for (var j = 0; j < m2[0].length; j++) {
                var sum = 0;
                for (var k = 0; k < m1[0].length; k++) {
                    sum += m1[i][k] * m2[k][j];
                }
                result[i][j] = sum;
            }
        }
        return result;
    }

    function isFirefox() {
        return navigator.userAgent.includes('Firefox');
    }
    function isMacOS() {
        return navigator.platform.toLowerCase().startsWith('mac');
    }
    function isDeepSelectorSupported() {
        try {
            document.querySelector('x /deep/ x');
            return true;
        }
        catch (err) {
            return false;
        }
    }

    function getMatches(regex, input, group) {
        if (group === void 0) { group = 0; }
        var matches = [];
        var m;
        while (m = regex.exec(input)) {
            matches.push(m[group]);
        }
        return matches;
    }

    function createFilterMatrix(config) {
        var m = Matrix.identity();
        if (config.sepia !== 0) {
            m = multiplyMatrices(m, Matrix.sepia(config.sepia / 100));
        }
        if (config.grayscale !== 0) {
            m = multiplyMatrices(m, Matrix.grayscale(config.grayscale / 100));
        }
        if (config.contrast !== 100) {
            m = multiplyMatrices(m, Matrix.contrast(config.contrast / 100));
        }
        if (config.brightness !== 100) {
            m = multiplyMatrices(m, Matrix.brightness(config.brightness / 100));
        }
        if (config.mode === 1) {
            m = multiplyMatrices(m, Matrix.invertNHue());
        }
        return m;
    }
    function applyColorMatrix(_a, matrix) {
        var r = _a[0], g = _a[1], b = _a[2];
        var rgb = [[r / 255], [g / 255], [b / 255], [1], [1]];
        var result = multiplyMatrices(matrix, rgb);
        return [0, 1, 2].map(function (i) { return clamp(Math.round(result[i][0] * 255), 0, 255); });
    }
    var Matrix = {
        identity: function () {
            return [
                [1, 0, 0, 0, 0],
                [0, 1, 0, 0, 0],
                [0, 0, 1, 0, 0],
                [0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1]
            ];
        },
        invertNHue: function () {
            return [
                [0.333, -0.667, -0.667, 0, 1],
                [-0.667, 0.333, -0.667, 0, 1],
                [-0.667, -0.667, 0.333, 0, 1],
                [0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1]
            ];
        },
        brightness: function (v) {
            return [
                [v, 0, 0, 0, 0],
                [0, v, 0, 0, 0],
                [0, 0, v, 0, 0],
                [0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1]
            ];
        },
        contrast: function (v) {
            var t = (1 - v) / 2;
            return [
                [v, 0, 0, 0, t],
                [0, v, 0, 0, t],
                [0, 0, v, 0, t],
                [0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1]
            ];
        },
        sepia: function (v) {
            return [
                [(0.393 + 0.607 * (1 - v)), (0.769 - 0.769 * (1 - v)), (0.189 - 0.189 * (1 - v)), 0, 0],
                [(0.349 - 0.349 * (1 - v)), (0.686 + 0.314 * (1 - v)), (0.168 - 0.168 * (1 - v)), 0, 0],
                [(0.272 - 0.272 * (1 - v)), (0.534 - 0.534 * (1 - v)), (0.131 + 0.869 * (1 - v)), 0, 0],
                [0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1]
            ];
        },
        grayscale: function (v) {
            return [
                [(0.2126 + 0.7874 * (1 - v)), (0.7152 - 0.7152 * (1 - v)), (0.0722 - 0.0722 * (1 - v)), 0, 0],
                [(0.2126 - 0.2126 * (1 - v)), (0.7152 + 0.2848 * (1 - v)), (0.0722 - 0.0722 * (1 - v)), 0, 0],
                [(0.2126 - 0.2126 * (1 - v)), (0.7152 - 0.7152 * (1 - v)), (0.0722 + 0.9278 * (1 - v)), 0, 0],
                [0, 0, 0, 1, 0],
                [0, 0, 0, 0, 1]
            ];
        },
    };

    var colorModificationCache = new Map();
    function clearColorModificationCache() {
        colorModificationCache.clear();
    }
    function modifyColorWithCache(rgb, filter, modifyHSL) {
        var fnCache;
        if (colorModificationCache.has(modifyHSL)) {
            fnCache = colorModificationCache.get(modifyHSL);
        }
        else {
            fnCache = new Map();
            colorModificationCache.set(modifyHSL, fnCache);
        }
        var id = Object.entries(rgb)
            .concat(Object.entries(filter).filter(function (_a) {
            var key = _a[0];
            return ['mode', 'brightness', 'contrast', 'grayscale', 'sepia'].indexOf(key) >= 0;
        }))
            .map(function (_a) {
            var key = _a[0], value = _a[1];
            return key + ":" + value;
        })
            .join(';');
        if (fnCache.has(id)) {
            return fnCache.get(id);
        }
        var hsl = rgbToHSL(rgb);
        var modified = modifyHSL(hsl);
        var _a = hslToRGB(modified), r = _a.r, g = _a.g, b = _a.b, a = _a.a;
        var matrix = createFilterMatrix(filter);
        var _b = applyColorMatrix([r, g, b], matrix), rf = _b[0], gf = _b[1], bf = _b[2];
        var color = (a === 1 ?
            rgbToHexString({ r: rf, g: gf, b: bf }) :
            rgbToString({ r: rf, g: gf, b: bf, a: a }));
        fnCache.set(id, color);
        return color;
    }
    function noopHSL(hsl) {
        return hsl;
    }
    function modifyColor(rgb, theme) {
        return modifyColorWithCache(rgb, theme, noopHSL);
    }
    function modifyLightModeHSL(_a) {
        var h = _a.h, s = _a.s, l = _a.l, a = _a.a;
        var lMin = 0;
        var lMid = 0.4;
        var lMax = 0.9;
        var sNeutralLim = 0.36;
        var lNeutralDark = 0.2;
        var lNeutralLight = 0.8;
        var sColored = 0.16;
        var hColoredL0 = 205;
        var hColoredL1 = 40;
        var lx = scale(l, 0, 1, lMin, lMax);
        var hx = h;
        var sx = s;
        var isNeutral = l < lNeutralDark || l > lNeutralLight || s < sNeutralLim;
        if (isNeutral) {
            sx = (l < lMid ?
                scale(l, 0, lMid, sColored, 0) :
                scale(l, lMid, 1, 0, sColored));
            hx = (l < lMid ? hColoredL0 : hColoredL1);
        }
        return { h: hx, s: sx, l: lx, a: a };
    }
    function modifyBgHSL(_a) {
        var h = _a.h, s = _a.s, l = _a.l, a = _a.a;
        var lMin = 0.1;
        var lMaxS0 = 0.25;
        var lMaxS1 = 0.4;
        var sNeutralLim = 0.12;
        var lNeutralLight = 0.8;
        var sColored = 0.05;
        var hColored = 205;
        var hBlue0 = 200;
        var hBlue1 = 280;
        var lMax = scale(s, 0, 1, lMaxS0, lMaxS1);
        var lx = (l < lMax ?
            l :
            l < 0.5 ?
                lMax :
                scale(l, 0.5, 1, lMax, lMin));
        var isNeutral = (l >= lNeutralLight && h > hBlue0 && h < hBlue1) || s < sNeutralLim;
        var hx = h;
        var sx = s;
        if (isNeutral) {
            sx = sColored;
            hx = hColored;
        }
        return { h: hx, s: sx, l: lx, a: a };
    }
    function modifyBackgroundColor(rgb, filter) {
        if (filter.mode === 0) {
            return modifyColorWithCache(rgb, filter, modifyLightModeHSL);
        }
        return modifyColorWithCache(rgb, __assign({}, filter, { mode: 0 }), modifyBgHSL);
    }
    function modifyFgHSL(_a) {
        var h = _a.h, s = _a.s, l = _a.l, a = _a.a;
        var lMax = 0.9;
        var lMinS0 = 0.7;
        var lMinS1 = 0.6;
        var sNeutralLim = 0.24;
        var lNeutralDark = 0.2;
        var sColored = 0.10;
        var hColored = 40;
        var hBlue0 = 205;
        var hBlue1 = 245;
        var hBlueMax = 220;
        var lBlueMin = 0.7;
        var isBlue = h > hBlue0 && h <= hBlue1;
        var lMin = scale(s, 0, 1, isBlue ? scale(h, hBlue0, hBlue1, lMinS0, lBlueMin) : lMinS0, lMinS1);
        var lx = (l < 0.5 ?
            scale(l, 0, 0.5, lMax, lMin) :
            l < lMin ?
                lMin :
                l);
        var hx = h;
        var sx = s;
        if (isBlue) {
            hx = scale(hx, hBlue0, hBlue1, hBlue0, hBlueMax);
        }
        var isNeutral = l < lNeutralDark || s < sNeutralLim;
        if (isNeutral) {
            sx = sColored;
            hx = hColored;
        }
        return { h: hx, s: sx, l: lx, a: a };
    }
    function modifyForegroundColor(rgb, filter) {
        if (filter.mode === 0) {
            return modifyColorWithCache(rgb, filter, modifyLightModeHSL);
        }
        return modifyColorWithCache(rgb, __assign({}, filter, { mode: 0 }), modifyFgHSL);
    }
    function modifyBorderHSL(_a) {
        var h = _a.h, s = _a.s, l = _a.l, a = _a.a;
        var lMinS0 = 0.2;
        var lMinS1 = 0.3;
        var lMaxS0 = 0.4;
        var lMaxS1 = 0.5;
        var lMin = scale(s, 0, 1, lMinS0, lMinS1);
        var lMax = scale(s, 0, 1, lMaxS0, lMaxS1);
        var lx = scale(l, 0, 1, lMax, lMin);
        return { h: h, s: s, l: lx, a: a };
    }
    function modifyBorderColor(rgb, filter) {
        if (filter.mode === 0) {
            return modifyColorWithCache(rgb, filter, modifyLightModeHSL);
        }
        return modifyColorWithCache(rgb, __assign({}, filter, { mode: 0 }), modifyBorderHSL);
    }
    function modifyShadowColor(rgb, filter) {
        return modifyBackgroundColor(rgb, filter);
    }
    function modifyGradientColor(rgb, filter) {
        return modifyBackgroundColor(rgb, filter);
    }

    function getURLHost(url) {
        return url.match(/^(.*?\/{2,3})?(.+?)(\/|$)/)[2];
    }

    function createTextStyle(config) {
        var lines = [];
        lines.push('* {');
        if (config.useFont && config.fontFamily) {
            lines.push("  font-family: " + config.fontFamily + " !important;");
        }
        if (config.textStroke > 0) {
            lines.push("  -webkit-text-stroke: " + config.textStroke + "px !important;");
            lines.push("  text-stroke: " + config.textStroke + "px !important;");
        }
        lines.push('}');
        return lines.join('\n');
    }

    var FilterMode;
    (function (FilterMode) {
        FilterMode[FilterMode["light"] = 0] = "light";
        FilterMode[FilterMode["dark"] = 1] = "dark";
    })(FilterMode || (FilterMode = {}));
    function getCSSFilterValue(config) {
        var filters = [];
        if (config.mode === FilterMode.dark) {
            filters.push('invert(100%) hue-rotate(180deg)');
        }
        if (config.brightness !== 100) {
            filters.push("brightness(" + config.brightness + "%)");
        }
        if (config.contrast !== 100) {
            filters.push("contrast(" + config.contrast + "%)");
        }
        if (config.grayscale !== 0) {
            filters.push("grayscale(" + config.grayscale + "%)");
        }
        if (config.sepia !== 0) {
            filters.push("sepia(" + config.sepia + "%)");
        }
        if (filters.length === 0) {
            return null;
        }
        return filters.join(' ');
    }

    function toSVGMatrix(matrix) {
        return matrix.slice(0, 4).map(function (m) { return m.map(function (m) { return m.toFixed(3); }).join(' '); }).join(' ');
    }
    function getSVGFilterMatrixValue(config) {
        return toSVGMatrix(createFilterMatrix(config));
    }

    var counter = 0;
    var resolvers = new Map();
    var rejectors = new Map();
    function bgFetch(request) {
        return new Promise(function (resolve, reject) {
            var id = ++counter;
            resolvers.set(id, resolve);
            rejectors.set(id, reject);
            chrome.runtime.sendMessage({ type: 'fetch', data: request, id: id });
        });
    }
    chrome.runtime.onMessage.addListener(function (_a) {
        var type = _a.type, data = _a.data, error = _a.error, id = _a.id;
        if (type === 'fetch-response') {
            var resolve = resolvers.get(id);
            var reject = rejectors.get(id);
            resolvers.delete(id);
            rejectors.delete(id);
            if (error) {
                reject && reject(error);
            }
            else {
                resolve && resolve(data);
            }
        }
    });

    function getOKResponse(url) {
        return __awaiter(this, void 0, void 0, function () {
            var response;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4, fetch(url, { cache: 'force-cache' })];
                    case 1:
                        response = _a.sent();
                        if (response.ok) {
                            return [2, response];
                        }
                        else {
                            throw new Error("Unable to load " + url + " " + response.status + " " + response.statusText);
                        }
                        return [2];
                }
            });
        });
    }
    function loadAsDataURL(url) {
        return __awaiter(this, void 0, void 0, function () {
            var response, blob, dataURL;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4, getOKResponse(url)];
                    case 1:
                        response = _a.sent();
                        return [4, response.blob()];
                    case 2:
                        blob = _a.sent();
                        return [4, (new Promise(function (resolve) {
                                var reader = new FileReader();
                                reader.onloadend = function () { return resolve(reader.result); };
                                reader.readAsDataURL(blob);
                            }))];
                    case 3:
                        dataURL = _a.sent();
                        return [2, dataURL];
                }
            });
        });
    }

    function getImageDetails(url) {
        return __awaiter(this, void 0, void 0, function () {
            var dataURL, image, info;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!url.startsWith('data:')) return [3, 1];
                        dataURL = url;
                        return [3, 3];
                    case 1: return [4, getImageDataURL(url)];
                    case 2:
                        dataURL = _a.sent();
                        _a.label = 3;
                    case 3: return [4, urlToImage(dataURL)];
                    case 4:
                        image = _a.sent();
                        info = analyzeImage(image);
                        return [2, __assign({ src: url, dataURL: dataURL, width: image.naturalWidth, height: image.naturalHeight }, info)];
                }
            });
        });
    }
    function getImageDataURL(url) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!(getURLHost(url) === location.host)) return [3, 2];
                        return [4, loadAsDataURL(url)];
                    case 1: return [2, _a.sent()];
                    case 2: return [4, bgFetch({ url: url, responseType: 'data-url' })];
                    case 3: return [2, _a.sent()];
                }
            });
        });
    }
    function urlToImage(url) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                return [2, new Promise(function (resolve, reject) {
                        var image = new Image();
                        image.onload = function () { return resolve(image); };
                        image.onerror = function () { return reject("Unable to load image " + url); };
                        image.src = url;
                    })];
            });
        });
    }
    function analyzeImage(image) {
        var MAX_ANALIZE_PIXELS_COUNT = 32 * 32;
        var naturalPixelsCount = image.naturalWidth * image.naturalHeight;
        var k = Math.min(1, Math.sqrt(MAX_ANALIZE_PIXELS_COUNT / naturalPixelsCount));
        var width = Math.max(1, Math.round(image.naturalWidth * k));
        var height = Math.max(1, Math.round(image.naturalHeight * k));
        var canvas = document.createElement('canvas');
        canvas.width = width;
        canvas.height = height;
        var context = canvas.getContext('2d');
        context.imageSmoothingEnabled = false;
        context.drawImage(image, 0, 0, width, height);
        var imageData = context.getImageData(0, 0, width, height);
        var d = imageData.data;
        var TRANSPARENT_ALPHA_THRESHOLD = 0.05;
        var DARK_LIGHTNESS_THRESHOLD = 0.4;
        var LIGHT_LIGHTNESS_THRESHOLD = 0.7;
        var transparentPixelsCount = 0;
        var darkPixelsCount = 0;
        var lightPixelsCount = 0;
        var i, x, y;
        var r, g, b, a;
        var l, min, max;
        for (y = 0; y < height; y++) {
            for (x = 0; x < width; x++) {
                i = 4 * (y * width + x);
                r = d[i + 0] / 255;
                g = d[i + 1] / 255;
                b = d[i + 2] / 255;
                a = d[i + 3] / 255;
                if (a < TRANSPARENT_ALPHA_THRESHOLD) {
                    transparentPixelsCount++;
                }
                else {
                    min = Math.min(r, g, b);
                    max = Math.max(r, g, b);
                    l = (max + min) / 2;
                    if (l < DARK_LIGHTNESS_THRESHOLD) {
                        darkPixelsCount++;
                    }
                    if (l > LIGHT_LIGHTNESS_THRESHOLD) {
                        lightPixelsCount++;
                    }
                }
            }
        }
        var totalPixelsCount = width * height;
        var opaquePixelsCount = totalPixelsCount - transparentPixelsCount;
        var DARK_IMAGE_THRESHOLD = 0.7;
        var LIGHT_IMAGE_THRESHOLD = 0.7;
        var TRANSPARENT_IMAGE_THRESHOLD = 0.1;
        var LARGE_IMAGE_PIXELS_COUNT = 800 * 600;
        return {
            isDark: ((darkPixelsCount / opaquePixelsCount) >= DARK_IMAGE_THRESHOLD),
            isLight: ((lightPixelsCount / opaquePixelsCount) >= LIGHT_IMAGE_THRESHOLD),
            isTransparent: ((transparentPixelsCount / totalPixelsCount) >= TRANSPARENT_IMAGE_THRESHOLD),
            isLarge: (naturalPixelsCount >= LARGE_IMAGE_PIXELS_COUNT),
        };
    }
    function getFilteredImageDataURL(_a, filter) {
        var dataURL = _a.dataURL, width = _a.width, height = _a.height;
        var matrix = getSVGFilterMatrixValue(filter);
        var svg = [
            "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"" + width + "\" height=\"" + height + "\">",
            '<defs>',
            '<filter id="darkreader-image-filter">',
            "<feColorMatrix type=\"matrix\" values=\"" + matrix + "\" />",
            '</filter>',
            '</defs>',
            "<image width=\"" + width + "\" height=\"" + height + "\" filter=\"url(#darkreader-image-filter)\" xlink:href=\"" + dataURL + "\" />",
            '</svg>',
        ].join('');
        var bytes = new Uint8Array(svg.length);
        for (var i = 0; i < svg.length; i++) {
            bytes[i] = svg.charCodeAt(i);
        }
        var blob = new Blob([bytes], { type: 'image/svg+xml' });
        var objectURL = URL.createObjectURL(blob);
        return objectURL;
    }

    function getModifiableCSSDeclaration(property, value, rule, isCancelled) {
        var important = Boolean(rule && rule.style && rule.style.getPropertyPriority(property));
        var sourceValue = value;
        if (property.startsWith('--')) {
            return null;
        }
        else if ((property.indexOf('color') >= 0 && property !== '-webkit-print-color-adjust') ||
            property === 'fill' ||
            property === 'stroke') {
            var modifier = getColorModifier(property, value);
            if (modifier) {
                return { property: property, value: modifier, important: important, sourceValue: sourceValue };
            }
        }
        else if (property === 'background-image') {
            var modifier = getBgImageModifier(property, value, rule, isCancelled);
            if (modifier) {
                return { property: property, value: modifier, important: important, sourceValue: sourceValue };
            }
        }
        else if (property.indexOf('shadow') >= 0) {
            var modifier = getShadowModifier(property, value);
            if (modifier) {
                return { property: property, value: modifier, important: important, sourceValue: sourceValue };
            }
        }
        return null;
    }
    function getModifiedUserAgentStyle(filter, isIFrame) {
        var lines = [];
        if (!isIFrame) {
            lines.push('html {');
            lines.push("    background-color: " + modifyBackgroundColor({ r: 255, g: 255, b: 255 }, filter) + " !important;");
            lines.push('}');
        }
        lines.push((isIFrame ? '' : 'html, body, ') + "input, textarea, select, button {");
        lines.push("    background-color: " + modifyBackgroundColor({ r: 255, g: 255, b: 255 }, filter) + ";");
        lines.push('}');
        lines.push('html, body, input, textarea, select, button {');
        lines.push("    border-color: " + modifyBorderColor({ r: 76, g: 76, b: 76 }, filter) + ";");
        lines.push("    color: " + modifyForegroundColor({ r: 0, g: 0, b: 0 }, filter) + ";");
        lines.push('}');
        lines.push('a {');
        lines.push("    color: " + modifyForegroundColor({ r: 0, g: 64, b: 255 }, filter) + ";");
        lines.push('}');
        lines.push('table {');
        lines.push("    border-color: " + modifyBorderColor({ r: 128, g: 128, b: 128 }, filter) + ";");
        lines.push('}');
        lines.push('::placeholder {');
        lines.push("    color: " + modifyForegroundColor({ r: 169, g: 169, b: 169 }, filter) + ";");
        lines.push('}');
        ['::selection', '::-moz-selection'].forEach(function (selection) {
            lines.push(selection + " {");
            lines.push("    background-color: " + modifyBackgroundColor({ r: 0, g: 96, b: 212 }, filter) + ";");
            lines.push("    color: " + modifyForegroundColor({ r: 255, g: 255, b: 255 }, filter) + ";");
            lines.push('}');
        });
        lines.push('input:-webkit-autofill,');
        lines.push('textarea:-webkit-autofill,');
        lines.push('select:-webkit-autofill {');
        lines.push("    background-color: " + modifyBackgroundColor({ r: 250, g: 255, b: 189 }, filter) + " !important;");
        lines.push("    color: " + modifyForegroundColor({ r: 0, g: 0, b: 0 }, filter) + " !important;");
        lines.push('}');
        if (!isMacOS()) {
            lines.push('::-webkit-scrollbar {');
            lines.push("    background-color: " + modifyBackgroundColor({ r: 241, g: 241, b: 241 }, filter) + ";");
            lines.push("    color: " + modifyForegroundColor({ r: 96, g: 96, b: 96 }, filter) + ";");
            lines.push('}');
            lines.push('::-webkit-scrollbar-thumb {');
            lines.push("    background-color: " + modifyBackgroundColor({ r: 193, g: 193, b: 193 }, filter) + ";");
            lines.push('}');
            lines.push('::-webkit-scrollbar-thumb:hover {');
            lines.push("    background-color: " + modifyBackgroundColor({ r: 166, g: 166, b: 166 }, filter) + ";");
            lines.push('}');
            lines.push('::-webkit-scrollbar-thumb:active {');
            lines.push("    background-color: " + modifyBackgroundColor({ r: 96, g: 96, b: 96 }, filter) + ";");
            lines.push('}');
            lines.push('::-webkit-scrollbar-corner {');
            lines.push("    background-color: " + modifyBackgroundColor({ r: 255, g: 255, b: 255 }, filter) + ";");
            lines.push('}');
            lines.push('* {');
            lines.push("    scrollbar-color: " + modifyBackgroundColor({ r: 193, g: 193, b: 193 }, filter) + " " + modifyBackgroundColor({ r: 241, g: 241, b: 241 }, filter) + ";");
            lines.push('}');
        }
        return lines.join('\n');
    }
    function getModifiedFallbackStyle(filter, _a) {
        var strict = _a.strict;
        var lines = [];
        lines.push("html, body, " + (strict ? 'body *' : 'body > *') + " {");
        lines.push("    background-color: " + modifyBackgroundColor({ r: 255, g: 255, b: 255 }, filter) + " !important;");
        lines.push("    border-color: " + modifyBorderColor({ r: 64, g: 64, b: 64 }, filter) + " !important;");
        lines.push("    color: " + modifyForegroundColor({ r: 0, g: 0, b: 0 }, filter) + " !important;");
        lines.push('}');
        return lines.join('\n');
    }
    var unparsableColors = new Set([
        'inherit',
        'transparent',
        'initial',
        'currentcolor',
        'none',
    ]);
    var colorParseCache = new Map();
    function parseColorWithCache($color) {
        $color = $color.trim();
        if (colorParseCache.has($color)) {
            return colorParseCache.get($color);
        }
        var color = parse($color);
        colorParseCache.set($color, color);
        return color;
    }
    function tryParseColor($color) {
        try {
            return parseColorWithCache($color);
        }
        catch (err) {
            return null;
        }
    }
    function getColorModifier(prop, value) {
        if (unparsableColors.has(value.toLowerCase())) {
            return value;
        }
        try {
            var rgb_1 = parseColorWithCache(value);
            if (prop.indexOf('background') >= 0) {
                return function (filter) { return modifyBackgroundColor(rgb_1, filter); };
            }
            if (prop.indexOf('border') >= 0 || prop.indexOf('outline') >= 0) {
                return function (filter) { return modifyBorderColor(rgb_1, filter); };
            }
            return function (filter) { return modifyForegroundColor(rgb_1, filter); };
        }
        catch (err) {
            logWarn('Color parse error', err);
            return null;
        }
    }
    var gradientRegex = /[\-a-z]+gradient\(([^\(\)]*(\(([^\(\)]*(\(.*?\)))*[^\(\)]*\))){0,15}[^\(\)]*\)/g;
    var imageDetailsCache = new Map();
    var awaitingForImageLoading = new Map();
    function getBgImageModifier(prop, value, rule, isCancelled) {
        var _this = this;
        try {
            var gradients = getMatches(gradientRegex, value);
            var urls = getMatches(cssURLRegex, value);
            if (urls.length === 0 && gradients.length === 0) {
                return value;
            }
            var getIndices = function (matches) {
                var index = 0;
                return matches.map(function (match) {
                    var valueIndex = value.indexOf(match, index);
                    index = valueIndex + match.length;
                    return { match: match, index: valueIndex };
                });
            };
            var matches_1 = getIndices(urls).map(function (i) { return (__assign({ type: 'url' }, i)); })
                .concat(getIndices(gradients).map(function (i) { return (__assign({ type: 'gradient' }, i)); }))
                .sort(function (a, b) { return a.index - b.index; });
            var getGradientModifier_1 = function (gradient) {
                var match = gradient.match(/^(.*-gradient)\((.*)\)$/);
                var type = match[1];
                var content = match[2];
                var partsRegex = /([^\(\),]+(\([^\(\)]*(\([^\(\)]*\)*[^\(\)]*)?\))?[^\(\),]*),?/g;
                var colorStopRegex = /^(from|color-stop|to)\(([^\(\)]*?,\s*)?(.*?)\)$/;
                var parts = getMatches(partsRegex, content, 1).map(function (part) {
                    part = part.trim();
                    var rgb = tryParseColor(part);
                    if (rgb) {
                        return function (filter) { return modifyGradientColor(rgb, filter); };
                    }
                    var space = part.lastIndexOf(' ');
                    rgb = tryParseColor(part.substring(0, space));
                    if (rgb) {
                        return function (filter) { return modifyGradientColor(rgb, filter) + " " + part.substring(space + 1); };
                    }
                    var colorStopMatch = part.match(colorStopRegex);
                    if (colorStopMatch) {
                        rgb = tryParseColor(colorStopMatch[3]);
                        if (rgb) {
                            return function (filter) { return colorStopMatch[1] + "(" + (colorStopMatch[2] ? colorStopMatch[2] + ", " : '') + modifyGradientColor(rgb, filter) + ")"; };
                        }
                    }
                    return function () { return part; };
                });
                return function (filter) {
                    return type + "(" + parts.map(function (modify) { return modify(filter); }).join(', ') + ")";
                };
            };
            var getURLModifier_1 = function (urlValue) {
                var url = getCSSURLValue(urlValue);
                if (rule.parentStyleSheet.href) {
                    var basePath = getCSSBaseBath(rule.parentStyleSheet.href);
                    url = getAbsoluteURL(basePath, url);
                }
                else if (rule.parentStyleSheet.ownerNode && rule.parentStyleSheet.ownerNode.baseURI) {
                    url = getAbsoluteURL(rule.parentStyleSheet.ownerNode.baseURI, url);
                }
                else {
                    url = getAbsoluteURL(location.origin, url);
                }
                var absoluteValue = "url(\"" + url + "\")";
                return function (filter) { return __awaiter(_this, void 0, void 0, function () {
                    var imageDetails, awaiters_1, err_1, bgImageValue;
                    return __generator(this, function (_a) {
                        switch (_a.label) {
                            case 0:
                                if (!imageDetailsCache.has(url)) return [3, 1];
                                imageDetails = imageDetailsCache.get(url);
                                return [3, 7];
                            case 1:
                                _a.trys.push([1, 6, , 7]);
                                if (!awaitingForImageLoading.has(url)) return [3, 3];
                                awaiters_1 = awaitingForImageLoading.get(url);
                                return [4, new Promise(function (resolve) { return awaiters_1.push(resolve); })];
                            case 2:
                                imageDetails = _a.sent();
                                if (!imageDetails) {
                                    return [2, null];
                                }
                                return [3, 5];
                            case 3:
                                awaitingForImageLoading.set(url, []);
                                return [4, getImageDetails(url)];
                            case 4:
                                imageDetails = _a.sent();
                                imageDetailsCache.set(url, imageDetails);
                                awaitingForImageLoading.get(url).forEach(function (resolve) { return resolve(imageDetails); });
                                awaitingForImageLoading.delete(url);
                                _a.label = 5;
                            case 5:
                                if (isCancelled()) {
                                    return [2, null];
                                }
                                return [3, 7];
                            case 6:
                                err_1 = _a.sent();
                                logWarn(err_1);
                                if (awaitingForImageLoading.has(url)) {
                                    awaitingForImageLoading.get(url).forEach(function (resolve) { return resolve(null); });
                                    awaitingForImageLoading.delete(url);
                                }
                                return [2, absoluteValue];
                            case 7:
                                bgImageValue = getBgImageValue_1(imageDetails, filter) || absoluteValue;
                                return [2, bgImageValue];
                        }
                    });
                }); };
            };
            var getBgImageValue_1 = function (imageDetails, filter) {
                var isDark = imageDetails.isDark, isLight = imageDetails.isLight, isTransparent = imageDetails.isTransparent, isLarge = imageDetails.isLarge, width = imageDetails.width;
                var result;
                if (isDark && isTransparent && filter.mode === 1 && !isLarge && width > 2) {
                    logInfo("Inverting dark image " + imageDetails.src);
                    var inverted = getFilteredImageDataURL(imageDetails, __assign({}, filter, { sepia: clamp(filter.sepia + 10, 0, 100) }));
                    result = "url(\"" + inverted + "\")";
                }
                else if (isLight && !isTransparent && filter.mode === 1) {
                    if (isLarge) {
                        result = 'none';
                    }
                    else {
                        logInfo("Dimming light image " + imageDetails.src);
                        var dimmed = getFilteredImageDataURL(imageDetails, filter);
                        result = "url(\"" + dimmed + "\")";
                    }
                }
                else if (filter.mode === 0 && isLight && !isLarge) {
                    logInfo("Applying filter to image " + imageDetails.src);
                    var filtered = getFilteredImageDataURL(imageDetails, __assign({}, filter, { brightness: clamp(filter.brightness - 10, 5, 200), sepia: clamp(filter.sepia + 10, 0, 100) }));
                    result = "url(\"" + filtered + "\")";
                }
                else {
                    result = null;
                }
                return result;
            };
            var modifiers_1 = [];
            var index_1 = 0;
            matches_1.forEach(function (_a, i) {
                var match = _a.match, type = _a.type, matchStart = _a.index;
                var prefixStart = index_1;
                var matchEnd = matchStart + match.length;
                index_1 = matchEnd;
                modifiers_1.push(function () { return value.substring(prefixStart, matchStart); });
                modifiers_1.push(type === 'url' ? getURLModifier_1(match) : getGradientModifier_1(match));
                if (i === matches_1.length - 1) {
                    modifiers_1.push(function () { return value.substring(matchEnd); });
                }
            });
            return function (filter) {
                var results = modifiers_1.map(function (modify) { return modify(filter); });
                if (results.some(function (r) { return r instanceof Promise; })) {
                    return Promise.all(results)
                        .then(function (asyncResults) {
                        return asyncResults.join('');
                    });
                }
                return results.join('');
            };
        }
        catch (err) {
            logWarn("Unable to parse gradient " + value, err);
            return null;
        }
    }
    function getShadowModifier(prop, value) {
        try {
            var index_2 = 0;
            var colorMatches_1 = getMatches(/(^|\s)([a-z]+\(.+?\)|#[0-9a-f]+|[a-z]+)(.*?(inset|outset)?($|,))/ig, value, 2);
            var modifiers_2 = colorMatches_1.map(function (match, i) {
                var prefixIndex = index_2;
                var matchIndex = value.indexOf(match, index_2);
                var matchEnd = matchIndex + match.length;
                index_2 = matchEnd;
                var rgb = tryParseColor(match);
                if (!rgb) {
                    return function () { return value.substring(prefixIndex, matchEnd); };
                }
                return function (filter) { return "" + value.substring(prefixIndex, matchIndex) + modifyShadowColor(rgb, filter) + (i === colorMatches_1.length - 1 ? value.substring(matchEnd) : ''); };
            });
            return function (filter) { return modifiers_2.map(function (modify) { return modify(filter); }).join(''); };
        }
        catch (err) {
            logWarn("Unable to parse shadow " + value, err);
            return null;
        }
    }
    function cleanModificationCache() {
        colorParseCache.clear();
        clearColorModificationCache();
        imageDetailsCache.clear();
        awaitingForImageLoading.clear();
    }

    var overrides = {
        'background-color': {
            customProp: '--darkreader-inline-bgcolor',
            cssProp: 'background-color',
            dataAttr: 'data-darkreader-inline-bgcolor',
            store: new WeakSet(),
        },
        'background-image': {
            customProp: '--darkreader-inline-bgimage',
            cssProp: 'background-image',
            dataAttr: 'data-darkreader-inline-bgimage',
            store: new WeakSet(),
        },
        'border-color': {
            customProp: '--darkreader-inline-border',
            cssProp: 'border-color',
            dataAttr: 'data-darkreader-inline-border',
            store: new WeakSet(),
        },
        'border-bottom-color': {
            customProp: '--darkreader-inline-border-bottom',
            cssProp: 'border-bottom-color',
            dataAttr: 'data-darkreader-inline-border-bottom',
            store: new WeakSet(),
        },
        'border-left-color': {
            customProp: '--darkreader-inline-border-left',
            cssProp: 'border-left-color',
            dataAttr: 'data-darkreader-inline-border-left',
            store: new WeakSet(),
        },
        'border-right-color': {
            customProp: '--darkreader-inline-border-right',
            cssProp: 'border-right-color',
            dataAttr: 'data-darkreader-inline-border-right',
            store: new WeakSet(),
        },
        'border-top-color': {
            customProp: '--darkreader-inline-border-top',
            cssProp: 'border-top-color',
            dataAttr: 'data-darkreader-inline-border-top',
            store: new WeakSet(),
        },
        'box-shadow': {
            customProp: '--darkreader-inline-boxshadow',
            cssProp: 'box-shadow',
            dataAttr: 'data-darkreader-inline-boxshadow',
            store: new WeakSet(),
        },
        'color': {
            customProp: '--darkreader-inline-color',
            cssProp: 'color',
            dataAttr: 'data-darkreader-inline-color',
            store: new WeakSet(),
        },
        'fill': {
            customProp: '--darkreader-inline-fill',
            cssProp: 'fill',
            dataAttr: 'data-darkreader-inline-fill',
            store: new WeakSet(),
        },
        'stroke': {
            customProp: '--darkreader-inline-stroke',
            cssProp: 'stroke',
            dataAttr: 'data-darkreader-inline-stroke',
            store: new WeakSet(),
        },
        'outline-color': {
            customProp: '--darkreader-inline-outline',
            cssProp: 'outline-color',
            dataAttr: 'data-darkreader-inline-outline',
            store: new WeakSet(),
        },
    };
    var overridesList = Object.values(overrides);
    var INLINE_STYLE_ATTRS = ['style', 'fill', 'stroke', 'bgcolor', 'color'];
    var INLINE_STYLE_SELECTOR = INLINE_STYLE_ATTRS.map(function (attr) { return "[" + attr + "]"; }).join(', ');
    function getInlineOverrideStyle() {
        return overridesList.map(function (_a) {
            var dataAttr = _a.dataAttr, customProp = _a.customProp, cssProp = _a.cssProp;
            return [
                "[" + dataAttr + ":not(.CaretBrowsing)] {",
                "  " + cssProp + ": var(" + customProp + ") !important;",
                '}',
            ].join('\n');
        }).join('\n');
    }
    var observer = null;
    function expand(nodes, selector) {
        var results = [];
        nodes.forEach(function (n) {
            if (n instanceof Element) {
                if (n.matches(selector)) {
                    results.push(n);
                }
                results.push.apply(results, Array.from(n.querySelectorAll(selector)));
            }
        });
        return results;
    }
    function watchForInlineStyles(elementStyleDidChange) {
        if (observer) {
            observer.disconnect();
        }
        observer = new MutationObserver(function (mutations) {
            mutations.forEach(function (m) {
                var createdInlineStyles = expand(Array.from(m.addedNodes), INLINE_STYLE_SELECTOR);
                if (createdInlineStyles.length > 0) {
                    createdInlineStyles.forEach(function (el) { return elementStyleDidChange(el); });
                }
                if (m.type === 'attributes') {
                    if (INLINE_STYLE_ATTRS.includes(m.attributeName)) {
                        elementStyleDidChange(m.target);
                    }
                    overridesList
                        .filter(function (_a) {
                        var store = _a.store, dataAttr = _a.dataAttr;
                        return store.has(m.target) && !m.target.hasAttribute(dataAttr);
                    })
                        .forEach(function (_a) {
                        var dataAttr = _a.dataAttr;
                        return m.target.setAttribute(dataAttr, '');
                    });
                }
            });
        });
        observer.observe(document, {
            childList: true,
            subtree: true,
            attributes: true,
            attributeFilter: INLINE_STYLE_ATTRS.concat(overridesList.map(function (_a) {
                var dataAttr = _a.dataAttr;
                return dataAttr;
            })),
        });
    }
    function stopWatchingForInlineStyles() {
        if (observer) {
            observer.disconnect();
            observer = null;
        }
    }
    var inlineStyleCache = new WeakMap();
    var filterProps = ['brightness', 'contrast', 'grayscale', 'sepia', 'mode'];
    function getInlineStyleCacheKey(el, theme) {
        return INLINE_STYLE_ATTRS
            .map(function (attr) { return attr + "=\"" + el.getAttribute(attr) + "\""; })
            .concat(filterProps.map(function (prop) { return prop + "=\"" + theme[prop] + "\""; }))
            .join(' ');
    }
    function overrideInlineStyle(element, theme) {
        var cacheKey = getInlineStyleCacheKey(element, theme);
        if (cacheKey === inlineStyleCache.get(element)) {
            return;
        }
        var unsetProps = new Set(Object.keys(overrides));
        function setCustomProp(targetCSSProp, modifierCSSProp, cssVal) {
            var _a = overrides[targetCSSProp], customProp = _a.customProp, dataAttr = _a.dataAttr;
            var mod = getModifiableCSSDeclaration(modifierCSSProp, cssVal, null, null);
            if (!mod) {
                return;
            }
            var value = mod.value;
            if (typeof value === 'function') {
                value = value(theme);
            }
            element.style.setProperty(customProp, value);
            if (!element.hasAttribute(dataAttr)) {
                element.setAttribute(dataAttr, '');
            }
            unsetProps.delete(targetCSSProp);
        }
        if (element.hasAttribute('bgcolor')) {
            var value = element.getAttribute('bgcolor');
            if (value.match(/^[0-9a-f]{3}$/i) || value.match(/^[0-9a-f]{6}$/i)) {
                value = "#" + value;
            }
            setCustomProp('background-color', 'background-color', value);
        }
        if (element.hasAttribute('color')) {
            var value = element.getAttribute('color');
            if (value.match(/^[0-9a-f]{3}$/i) || value.match(/^[0-9a-f]{6}$/i)) {
                value = "#" + value;
            }
            setCustomProp('color', 'color', value);
        }
        if (element.hasAttribute('fill') && element instanceof SVGElement) {
            var SMALL_SVG_LIMIT = 32;
            var value = element.getAttribute('fill');
            var isBg = false;
            if (!(element instanceof SVGTextElement)) {
                var _a = element.getBoundingClientRect(), width = _a.width, height = _a.height;
                isBg = (width > SMALL_SVG_LIMIT || height > SMALL_SVG_LIMIT);
            }
            setCustomProp('fill', isBg ? 'background-color' : 'color', value);
        }
        if (element.hasAttribute('stroke')) {
            var value = element.getAttribute('stroke');
            setCustomProp('stroke', element instanceof SVGLineElement || element instanceof SVGTextElement ? 'border-color' : 'color', value);
        }
        element.style && iterateCSSDeclarations(element.style, function (property, value) {
            if (property === 'background-image' && value.indexOf('url') >= 0) {
                return;
            }
            if (overrides.hasOwnProperty(property)) {
                setCustomProp(property, property, value);
            }
        });
        if (element.style && element instanceof SVGTextElement && element.style.fill) {
            setCustomProp('fill', 'color', element.style.getPropertyValue('fill'));
        }
        Array.from(unsetProps).forEach(function (cssProp) {
            var _a = overrides[cssProp], store = _a.store, dataAttr = _a.dataAttr;
            store.delete(element);
            element.removeAttribute(dataAttr);
        });
        inlineStyleCache.set(element, getInlineStyleCacheKey(element, theme));
    }

    var metaThemeColorName = 'theme-color';
    var metaThemeColorSelector = "meta[name=\"" + metaThemeColorName + "\"]";
    var srcMetaThemeColor = null;
    var observer$1 = null;
    function changeMetaThemeColor(meta, theme) {
        srcMetaThemeColor = srcMetaThemeColor || meta.content;
        try {
            var color = parse(srcMetaThemeColor);
            meta.content = modifyBackgroundColor(color, theme);
        }
        catch (err) {
            logWarn(err);
        }
    }
    function changeMetaThemeColorWhenAvailable(theme) {
        var meta = document.querySelector(metaThemeColorSelector);
        if (meta) {
            changeMetaThemeColor(meta, theme);
        }
        else {
            if (observer$1) {
                observer$1.disconnect();
            }
            observer$1 = new MutationObserver(function (mutations) {
                loop: for (var _i = 0, mutations_1 = mutations; _i < mutations_1.length; _i++) {
                    var m = mutations_1[_i];
                    for (var _a = 0, _b = Array.from(m.addedNodes); _a < _b.length; _a++) {
                        var node = _b[_a];
                        if (node instanceof HTMLMetaElement && node.name === metaThemeColorName) {
                            observer$1.disconnect();
                            observer$1 = null;
                            changeMetaThemeColor(node, theme);
                            break loop;
                        }
                    }
                }
            });
            observer$1.observe(document.head, { childList: true });
        }
    }
    function restoreMetaThemeColor() {
        if (observer$1) {
            observer$1.disconnect();
            observer$1 = null;
        }
        var meta = document.querySelector(metaThemeColorSelector);
        if (meta && srcMetaThemeColor) {
            meta.content = srcMetaThemeColor;
        }
    }

    function throttle(callback) {
        var pending = false;
        var frameId = null;
        var lastArgs;
        var throttled = (function () {
            var args = [];
            for (var _i = 0; _i < arguments.length; _i++) {
                args[_i] = arguments[_i];
            }
            lastArgs = args;
            if (frameId) {
                pending = true;
            }
            else {
                callback.apply(void 0, lastArgs);
                frameId = requestAnimationFrame(function () {
                    frameId = null;
                    if (pending) {
                        callback.apply(void 0, lastArgs);
                        pending = false;
                    }
                });
            }
        });
        var cancel = function () {
            cancelAnimationFrame(frameId);
            pending = false;
            frameId = null;
        };
        return Object.assign(throttled, { cancel: cancel });
    }
    function createAsyncTasksQueue() {
        var tasks = [];
        var frameId = null;
        function runTasks() {
            var task;
            while (task = tasks.shift()) {
                task();
            }
            frameId = null;
        }
        function add(task) {
            tasks.push(task);
            if (!frameId) {
                frameId = requestAnimationFrame(runTasks);
            }
        }
        function cancel() {
            tasks.splice(0);
            cancelAnimationFrame(frameId);
            frameId = null;
        }
        return { add: add, cancel: cancel };
    }

    function getDuration(time) {
        var duration = 0;
        if (time.seconds) {
            duration += time.seconds * 1000;
        }
        if (time.minutes) {
            duration += time.minutes * 60 * 1000;
        }
        if (time.hours) {
            duration += time.hours * 60 * 60 * 1000;
        }
        if (time.days) {
            duration += time.days * 24 * 60 * 60 * 1000;
        }
        return duration;
    }

    function removeNode(node) {
        node && node.parentNode && node.parentNode.removeChild(node);
    }
    function watchForNodePosition(node, onRestore) {
        var MAX_ATTEMPTS_COUNT = 10;
        var ATTEMPTS_INTERVAL = getDuration({ seconds: 10 });
        var prevSibling = node.previousSibling;
        var parent = node.parentElement;
        if (!parent) {
            logWarn('Unable to watch for node position: parent element not found', node, prevSibling);
            return { stop: function () { } };
        }
        var attempts = 0;
        var start = null;
        var restore = throttle(function () {
            attempts++;
            var now = Date.now();
            if (start == null) {
                start = now;
            }
            else if (attempts >= MAX_ATTEMPTS_COUNT) {
                if (now - start < ATTEMPTS_INTERVAL) {
                    logWarn('Node position watcher stopped: some script conflicts with Dark Reader and can cause high CPU usage', node, prevSibling);
                    stop();
                    return;
                }
                start = now;
                attempts = 1;
            }
            if ((prevSibling && prevSibling.parentElement !== parent)) {
                logWarn('Unable to restore node position: sibling was removed', node, prevSibling, parent);
                stop();
                return;
            }
            logWarn('Node was removed, restoring it\'s position', node, prevSibling, parent);
            parent.insertBefore(node, prevSibling ? prevSibling.nextSibling : parent.firstChild);
            onRestore && onRestore();
        });
        var observer = new MutationObserver(function () {
            if (!node.parentElement) {
                restore();
            }
        });
        var run = function () {
            observer.observe(parent, { childList: true });
        };
        var stop = function () {
            observer.disconnect();
        };
        run();
        return { run: run, stop: stop };
    }

    var STYLE_SELECTOR = isDeepSelectorSupported()
        ? 'html /deep/ link[rel*="stylesheet" i], html /deep/ style'
        : 'html link[rel*="stylesheet" i], html style';
    function shouldManageStyle(element) {
        return (((element instanceof HTMLStyleElement) ||
            (element instanceof HTMLLinkElement && element.rel && element.rel.toLowerCase().includes('stylesheet'))) &&
            !element.classList.contains('darkreader') &&
            element.media !== 'print');
    }
    var asyncQueue = createAsyncTasksQueue();
    function manageStyle(element, _a) {
        var update = _a.update, loadingStart = _a.loadingStart, loadingEnd = _a.loadingEnd;
        var prevStyles = [];
        var next = element;
        while ((next = next.nextElementSibling) && next.matches('.darkreader')) {
            prevStyles.push(next);
        }
        var corsCopy = prevStyles.find(function (el) { return el.matches('.darkreader--cors'); }) || null;
        var syncStyle = prevStyles.find(function (el) { return el.matches('.darkreader--sync'); }) || null;
        var corsCopyPositionWatcher = null;
        var syncStylePositionWatcher = null;
        var cancelAsyncOperations = false;
        function isCancelled() {
            return cancelAsyncOperations;
        }
        var observer = new MutationObserver(function () {
            update();
        });
        var observerOptions = { attributes: true, childList: true, characterData: true };
        function containsCSSImport() {
            return element instanceof HTMLStyleElement && element.textContent.trim().match(cssImportRegex);
        }
        function getRulesSync() {
            if (corsCopy) {
                return corsCopy.sheet.cssRules;
            }
            if (element.sheet == null) {
                return null;
            }
            if (element instanceof HTMLLinkElement) {
                try {
                    return element.sheet.cssRules;
                }
                catch (err) {
                    logWarn(err);
                    return null;
                }
            }
            if (containsCSSImport()) {
                return null;
            }
            return safeGetSheetRules();
        }
        var isLoadingRules = false;
        var wasLoadingError = false;
        function getRulesAsync() {
            return __awaiter(this, void 0, void 0, function () {
                var cssText, cssBasePath, err_1, fullCSSText, err_2;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            if (!(element instanceof HTMLLinkElement)) return [3, 6];
                            if (!(element.sheet == null)) return [3, 4];
                            _a.label = 1;
                        case 1:
                            _a.trys.push([1, 3, , 4]);
                            return [4, linkLoading(element)];
                        case 2:
                            _a.sent();
                            if (cancelAsyncOperations) {
                                return [2, null];
                            }
                            return [3, 4];
                        case 3:
                            err_1 = _a.sent();
                            logWarn(err_1);
                            wasLoadingError = true;
                            return [2, null];
                        case 4:
                            try {
                                if (element.sheet.cssRules != null) {
                                    return [2, element.sheet.cssRules];
                                }
                            }
                            catch (err) {
                                logWarn(err);
                            }
                            return [4, loadText(element.href)];
                        case 5:
                            cssText = _a.sent();
                            cssBasePath = getCSSBaseBath(element.href);
                            if (cancelAsyncOperations) {
                                return [2, null];
                            }
                            return [3, 7];
                        case 6:
                            if (containsCSSImport()) {
                                cssText = element.textContent.trim();
                                cssBasePath = getCSSBaseBath(location.href);
                            }
                            else {
                                return [2, null];
                            }
                            _a.label = 7;
                        case 7:
                            if (!cssText) return [3, 12];
                            _a.label = 8;
                        case 8:
                            _a.trys.push([8, 10, , 11]);
                            return [4, replaceCSSImports(cssText, cssBasePath)];
                        case 9:
                            fullCSSText = _a.sent();
                            corsCopy = createCORSCopy(element, fullCSSText);
                            if (corsCopy) {
                                corsCopyPositionWatcher = watchForNodePosition(corsCopy);
                            }
                            return [3, 11];
                        case 10:
                            err_2 = _a.sent();
                            logWarn(err_2);
                            return [3, 11];
                        case 11:
                            if (corsCopy) {
                                return [2, corsCopy.sheet.cssRules];
                            }
                            _a.label = 12;
                        case 12: return [2, null];
                    }
                });
            });
        }
        function details() {
            var rules = getRulesSync();
            if (!rules) {
                if (isLoadingRules || wasLoadingError) {
                    return null;
                }
                isLoadingRules = true;
                loadingStart();
                getRulesAsync().then(function (results) {
                    isLoadingRules = false;
                    loadingEnd();
                    if (results) {
                        update();
                    }
                }).catch(function (err) {
                    logWarn(err);
                    isLoadingRules = false;
                    loadingEnd();
                });
                return null;
            }
            var variables = getCSSVariables(rules);
            return { variables: variables };
        }
        function getFilterKey(filter) {
            return ['mode', 'brightness', 'contrast', 'grayscale', 'sepia'].map(function (p) { return p + ":" + filter[p]; }).join(';');
        }
        var renderId = 0;
        var rulesTextCache = new Map();
        var rulesModCache = new Map();
        var prevFilterKey = null;
        function render(filter, variables) {
            var rules = getRulesSync();
            if (!rules) {
                return;
            }
            cancelAsyncOperations = false;
            var rulesChanged = (rulesModCache.size === 0);
            var notFoundCacheKeys = new Set(rulesModCache.keys());
            var filterKey = getFilterKey(filter);
            var filterChanged = (filterKey !== prevFilterKey);
            var modRules = [];
            iterateCSSRules(rules, function (rule) {
                var cssText = rule.cssText;
                var textDiffersFromPrev = false;
                notFoundCacheKeys.delete(cssText);
                if (!rulesTextCache.has(cssText)) {
                    rulesTextCache.set(cssText, cssText);
                    textDiffersFromPrev = true;
                }
                var vars = null;
                var varsRule = null;
                if (variables.size > 0 || cssText.includes('var(')) {
                    var cssTextWithVariables = replaceCSSVariables(cssText, variables);
                    if (rulesTextCache.get(cssText) !== cssTextWithVariables) {
                        rulesTextCache.set(cssText, cssTextWithVariables);
                        textDiffersFromPrev = true;
                        vars = document.createElement('style');
                        vars.classList.add('darkreader');
                        vars.classList.add('darkreader--vars');
                        vars.media = 'screen';
                        vars.textContent = cssTextWithVariables;
                        element.parentNode.insertBefore(vars, element.nextSibling);
                        varsRule = vars.sheet.cssRules[0];
                    }
                }
                if (textDiffersFromPrev) {
                    rulesChanged = true;
                }
                else {
                    modRules.push(rulesModCache.get(cssText));
                    return;
                }
                var modDecs = [];
                var targetRule = varsRule || rule;
                targetRule && targetRule.style && iterateCSSDeclarations(targetRule.style, function (property, value) {
                    var mod = getModifiableCSSDeclaration(property, value, rule, isCancelled);
                    if (mod) {
                        modDecs.push(mod);
                    }
                });
                var modRule = null;
                if (modDecs.length > 0) {
                    modRule = { selector: rule.selectorText, declarations: modDecs };
                    if (rule.parentRule instanceof CSSMediaRule) {
                        modRule.media = rule.parentRule.media.mediaText;
                    }
                    modRules.push(modRule);
                }
                rulesModCache.set(cssText, modRule);
                removeNode(vars);
            });
            notFoundCacheKeys.forEach(function (key) {
                rulesTextCache.delete(key);
                rulesModCache.delete(key);
            });
            prevFilterKey = filterKey;
            if (!rulesChanged && !filterChanged) {
                return;
            }
            renderId++;
            function setRule(target, index, declarations) {
                var selector = declarations[0].selector;
                target.insertRule(selector + " {}", index);
                var style = target.cssRules.item(index).style;
                declarations.forEach(function (_a) {
                    var property = _a.property, value = _a.value, important = _a.important, sourceValue = _a.sourceValue;
                    style.setProperty(property, value == null ? sourceValue : value, important ? 'important' : '');
                });
            }
            var readyDeclarations = [];
            var asyncDeclarations = new Map();
            var asyncDeclarationCounter = 0;
            function buildStyleSheet() {
                var groups = [];
                readyDeclarations.forEach(function (decl, i) {
                    var mediaGroup;
                    var selectorGroup;
                    var prev = i === 0 ? null : readyDeclarations[i - 1];
                    var isSameMedia = prev && prev.media === decl.media;
                    var isSameMediaAndSelector = prev && isSameMedia && prev.selector === decl.selector;
                    if (isSameMedia) {
                        mediaGroup = groups[groups.length - 1];
                    }
                    else {
                        mediaGroup = [];
                        groups.push(mediaGroup);
                    }
                    if (isSameMediaAndSelector) {
                        selectorGroup = mediaGroup[mediaGroup.length - 1];
                    }
                    else {
                        selectorGroup = [];
                        mediaGroup.push(selectorGroup);
                    }
                    selectorGroup.push(decl);
                });
                if (!syncStyle) {
                    syncStyle = document.createElement('style');
                    syncStyle.classList.add('darkreader');
                    syncStyle.classList.add('darkreader--sync');
                    syncStyle.media = 'screen';
                }
                syncStylePositionWatcher && syncStylePositionWatcher.stop();
                element.parentNode.insertBefore(syncStyle, corsCopy ? corsCopy.nextSibling : element.nextSibling);
                var sheet = syncStyle.sheet;
                for (var i = sheet.cssRules.length - 1; i >= 0; i--) {
                    sheet.deleteRule(i);
                }
                groups.forEach(function (mediaGroup) {
                    var media = mediaGroup[0][0].media;
                    var target;
                    if (media) {
                        sheet.insertRule("@media " + media + " {}", sheet.cssRules.length);
                        target = sheet.cssRules[sheet.cssRules.length - 1];
                    }
                    else {
                        target = sheet;
                    }
                    mediaGroup.forEach(function (selectorGroup) {
                        var asyncItems = selectorGroup.filter(function (_a) {
                            var value = _a.value;
                            return value == null;
                        });
                        if (asyncItems.length > 0) {
                            asyncItems.forEach(function (_a) {
                                var asyncKey = _a.asyncKey;
                                return asyncDeclarations.set(asyncKey, { declarations: selectorGroup, target: target, index: target.cssRules.length });
                            });
                        }
                        setRule(target, target.cssRules.length, selectorGroup);
                    });
                });
                if (syncStylePositionWatcher) {
                    syncStylePositionWatcher.run();
                }
                else {
                    syncStylePositionWatcher = watchForNodePosition(syncStyle, buildStyleSheet);
                }
            }
            function rebuildAsyncRule(key) {
                var _a = asyncDeclarations.get(key), declarations = _a.declarations, target = _a.target, index = _a.index;
                target.deleteRule(index);
                setRule(target, index, declarations);
                asyncDeclarations.delete(key);
            }
            modRules.filter(function (r) { return r; }).forEach(function (_a) {
                var selector = _a.selector, declarations = _a.declarations, media = _a.media;
                declarations.forEach(function (_a) {
                    var property = _a.property, value = _a.value, important = _a.important, sourceValue = _a.sourceValue;
                    if (typeof value === 'function') {
                        var modified = value(filter);
                        if (modified instanceof Promise) {
                            var index_1 = readyDeclarations.length;
                            var asyncKey_1 = asyncDeclarationCounter++;
                            readyDeclarations.push({ media: media, selector: selector, property: property, value: null, important: important, asyncKey: asyncKey_1, sourceValue: sourceValue });
                            var promise = modified;
                            var currentRenderId_1 = renderId;
                            promise.then(function (asyncValue) {
                                if (!asyncValue || cancelAsyncOperations || currentRenderId_1 !== renderId) {
                                    return;
                                }
                                readyDeclarations[index_1].value = asyncValue;
                                asyncQueue.add(function () {
                                    if (cancelAsyncOperations || currentRenderId_1 !== renderId) {
                                        return;
                                    }
                                    rebuildAsyncRule(asyncKey_1);
                                });
                            });
                        }
                        else {
                            readyDeclarations.push({ media: media, selector: selector, property: property, value: modified, important: important, sourceValue: sourceValue });
                        }
                    }
                    else {
                        readyDeclarations.push({ media: media, selector: selector, property: property, value: value, important: important, sourceValue: sourceValue });
                    }
                });
            });
            buildStyleSheet();
        }
        var rulesCount = null;
        var rulesCheckFrameId = null;
        function safeGetSheetRules() {
            try {
                return element.sheet.cssRules;
            }
            catch (err) {
                logWarn(err);
                return null;
            }
        }
        function subscribeToSheetChanges() {
            if (element.sheet && safeGetSheetRules()) {
                rulesCount = element.sheet.cssRules.length;
            }
            unsubscribeFromSheetChanges();
            var checkForUpdate = function () {
                if (element.sheet && safeGetSheetRules() &&
                    element.sheet.cssRules.length !== rulesCount) {
                    rulesCount = element.sheet.cssRules.length;
                    update();
                }
                rulesCheckFrameId = requestAnimationFrame(checkForUpdate);
            };
            checkForUpdate();
        }
        function unsubscribeFromSheetChanges() {
            cancelAnimationFrame(rulesCheckFrameId);
        }
        function pause() {
            observer.disconnect();
            corsCopyPositionWatcher && corsCopyPositionWatcher.stop();
            syncStylePositionWatcher && syncStylePositionWatcher.stop();
            cancelAsyncOperations = true;
            unsubscribeFromSheetChanges();
        }
        function destroy() {
            pause();
            removeNode(corsCopy);
            removeNode(syncStyle);
        }
        function watch() {
            observer.observe(element, observerOptions);
            if (element instanceof HTMLStyleElement) {
                subscribeToSheetChanges();
            }
        }
        return {
            details: details,
            render: render,
            pause: pause,
            destroy: destroy,
            watch: watch,
        };
    }
    function linkLoading(link) {
        return new Promise(function (resolve, reject) {
            var cleanUp = function () {
                link.removeEventListener('load', onLoad);
                link.removeEventListener('error', onError);
            };
            var onLoad = function () {
                cleanUp();
                resolve();
            };
            var onError = function () {
                cleanUp();
                reject("Link loading failed " + link.href);
            };
            link.addEventListener('load', onLoad);
            link.addEventListener('error', onError);
        });
    }
    function getCSSImportURL(importDeclaration) {
        return getCSSURLValue(importDeclaration.substring(8).replace(/;$/, ''));
    }
    function loadText(url) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!url.startsWith('data:')) return [3, 3];
                        return [4, fetch(url)];
                    case 1: return [4, (_a.sent()).text()];
                    case 2: return [2, _a.sent()];
                    case 3: return [4, bgFetch({ url: url, responseType: 'text' })];
                    case 4: return [2, _a.sent()];
                }
            });
        });
    }
    function replaceCSSImports(cssText, basePath) {
        return __awaiter(this, void 0, void 0, function () {
            var importMatches, _i, importMatches_1, match, importURL, absoluteURL, importedCSS, err_3;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        cssText = removeCSSComments(cssText);
                        cssText = replaceCSSFontFace(cssText);
                        cssText = replaceCSSRelativeURLsWithAbsolute(cssText, basePath);
                        importMatches = getMatches(cssImportRegex, cssText);
                        _i = 0, importMatches_1 = importMatches;
                        _a.label = 1;
                    case 1:
                        if (!(_i < importMatches_1.length)) return [3, 8];
                        match = importMatches_1[_i];
                        importURL = getCSSImportURL(match);
                        absoluteURL = getAbsoluteURL(basePath, importURL);
                        importedCSS = void 0;
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 5, , 6]);
                        return [4, loadText(absoluteURL)];
                    case 3:
                        importedCSS = _a.sent();
                        return [4, replaceCSSImports(importedCSS, getCSSBaseBath(absoluteURL))];
                    case 4:
                        importedCSS = _a.sent();
                        return [3, 6];
                    case 5:
                        err_3 = _a.sent();
                        logWarn(err_3);
                        importedCSS = '';
                        return [3, 6];
                    case 6:
                        cssText = cssText.split(match).join(importedCSS);
                        _a.label = 7;
                    case 7:
                        _i++;
                        return [3, 1];
                    case 8:
                        cssText = cssText.trim();
                        return [2, cssText];
                }
            });
        });
    }
    function createCORSCopy(srcElement, cssText) {
        if (!cssText) {
            return null;
        }
        var cors = document.createElement('style');
        cors.classList.add('darkreader');
        cors.classList.add('darkreader--cors');
        cors.media = 'screen';
        cors.textContent = cssText;
        srcElement.parentNode.insertBefore(cors, srcElement.nextSibling);
        cors.sheet.disabled = true;
        return cors;
    }

    var observer$2 = null;
    function getAllManageableStyles(nodes) {
        var results = [];
        Array.from(nodes).forEach(function (node) {
            if (node instanceof Element) {
                if (shouldManageStyle(node)) {
                    results.push(node);
                }
                results.push.apply(results, Array.from(node.querySelectorAll(STYLE_SELECTOR)).filter(shouldManageStyle));
            }
        });
        return results;
    }
    function iterateShadowNodes(nodes, iterator) {
        Array.from(nodes).forEach(function (node) {
            if (node instanceof Element) {
                if (node.shadowRoot) {
                    iterator(node);
                }
                iterateShadowNodes(node.childNodes, iterator);
            }
        });
    }
    var shadowObservers = new Set();
    function watchForStyleChanges(update) {
        if (observer$2) {
            observer$2.disconnect();
            shadowObservers.forEach(function (o) { return o.disconnect(); });
            shadowObservers.clear();
        }
        function handleMutations(mutations) {
            var createdStyles = mutations.reduce(function (nodes, m) { return nodes.concat(getAllManageableStyles(m.addedNodes)); }, []);
            var removedStyles = mutations.reduce(function (nodes, m) { return nodes.concat(getAllManageableStyles(m.removedNodes)); }, []);
            var updatedStyles = mutations
                .filter(function (_a) {
                var target = _a.target, type = _a.type;
                return type === 'attributes' && shouldManageStyle(target);
            })
                .reduce(function (styles, _a) {
                var target = _a.target;
                styles.push(target);
                return styles;
            }, []);
            if (createdStyles.length + removedStyles.length + updatedStyles.length > 0) {
                update({
                    created: createdStyles,
                    updated: updatedStyles,
                    removed: removedStyles,
                });
            }
            var allAddedNodes = [];
            mutations.forEach(function (m) {
                m.addedNodes.forEach(function (n) {
                    allAddedNodes.push(n);
                });
            });
            iterateShadowNodes(allAddedNodes, subscribeForShadowRootChanges);
        }
        function subscribeForShadowRootChanges(node) {
            var shadowObserver = new MutationObserver(handleMutations);
            shadowObserver.observe(node.shadowRoot, mutationObserverOptions);
            shadowObservers.add(shadowObserver);
        }
        var mutationObserverOptions = { childList: true, subtree: true, attributes: true, attributeFilter: ['rel'] };
        observer$2 = new MutationObserver(handleMutations);
        observer$2.observe(document.documentElement, mutationObserverOptions);
        iterateShadowNodes(document.documentElement.children, subscribeForShadowRootChanges);
    }
    function stopWatchingForStyleChanges() {
        if (observer$2) {
            observer$2.disconnect();
            observer$2 = null;
            shadowObservers.forEach(function (o) { return o.disconnect(); });
            shadowObservers.clear();
        }
    }

    var styleManagers = new Map();
    var variables = new Map();
    var filter = null;
    var fixes = null;
    var isIFrame = null;
    function createOrUpdateStyle(className) {
        var style = (document.head || document).querySelector("." + className);
        if (!style) {
            style = document.createElement('style');
            style.classList.add('darkreader');
            style.classList.add(className);
            style.media = 'screen';
        }
        return style;
    }
    var stylePositionWatchers = new Map();
    function setupStylePositionWatcher(node, alias) {
        stylePositionWatchers.has(alias) && stylePositionWatchers.get(alias).stop();
        stylePositionWatchers.set(alias, watchForNodePosition(node));
    }
    function stopStylePositionWatchers() {
        Array.from(stylePositionWatchers.values()).forEach(function (watcher) { return watcher.stop(); });
        stylePositionWatchers.clear();
    }
    function createStaticStyleOverrides() {
        var fallbackStyle = createOrUpdateStyle('darkreader--fallback');
        document.head.insertBefore(fallbackStyle, document.head.firstChild);
        fallbackStyle.textContent = getModifiedFallbackStyle(filter, { strict: true });
        setupStylePositionWatcher(fallbackStyle, 'fallback');
        var userAgentStyle = createOrUpdateStyle('darkreader--user-agent');
        document.head.insertBefore(userAgentStyle, fallbackStyle.nextSibling);
        userAgentStyle.textContent = getModifiedUserAgentStyle(filter, isIFrame);
        setupStylePositionWatcher(userAgentStyle, 'user-agent');
        var textStyle = createOrUpdateStyle('darkreader--text');
        document.head.insertBefore(textStyle, fallbackStyle.nextSibling);
        if (filter.useFont || filter.textStroke > 0) {
            textStyle.textContent = createTextStyle(filter);
        }
        else {
            textStyle.textContent = '';
        }
        setupStylePositionWatcher(textStyle, 'text');
        var invertStyle = createOrUpdateStyle('darkreader--invert');
        document.head.insertBefore(invertStyle, textStyle.nextSibling);
        if (fixes && Array.isArray(fixes.invert) && fixes.invert.length > 0) {
            invertStyle.textContent = [
                fixes.invert.join(', ') + " {",
                "    filter: " + getCSSFilterValue(__assign({}, filter, { contrast: filter.mode === 0 ? filter.contrast : clamp(filter.contrast - 10, 0, 100) })) + " !important;",
                '}',
            ].join('\n');
        }
        else {
            invertStyle.textContent = '';
        }
        setupStylePositionWatcher(invertStyle, 'invert');
        var inlineStyle = createOrUpdateStyle('darkreader--inline');
        document.head.insertBefore(inlineStyle, invertStyle.nextSibling);
        inlineStyle.textContent = getInlineOverrideStyle();
        setupStylePositionWatcher(inlineStyle, 'inline');
        var overrideStyle = createOrUpdateStyle('darkreader--override');
        document.head.appendChild(overrideStyle);
        overrideStyle.textContent = fixes && fixes.css ? replaceCSSTemplates(fixes.css) : '';
        setupStylePositionWatcher(overrideStyle, 'override');
    }
    function replaceCSSTemplates($cssText) {
        return $cssText.replace(/\${(.+?)}/g, function (m0, $color) {
            try {
                var color = parseColorWithCache($color);
                return modifyColor(color, filter);
            }
            catch (err) {
                logWarn(err);
                return $color;
            }
        });
    }
    function cleanFallbackStyle() {
        var fallback = document.head.querySelector('.darkreader--fallback');
        if (fallback) {
            fallback.textContent = '';
        }
    }
    function createDynamicStyleOverrides() {
        cancelRendering();
        updateVariables(getElementCSSVariables(document.documentElement));
        var newManagers = Array.from(document.querySelectorAll(STYLE_SELECTOR))
            .filter(function (style) { return !styleManagers.has(style) && shouldManageStyle(style); })
            .map(function (style) { return createManager(style); });
        var newVariables = newManagers
            .map(function (manager) { return manager.details(); })
            .filter(function (details) { return details && details.variables.size > 0; })
            .map(function (_a) {
            var variables = _a.variables;
            return variables;
        });
        if (newVariables.length === 0) {
            styleManagers.forEach(function (manager) { return manager.render(filter, variables); });
            if (loadingStyles.size === 0) {
                cleanFallbackStyle();
            }
        }
        else {
            newVariables.forEach(function (variables) { return updateVariables(variables); });
            throttledRenderAllStyles(function () {
                if (loadingStyles.size === 0) {
                    cleanFallbackStyle();
                }
            });
        }
        newManagers.forEach(function (manager) { return manager.watch(); });
        var inlineStyleElements = Array.from(document.querySelectorAll(INLINE_STYLE_SELECTOR));
        inlineStyleElements.forEach(function (el) { return overrideInlineStyle(el, filter); });
    }
    var loadingStylesCounter = 0;
    var loadingStyles = new Set();
    function createManager(element) {
        if (styleManagers.has(element)) {
            return;
        }
        var loadingStyleId = ++loadingStylesCounter;
        function loadingStart() {
            if (!isPageLoaded() || !didDocumentShowUp) {
                loadingStyles.add(loadingStyleId);
                var fallbackStyle = document.querySelector('.darkreader--fallback');
                if (!fallbackStyle.textContent) {
                    fallbackStyle.textContent = getModifiedFallbackStyle(filter, { strict: false });
                }
            }
        }
        function loadingEnd() {
            loadingStyles.delete(loadingStyleId);
            if (loadingStyles.size === 0 && isPageLoaded()) {
                cleanFallbackStyle();
            }
        }
        function update() {
            var details = manager.details();
            if (!details) {
                return;
            }
            if (details.variables.size === 0) {
                manager.render(filter, variables);
            }
            else {
                updateVariables(details.variables);
                throttledRenderAllStyles();
            }
        }
        var manager = manageStyle(element, { update: update, loadingStart: loadingStart, loadingEnd: loadingEnd });
        styleManagers.set(element, manager);
        return manager;
    }
    function updateVariables(newVars) {
        if (newVars.size === 0) {
            return;
        }
        newVars.forEach(function (value, key) { return variables.set(key, value); });
        variables.forEach(function (value, key) { return variables.set(key, replaceCSSVariables(value, variables)); });
    }
    function removeManager(element) {
        var manager = styleManagers.get(element);
        if (manager) {
            manager.destroy();
            styleManagers.delete(element);
        }
    }
    var throttledRenderAllStyles = throttle(function (callback) {
        styleManagers.forEach(function (manager) { return manager.render(filter, variables); });
        callback && callback();
    });
    var cancelRendering = function () {
        throttledRenderAllStyles.cancel();
    };
    function isPageLoaded() {
        return document.readyState === 'complete' || document.readyState === 'interactive';
    }
    function onReadyStateChange() {
        if (!isPageLoaded()) {
            return;
        }
        document.removeEventListener('readystatechange', onReadyStateChange);
        if (loadingStyles.size === 0) {
            cleanFallbackStyle();
        }
    }
    var documentVisibilityListener = null;
    var didDocumentShowUp = !document.hidden;
    function watchForDocumentVisibility(callback) {
        var alreadyWatching = Boolean(documentVisibilityListener);
        documentVisibilityListener = function () {
            if (!document.hidden) {
                stopWatchingForDocumentVisibility();
                callback();
                didDocumentShowUp = true;
            }
        };
        if (!alreadyWatching) {
            document.addEventListener('visibilitychange', documentVisibilityListener);
        }
    }
    function stopWatchingForDocumentVisibility() {
        document.removeEventListener('visibilitychange', documentVisibilityListener);
        documentVisibilityListener = null;
    }
    function createThemeAndWatchForUpdates() {
        createStaticStyleOverrides();
        function runDynamicStyle() {
            createDynamicStyleOverrides();
            watchForUpdates();
        }
        if (document.hidden) {
            watchForDocumentVisibility(runDynamicStyle);
        }
        else {
            runDynamicStyle();
        }
        changeMetaThemeColorWhenAvailable(filter);
    }
    function watchForUpdates() {
        watchForStyleChanges(function (_a) {
            var created = _a.created, updated = _a.updated, removed = _a.removed;
            var createdStyles = new Set(created);
            var movedStyles = new Set(removed.filter(function (style) { return createdStyles.has(style); }));
            removed
                .filter(function (style) { return !movedStyles.has(style); })
                .forEach(function (style) { return removeManager(style); });
            var newManagers = Array.from(new Set(created.concat(updated)))
                .filter(function (style) { return !styleManagers.has(style); })
                .map(function (style) { return createManager(style); });
            var newVariables = newManagers
                .map(function (manager) { return manager.details(); })
                .filter(function (details) { return details && details.variables.size > 0; })
                .map(function (_a) {
                var variables = _a.variables;
                return variables;
            });
            if (newVariables.length === 0) {
                newManagers.forEach(function (manager) { return manager.render(filter, variables); });
            }
            else {
                newVariables.forEach(function (variables) { return updateVariables(variables); });
                throttledRenderAllStyles();
            }
            newManagers.forEach(function (manager) { return manager.watch(); });
        });
        watchForInlineStyles(function (element) {
            overrideInlineStyle(element, filter);
            if (element === document.documentElement) {
                var rootVariables = getElementCSSVariables(document.documentElement);
                if (rootVariables.size > 0) {
                    updateVariables(rootVariables);
                    throttledRenderAllStyles();
                }
            }
        });
        document.addEventListener('readystatechange', onReadyStateChange);
    }
    function stopWatchingForUpdates() {
        styleManagers.forEach(function (manager) { return manager.pause(); });
        stopStylePositionWatchers();
        stopWatchingForStyleChanges();
        stopWatchingForInlineStyles();
        document.removeEventListener('readystatechange', onReadyStateChange);
    }
    function createOrUpdateDynamicTheme(filterConfig, dynamicThemeFixes, iframe) {
        filter = filterConfig;
        fixes = dynamicThemeFixes;
        isIFrame = iframe;
        if (document.head) {
            createThemeAndWatchForUpdates();
        }
        else {
            if (!isFirefox()) {
                var fallbackStyle = createOrUpdateStyle('darkreader--fallback');
                document.documentElement.appendChild(fallbackStyle);
                fallbackStyle.textContent = getModifiedFallbackStyle(filter, { strict: true });
            }
            var headObserver_1 = new MutationObserver(function () {
                if (document.head) {
                    headObserver_1.disconnect();
                    createThemeAndWatchForUpdates();
                }
            });
            headObserver_1.observe(document, { childList: true, subtree: true });
        }
    }
    function removeDynamicTheme() {
        cleanDynamicThemeCache();
        removeNode(document.querySelector('.darkreader--fallback'));
        if (document.head) {
            restoreMetaThemeColor();
            removeNode(document.head.querySelector('.darkreader--user-agent'));
            removeNode(document.head.querySelector('.darkreader--text'));
            removeNode(document.head.querySelector('.darkreader--invert'));
            removeNode(document.head.querySelector('.darkreader--inline'));
            removeNode(document.head.querySelector('.darkreader--override'));
        }
        Array.from(styleManagers.keys()).forEach(function (el) { return removeManager(el); });
        Array.from(document.querySelectorAll('.darkreader')).forEach(removeNode);
    }
    function cleanDynamicThemeCache() {
        stopWatchingForDocumentVisibility();
        cancelRendering();
        stopWatchingForUpdates();
        cleanModificationCache();
    }

    var defaultTheme = {
        mode: 1,
        brightness: 100,
        contrast: 100,
        grayscale: 0,
        sepia: 0,
        useFont: false,
        fontFamily: '',
        textStroke: 0,
        engine: ThemeEngines.dynamicTheme,
        stylesheet: '',
    };
    function enable(themeOptions, fixes, isIFrame) {
        if (fixes === void 0) { fixes = null; }
        if (isIFrame === void 0) { isIFrame = false; }
        var theme = __assign({}, defaultTheme, themeOptions);
        if (theme.engine !== ThemeEngines.dynamicTheme) {
            throw new Error('Theme engine is not supported');
        }
        createOrUpdateDynamicTheme(theme, fixes, isIFrame);
    }
    function disable() {
        removeDynamicTheme();
    }

    exports.disable = disable;
    exports.enable = enable;

    Object.defineProperty(exports, '__esModule', { value: true });

}));

},{}]},{},[1]);
