/*
 inspired from
 https://raw.githubusercontent.com/github/url-polyfill/master/url.js
 and https://stackoverflow.com/questions/6168260/how-to-parse-a-url
*/
(function (factory) {
    if (typeof module === "object" && typeof module.exports === "object") {
        var v = factory(require, exports);
        if (v !== undefined) module.exports = v;
    }
    else if (typeof define === "function" && define.amd) {
        define(["require", "exports"], factory);
    }
})(function (require, exports) {
    "use strict";
    Object.defineProperty(exports, "__esModule", { value: true });
    let createIterator;
    if (Symbol && Symbol.iterator && (typeof ([][Symbol.iterator]) === 'function')) {
        createIterator = (items) => {
            return items[Symbol.iterator]();
        };
    }
    else {
        createIterator = (items) => {
            return {
                next: function () {
                    const value = items.shift();
                    return { done: value === void 0, value: value };
                }
            };
        };
    }
    /**
     * Encodes a path segment.
     * RFC 3986 reserves !, ', (, ), and * and the implementation pipes the
     * output of encodeURIComponent to a hex encoding pass for these special
     * characters.
     */
    function encodePathSegment(segment) {
        return encodeURIComponent(segment).replace(/[!'()*]/g, function (c) {
            return '%' + c.charCodeAt(0).toString(16);
        });
    }
    class URLSearchParams {
        constructor(init) {
            this._entries = {};
            if (typeof init === 'string') {
                if (init !== '') {
                    init = init.replace(/^\?/, '');
                    const attributes = init.split('&');
                    let attribute;
                    for (let i = 0; i < attributes.length; i++) {
                        attribute = attributes[i].split('=');
                        this.append(decodeURIComponent(attribute[0]), (attribute.length > 1) ? decodeURIComponent(attribute[1]) : '');
                    }
                }
            }
            else if (init instanceof URLSearchParams) {
                init.forEach((value, name) => {
                    this.append(value, name);
                });
            }
        }
        append(name, value) {
            value = value.toString();
            if (name in this._entries) {
                this._entries[name].push(value);
            }
            else {
                this._entries[name] = [value];
            }
        }
        delete(name) {
            delete this._entries[name];
        }
        get(name) {
            return (name in this._entries) ? this._entries[name][0] : null;
        }
        getAll(name) {
            return (name in this._entries) ? this._entries[name].slice(0) : [];
        }
        has(name) {
            return (name in this._entries);
        }
        set(name, value) {
            this._entries[name] = [value.toString()];
        }
        forEach(callback) {
            let entries;
            for (let name in this._entries) {
                if (this._entries.hasOwnProperty(name)) {
                    entries = this._entries[name];
                    for (let i = 0; i < entries.length; i++) {
                        callback.call(this, entries[i], name, this);
                    }
                }
            }
        }
        keys() {
            const items = [];
            this.forEach((value, name) => { items.push(name); });
            return createIterator(items);
        }
        values() {
            const items = [];
            this.forEach((value) => { items.push(value); });
            return createIterator(items);
        }
        entries() {
            const items = [];
            this.forEach((value, name) => { items.push([value, name]); });
            return createIterator(items);
        }
        toString() {
            let searchString = '';
            this.forEach((value, name) => {
                if (searchString.length > 0)
                    searchString += '&';
                searchString += encodeURIComponent(name) + '=' + encodeURIComponent(value);
            });
            return searchString;
        }
    }
    exports.URLSearchParams = URLSearchParams;
    class URL {
        constructor(url, base) {
            let baseParts;
            try {
                baseParts = URL.parse(base);
            }
            catch (e) {
                throw new Error('Invalid base URL');
            }
            let urlParts = URL.parse(url);
            if (urlParts.protocol) {
                this._parts = { ...urlParts };
            }
            else {
                this._parts = {
                    protocol: baseParts.protocol,
                    username: baseParts.username,
                    password: baseParts.password,
                    hostname: baseParts.hostname,
                    port: baseParts.port,
                    path: urlParts.path || baseParts.path,
                    query: urlParts.query || baseParts.query,
                    hash: urlParts.hash,
                };
            }
            // console.log(URL.parse(base), URL.parse(url), this._parts);
        }
        static init() {
            this.URLRegExp = new RegExp('^' + this.patterns.protocol + '?' + this.patterns.authority + '?' + this.patterns.path + this.patterns.query + '?' + this.patterns.hash + '?');
            this.AuthorityRegExp = new RegExp('^' + this.patterns.authentication + '?' + this.patterns.hostname + this.patterns.port + '?$');
        }
        static parse(url) {
            const urlMatch = this.URLRegExp.exec(url);
            if (urlMatch !== null) {
                const authorityMatch = urlMatch[2] ? this.AuthorityRegExp.exec(urlMatch[2]) : [null, null, null, null, null];
                if (authorityMatch !== null) {
                    return {
                        protocol: urlMatch[1] || '',
                        username: authorityMatch[1] || '',
                        password: authorityMatch[2] || '',
                        hostname: authorityMatch[3] || '',
                        port: authorityMatch[4] || '',
                        path: urlMatch[3] || '',
                        query: urlMatch[4] || '',
                        hash: urlMatch[5] || '',
                    };
                }
            }
            throw new Error('Invalid URL');
        }
        get hash() {
            return this._parts.hash;
        }
        set hash(value) {
            value = value.toString();
            if (value.length === 0) {
                this._parts.hash = '';
            }
            else {
                if (value.charAt(0) !== '#')
                    value = '#' + value;
                this._parts.hash = encodeURIComponent(value);
            }
        }
        get host() {
            return this.hostname + (this.port ? (':' + this.port) : '');
        }
        set host(value) {
            value = value.toString();
            const url = new URL('http://' + value);
            this._parts.hostname = url.hostname;
            this._parts.port = url.port;
        }
        get hostname() {
            return this._parts.hostname;
        }
        set hostname(value) {
            value = value.toString();
            this._parts.hostname = encodeURIComponent(value);
        }
        get href() {
            const authentication = (this.username || this.password) ? (this.username + (this.password ? (':' + this.password) : '') + '@') : '';
            return this.protocol + '//' + authentication + this.host + this.pathname + this.search + this.hash;
        }
        set href(value) {
            value = value.toString();
            const url = new URL(value);
            this._parts = { ...url._parts };
        }
        get origin() {
            return this.protocol + '//' + this.host;
        }
        get password() {
            return this._parts.password;
        }
        set password(value) {
            value = value.toString();
            this._parts.password = encodeURIComponent(value);
        }
        get pathname() {
            return this._parts.path ? this._parts.path : '/';
        }
        set pathname(value) {
            let chunks = value.toString().split('/').map(encodePathSegment);
            if (chunks[0]) {
                // ensure joined string starts with slash.
                chunks.unshift('');
            }
            this._parts.path = chunks.join('/');
        }
        get port() {
            return this._parts.port;
        }
        set port(value) {
            let port = parseInt(value);
            if (isNaN(port)) {
                this._parts.port = '0';
            }
            else {
                this._parts.port = Math.max(0, port % (2 ** 16)).toString();
            }
        }
        get protocol() {
            return this._parts.protocol + ':';
        }
        set protocol(value) {
            value = value.toString();
            if (value.length !== 0) {
                if (value.charAt(value.length - 1) === ':') {
                    value = value.slice(0, -1);
                }
                this._parts.protocol = encodeURIComponent(value);
            }
        }
        get search() {
            return this._parts.query;
        }
        set search(value) {
            value = value.toString();
            if (value.charAt(0) !== '?')
                value = '?' + value;
            this._parts.query = value;
        }
        get username() {
            return this._parts.username;
        }
        set username(value) {
            value = value.toString();
            this._parts.username = encodeURIComponent(value);
        }
        get searchParams() {
            const searchParams = new URLSearchParams(this.search);
            ['append', 'delete', 'set'].forEach((methodName) => {
                const method = searchParams[methodName];
                searchParams[methodName] = (...args) => {
                    method.apply(searchParams, args);
                    this.search = searchParams.toString();
                };
            });
            return searchParams;
        }
        toString() {
            return this.href;
        }
    }
    // createObjectURL(object: any, options?: ObjectURLOptions): string;
    // revokeObjectURL(url: string): void;
    URL.patterns = {
        protocol: '(?:([^:/?#]+):)',
        authority: '(?://([^/?#]*))',
        path: '([^?#]*)',
        query: '(\\?[^#]*)',
        hash: '(#.*)',
        authentication: '(?:([^:]*)(?::([^@]*))?@)',
        hostname: '([^:]+)',
        port: '(?::(\\d+))',
    };
    exports.URL = URL;
    URL.init();
});
