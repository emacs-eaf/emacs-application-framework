"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var protocolClause = '(https?:\\/\\/)';
var domainCharacterSet = '[\\da-z\\.-]+';
var negatedDomainCharacterSet = '[^\\da-z\\.-]+';
var domainBodyClause = '(' + domainCharacterSet + ')';
var tldClause = '([a-z\\.]{2,6})';
var ipClause = '((\\d{1,3}\\.){3}\\d{1,3})';
var localHostClause = '(localhost)';
var portClause = '(:\\d{1,5})';
var hostClause = '((' + domainBodyClause + '\\.' + tldClause + ')|' + ipClause + '|' + localHostClause + ')' + portClause + '?';
var pathCharacterSet = '(\\/[\\/\\w\\.\\-%~:+]*)*([^:"\'\\s])';
var pathClause = '(' + pathCharacterSet + ')?';
var queryStringHashFragmentCharacterSet = '[0-9\\w\\[\\]\\(\\)\\/\\?\\!#@$%&\'*+,:;~\\=\\.\\-]*';
var queryStringClause = '(\\?' + queryStringHashFragmentCharacterSet + ')?';
var hashFragmentClause = '(#' + queryStringHashFragmentCharacterSet + ')?';
var negatedPathCharacterSet = '[^\\/\\w\\.\\-%]+';
var bodyClause = hostClause + pathClause + queryStringClause + hashFragmentClause;
var start = '(?:^|' + negatedDomainCharacterSet + ')(';
var end = ')($|' + negatedPathCharacterSet + ')';
var strictUrlRegex = new RegExp(start + protocolClause + bodyClause + end);
function handleLink(event, uri) {
    window.open(uri, '_blank');
}
var WebLinksAddon = (function () {
    function WebLinksAddon(_handler, _options) {
        if (_handler === void 0) { _handler = handleLink; }
        if (_options === void 0) { _options = {}; }
        this._handler = _handler;
        this._options = _options;
        this._options.matchIndex = 1;
    }
    WebLinksAddon.prototype.activate = function (terminal) {
        this._terminal = terminal;
        this._linkMatcherId = this._terminal.registerLinkMatcher(strictUrlRegex, this._handler, this._options);
    };
    WebLinksAddon.prototype.dispose = function () {
        if (this._linkMatcherId !== undefined && this._terminal !== undefined) {
            this._terminal.deregisterLinkMatcher(this._linkMatcherId);
        }
    };
    return WebLinksAddon;
}());
exports.WebLinksAddon = WebLinksAddon;
//# sourceMappingURL=WebLinksAddon.js.map