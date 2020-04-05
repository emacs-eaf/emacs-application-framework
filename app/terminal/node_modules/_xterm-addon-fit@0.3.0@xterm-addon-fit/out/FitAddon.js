"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var MINIMUM_COLS = 2;
var MINIMUM_ROWS = 1;
var FitAddon = (function () {
    function FitAddon() {
    }
    FitAddon.prototype.activate = function (terminal) {
        this._terminal = terminal;
    };
    FitAddon.prototype.dispose = function () { };
    FitAddon.prototype.fit = function () {
        var dims = this.proposeDimensions();
        if (!dims || !this._terminal) {
            return;
        }
        var core = this._terminal._core;
        if (this._terminal.rows !== dims.rows || this._terminal.cols !== dims.cols) {
            core._renderService.clear();
            this._terminal.resize(dims.cols, dims.rows);
        }
    };
    FitAddon.prototype.proposeDimensions = function () {
        if (!this._terminal) {
            return undefined;
        }
        if (!this._terminal.element || !this._terminal.element.parentElement) {
            return undefined;
        }
        var core = this._terminal._core;
        var parentElementStyle = window.getComputedStyle(this._terminal.element.parentElement);
        var parentElementHeight = parseInt(parentElementStyle.getPropertyValue('height'));
        var parentElementWidth = Math.max(0, parseInt(parentElementStyle.getPropertyValue('width')));
        var elementStyle = window.getComputedStyle(this._terminal.element);
        var elementPadding = {
            top: parseInt(elementStyle.getPropertyValue('padding-top')),
            bottom: parseInt(elementStyle.getPropertyValue('padding-bottom')),
            right: parseInt(elementStyle.getPropertyValue('padding-right')),
            left: parseInt(elementStyle.getPropertyValue('padding-left'))
        };
        var elementPaddingVer = elementPadding.top + elementPadding.bottom;
        var elementPaddingHor = elementPadding.right + elementPadding.left;
        var availableHeight = parentElementHeight - elementPaddingVer;
        var availableWidth = parentElementWidth - elementPaddingHor - core.viewport.scrollBarWidth;
        var geometry = {
            cols: Math.max(MINIMUM_COLS, Math.floor(availableWidth / core._renderService.dimensions.actualCellWidth)),
            rows: Math.max(MINIMUM_ROWS, Math.floor(availableHeight / core._renderService.dimensions.actualCellHeight))
        };
        return geometry;
    };
    return FitAddon;
}());
exports.FitAddon = FitAddon;
//# sourceMappingURL=FitAddon.js.map