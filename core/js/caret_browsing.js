
/**
 * This file is derived from qutebrowser caret.js
 * https://github.com/qutebrowser/qutebrowser/blob/b44e3ba657e622cf813d0c072d6fe538fca9bf2a/qutebrowser/javascript/caret.js
 * 
 * Copyright 2018-2020 Florian Bruhin (The Compiler) <mail@qutebrowser.org> 
 * 
 * Ported chrome-caretbrowsing extension.
 * https://cs.chromium.org/chromium/src/ui/accessibility/extensions/caretbrowsing/
 * 
 */

// Copyright 2014 The Chromium Authors. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//    * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//    * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


"use strict";

const axs = {};

axs.dom = {};

axs.color = {};

axs.utils = {};

axs.dom.parentElement = function(node) {
    if (!node) {
        return null;
    }
    const composedNode = axs.dom.composedParentNode(node);
    if (!composedNode) {
        return null;
    }
    switch (composedNode.nodeType) {
    case Node.ELEMENT_NODE:
        return composedNode;
    default:
        return axs.dom.parentElement(composedNode);
    }
};

axs.dom.shadowHost = function(node) {
    if ("host" in node) {
        return node.host;
    }
    return null;
};

axs.dom.composedParentNode = function(node) {
    if (!node) {
        return null;
    }
    if (node.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
        return axs.dom.shadowHost(node);
    }
    const parentNode = node.parentNode;
    if (!parentNode) {
        return null;
    }
    if (parentNode.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
        return axs.dom.shadowHost(parentNode);
    }
    if (!parentNode.shadowRoot) {
        return parentNode;
    }
    const points = node.getDestinationInsertionPoints();
    if (points.length > 0) {
        return axs.dom.composedParentNode(points[points.length - 1]);
    }
    return null;
};

axs.color.Color = function(red, green, blue, alpha) { // eslint-disable-line max-params,max-len
    this.red = red;
    this.green = green;
    this.blue = blue;
    this.alpha = alpha;
};

axs.color.parseColor = function(colorText) {
    if (colorText === "transparent") {
        return new axs.color.Color(0, 0, 0, 0);
    }
    let match = colorText.match(/^rgb\((\d+), (\d+), (\d+)\)$/);
    if (match) {
        const blue = parseInt(match[3], 10);
        const green = parseInt(match[2], 10);
        const red = parseInt(match[1], 10);
        return new axs.color.Color(red, green, blue, 1);
    }
    match = colorText.match(/^rgba\((\d+), (\d+), (\d+), (\d*(\.\d+)?)\)/);
    if (match) {
        const red = parseInt(match[1], 10);
        const green = parseInt(match[2], 10);
        const blue = parseInt(match[3], 10);
        const alpha = parseFloat(match[4]);
        return new axs.color.Color(red, green, blue, alpha);
    }
    return null;
};

axs.color.flattenColors = function(color1, color2) {
    const colorAlpha = color1.alpha;
    return new axs.color.Color(
        ((1 - colorAlpha) * color2.red) + (colorAlpha * color1.red),
        ((1 - colorAlpha) * color2.green) + (colorAlpha * color1.green),
        ((1 - colorAlpha) * color2.blue) + (colorAlpha * color2.blue),
        color1.alpha + (color2.alpha * (1 - color1.alpha)));
};

axs.utils.getParentBgColor = function(_el) {
    let el = _el;
    let el2 = el;
    let iter = null;
    el = [];
    for (iter = null; (el2 = axs.dom.parentElement(el2));) {
        const style = window.getComputedStyle(el2, null);
        if (style) {
            const color = axs.color.parseColor(style.backgroundColor);
            if (color &&
                (style.opacity < 1 &&
                (color.alpha *= style.opacity),
                color.alpha !== 0 &&
                (el.push(color), color.alpha === 1))) {
                iter = !0;
                break;
            }
        }
    }
    if (!iter) {
        el.push(new axs.color.Color(255, 255, 255, 1));
    }
    for (el2 = el.pop(); el.length;) {
        iter = el.pop();
        el2 = axs.color.flattenColors(iter, el2);
    }
    return el2;
};

axs.utils.getFgColor = function(el, el2, color) {
    let color2 = axs.color.parseColor(el.color);
    if (!color2) {
        return null;
    }
    if (color2.alpha < 1) {
        color2 = axs.color.flattenColors(color2, color);
    }
    if (el.opacity < 1) {
        const el3 = axs.utils.getParentBgColor(el2);
        color2.alpha *= el.opacity;
        color2 = axs.color.flattenColors(color2, el3);
    }
    return color2;
};

axs.utils.getBgColor = function(el, elParent) {
    let color = axs.color.parseColor(el.backgroundColor);
    if (!color) {
        return null;
    }
    if (el.opacity < 1) {
        color.alpha *= el.opacity;
    }
    if (color.alpha < 1) {
        const bgColor = axs.utils.getParentBgColor(elParent);
        if (bgColor === null) {
            return null;
        }
        color = axs.color.flattenColors(color, bgColor);
    }
    return color;
};

axs.color.colorChannelToString = function(_color) {
    const color = Math.round(_color);
    if (color < 15) {
        return `0${color.toString(16)}`;
    }
    return color.toString(16);
};

axs.color.colorToString = function(color) {
    if (color.alpha === 1) {
        const red = axs.color.colorChannelToString(color.red);
        const green = axs.color.colorChannelToString(color.green);
        const blue = axs.color.colorChannelToString(color.blue);
        return `#${red}${green}${blue}`;
    }
    const arr = [color.red, color.green, color.blue, color.alpha].join();
    return `rgba(${arr})`;
};

const Cursor = function(node, index, text) { // eslint-disable-line func-style,max-len
    this.node = node;
    this.index = index;
    this.text = text;
};

Cursor.prototype.clone = function() {
    return new Cursor(this.node, this.index, this.text);
};

Cursor.prototype.copyFrom = function(otherCursor) {
    this.node = otherCursor.node;
    this.index = otherCursor.index;
    this.text = otherCursor.text;
};

const TraverseUtil = {};

TraverseUtil.getNodeText = function(node) {
    if (node.constructor === Text) {
        return node.data;
    }
    return "";
};

TraverseUtil.treatAsLeafNode = function(node) {
    return node.childNodes.length === 0 ||
        node.nodeName === "SELECT" ||
        node.nodeName === "OBJECT";
};

TraverseUtil.isWhitespace = function(ch) {
    return (ch === " " || ch === "\n" || ch === "\r" || ch === "\t");
};

TraverseUtil.isVisible = function(node) {
    if (!node.style) {
        return true;
    }
    const style = window.getComputedStyle(node, null);
    return (Boolean(style) &&
            style.display !== "none" &&
            style.visibility !== "hidden");
};

TraverseUtil.isSkipped = function(_node) {
    let node = _node;
    if (node.constructor === Text) {
        node = node.parentElement;
    }
    if (node.className === "CaretBrowsing_Caret" ||
        node.className === "CaretBrowsing_AnimateCaret") {
        return true;
    }
    return false;
};

TraverseUtil.forwardsChar = function(cursor, nodesCrossed) { // eslint-disable-line max-statements,max-len
    for (;;) {
        let childNode = null;
        if (!TraverseUtil.treatAsLeafNode(cursor.node)) {
            for (let i = cursor.index;
                i < cursor.node.childNodes.length;
                i++) {
                const node = cursor.node.childNodes[i];
                if (TraverseUtil.isSkipped(node)) {
                    nodesCrossed.push(node);
                } else if (TraverseUtil.isVisible(node)) {
                    childNode = node;
                    break;
                }
            }
        }
        if (childNode) {
            cursor.node = childNode;
            cursor.index = 0;
            cursor.text = TraverseUtil.getNodeText(cursor.node);
            if (cursor.node.constructor !== Text) {
                nodesCrossed.push(cursor.node);
            }
        } else {
            if (cursor.index < cursor.text.length) {
                return cursor.text[cursor.index++];
            }

            while (cursor.node !== null) {
                let siblingNode = null;
                for (let node = cursor.node.nextSibling;
                    node !== null;
                    node = node.nextSibling) {
                    if (TraverseUtil.isSkipped(node)) {
                        nodesCrossed.push(node);
                    } else if (TraverseUtil.isVisible(node)) {
                        siblingNode = node;
                        break;
                    }
                }
                if (siblingNode) {
                    cursor.node = siblingNode;
                    cursor.text = TraverseUtil.getNodeText(siblingNode);
                    cursor.index = 0;

                    if (cursor.node.constructor !== Text) {
                        nodesCrossed.push(cursor.node);
                    }

                    break;
                }

                const parentNode = cursor.node.parentNode;
                if (parentNode &&
                    parentNode.constructor !== HTMLBodyElement) {
                    cursor.node = cursor.node.parentNode;
                    cursor.text = null;
                    cursor.index = 0;
                } else {
                    return null;
                }
            }
        }
    }
};

TraverseUtil.getNextChar = function( // eslint-disable-line max-params
    startCursor, endCursor, nodesCrossed, skipWhitespace) {
    startCursor.copyFrom(endCursor);
    let fChar = TraverseUtil.forwardsChar(endCursor, nodesCrossed);
    if (fChar === null) {
        return null;
    }

    const initialWhitespace = TraverseUtil.isWhitespace(fChar);

    while ((TraverseUtil.isWhitespace(fChar)) ||
        (TraverseUtil.isSkipped(endCursor.node))) {
        fChar = TraverseUtil.forwardsChar(endCursor, nodesCrossed);
        if (fChar === null) {
            return null;
        }
    }
    if (skipWhitespace || !initialWhitespace) {
        startCursor.copyFrom(endCursor);
        startCursor.index--;
        return fChar;
    }

    for (let i = 0; i < nodesCrossed.length; i++) {
        if (TraverseUtil.isSkipped(nodesCrossed[i])) {
            endCursor.index--;
            startCursor.copyFrom(endCursor);
            startCursor.index--;
            return " ";
        }
    }
    endCursor.index--;
    return " ";
};

TraverseUtil.backwardsChar = function(cursor, nodesCrossed) {
    while (true) {
        // Move down until we get to a leaf node.
        var childNode = null;
        if (!TraverseUtil.treatAsLeafNode(cursor.node)) {
            for (var i = cursor.index - 1; i >= 0; i--) {
                var node = cursor.node.childNodes[i];
                if (TraverseUtil.isSkipped(node)) {
                    nodesCrossed.push(node);
                    continue;
                }
                if (TraverseUtil.isVisible(node)) {
                    childNode = node;
                    break;
                }
            }
        }
        if (childNode) {
            cursor.node = childNode;
            cursor.text = TraverseUtil.getNodeText(cursor.node);
            if (cursor.text.length)
                cursor.index = cursor.text.length;
            else
                cursor.index = cursor.node.childNodes.length;
            if (cursor.node.constructor != Text)
                nodesCrossed.push(cursor.node);
            continue;
        }

        // Return the previous character from this leaf node.
        if (cursor.text.length > 0 && cursor.index > 0) {
            return cursor.text[--cursor.index];
        }

        // Move to the previous sibling, going up the tree as necessary.
        while (true) {
            // Try to move to the previous sibling.
            var siblingNode = null;
            for (var node = cursor.node.previousSibling;
                node != null;
                node = node.previousSibling) {
                if (TraverseUtil.isSkipped(node)) {
                    nodesCrossed.push(node);
                    continue;
                }
                if (TraverseUtil.isVisible(node)) {
                    siblingNode = node;
                    break;
                }
            }
            if (siblingNode) {
                cursor.node = siblingNode;
                cursor.text = TraverseUtil.getNodeText(siblingNode);
                if (cursor.text.length)
                    cursor.index = cursor.text.length;
                else
                    cursor.index = cursor.node.childNodes.length;
                if (cursor.node.constructor != Text)
                    nodesCrossed.push(cursor.node);
                break;
            }

            // Otherwise, move to the parent.
            if (cursor.node.parentNode &&
                cursor.node.parentNode.constructor != HTMLBodyElement) {
                cursor.node = cursor.node.parentNode;
                cursor.text = null;
                cursor.index = 0;
            } else {
                return null;
            }
        }
    }
};

TraverseUtil.getNextWord = function(startCursor, endCursor,
    nodesCrossed) {

// Find the first non-whitespace or non-skipped character.
var cursor = endCursor.clone();
var c = TraverseUtil.forwardsChar(cursor, nodesCrossed);
if (c == null)
    return null;
while ((TraverseUtil.isWhitespace(c)) ||
    (TraverseUtil.isSkipped(cursor.node))) {
    c = TraverseUtil.forwardsChar(cursor, nodesCrossed);
    if (c == null)
    return null;
}

// Set startCursor to the position immediately before the first
// character in our word. It's safe to decrement |index| because
// forwardsChar guarantees that the cursor will be immediately to the
// right of the returned character on exit.
startCursor.copyFrom(cursor);
startCursor.index--;

// Keep building up our word until we reach a whitespace character or
// would cross a tag.  Don't actually return any tags crossed, because this
// word goes up until the tag boundary but not past it.
endCursor.copyFrom(cursor);
var word = c;
var newNodesCrossed = [];
c = TraverseUtil.forwardsChar(cursor, newNodesCrossed);
if (c == null) {
    return word;
}
while (!TraverseUtil.isWhitespace(c) &&
    newNodesCrossed.length == 0) {
    word += c;
    endCursor.copyFrom(cursor);
    c = TraverseUtil.forwardsChar(cursor, newNodesCrossed);
    if (c == null) {
    return word;
    }
}
return word;
};

TraverseUtil.getPreviousWord = function(startCursor, endCursor,
    nodesCrossed) {
// Find the first non-whitespace or non-skipped character.
var cursor = startCursor.clone();
var c = TraverseUtil.backwardsChar(cursor, nodesCrossed);
if (c == null)
    return null;
while ((TraverseUtil.isWhitespace(c) ||
    (TraverseUtil.isSkipped(cursor.node)))) {
    c = TraverseUtil.backwardsChar(cursor, nodesCrossed);
    if (c == null)
    return null;
}

// Set endCursor to the position immediately after the first
// character we've found (the last character of the word, since we're
// searching backwards).
endCursor.copyFrom(cursor);
endCursor.index++;

// Keep building up our word until we reach a whitespace character or
// would cross a tag.  Don't actually return any tags crossed, because this
// word goes up until the tag boundary but not past it.
startCursor.copyFrom(cursor);
var word = c;
var newNodesCrossed = [];
c = TraverseUtil.backwardsChar(cursor, newNodesCrossed);
if (c == null)
    return word;
while (!TraverseUtil.isWhitespace(c) &&
    newNodesCrossed.length == 0) {
    word = c + word;
    startCursor.copyFrom(cursor);
    c = TraverseUtil.backwardsChar(cursor, newNodesCrossed);
    if (c == null)
    return word;
}

return word;
};

const CaretBrowsing = {};

CaretBrowsing.isEnabled = false;

CaretBrowsing.onEnable = "flash";

CaretBrowsing.onJump = "flash";

CaretBrowsing.isWindowFocused = false;

CaretBrowsing.isCaretVisible = false;

CaretBrowsing.caretElement = undefined;

CaretBrowsing.caretX = 0;

CaretBrowsing.caretY = 0;

CaretBrowsing.caretWidth = 0;

CaretBrowsing.caretHeight = 0;

CaretBrowsing.caretForeground = "%2";

CaretBrowsing.caretBackground = "%1";

CaretBrowsing.isSelectionCollapsed = false;

CaretBrowsing.blinkFunctionId = null;

CaretBrowsing.targetX = null;

CaretBrowsing.blinkFlag = true;

CaretBrowsing.markEnabled = false;

CaretBrowsing.positionCaret = function() {
    var start = new Cursor(document.body, 0, '');
    var end = new Cursor(document.body, 0, '');
    var nodesCrossed = [];
    var result = TraverseUtil.getNextChar(start, end, nodesCrossed, true);
    if (result == null) {
        return;
    }
    CaretBrowsing.setAndValidateSelection(start, start);
}

CaretBrowsing.isFocusable = function(targetNode) {
    if (!targetNode || typeof (targetNode.tabIndex) !== "number") {
        return false;
    }

    if (targetNode.tabIndex >= 0) {
        return true;
    }

    if (targetNode.hasAttribute &&
        targetNode.hasAttribute("tabindex") &&
        targetNode.getAttribute("tabindex") === "-1") {
        return true;
    }

    return false;
}

CaretBrowsing.isControlThatNeedsArrowKeys = function(node) { // eslint-disable-line complexity,max-len
    if (!node) {
        return false;
    }

    if (node === document.body || node !== document.activeElement) {
        return false;
    }

    if (node.constructor === HTMLSelectElement) {
        return true;
    }

    if (node.constructor === HTMLInputElement) {
        switch (node.type) { // eslint-disable-line default-case
        case "email":
        case "number":
        case "password":
        case "search":
        case "text":
        case "tel":
        case "url":
        case "":
            return true;
        case "datetime":
        case "datetime-local":
        case "date":
        case "month":
        case "radio":
        case "range":
        case "week":
            return true;
        }
    }

    if (node.getAttribute && CaretBrowsing.isFocusable(node)) {
        const role = node.getAttribute("role");
        switch (role) { // eslint-disable-line default-case
        case "combobox":
        case "grid":
        case "gridcell":
        case "listbox":
        case "menu":
        case "menubar":
        case "menuitem":
        case "menuitemcheckbox":
        case "menuitemradio":
        case "option":
        case "radiogroup":
        case "scrollbar":
        case "slider":
        case "spinbutton":
        case "tab":
        case "tablist":
        case "textbox":
        case "tree":
        case "treegrid":
        case "treeitem":
            return true;
        }
    }

    return false;
};

CaretBrowsing.injectCaretStyles = function() {
    const style = ".CaretBrowsing_Caret {" +
        "  position: absolute;" +
        "  z-index: 2147483647;" +
        "  min-height: 10px;" +
        "  background-color: %1;" +
        "}" +
        ".CaretBrowsing_AnimateCaret {" +
        "  position: absolute;" +
        "  z-index: 2147483647;" +
        "  min-height: 10px;" +
        "}" +
        ".CaretBrowsing_FlashVert {" +
        "  position: absolute;" +
        "  z-index: 2147483647;" +
        "  background: linear-gradient(" +
        "      270deg," +
        "      rgba(128, 128, 255, 0) 0%," +
        "      rgba(128, 128, 255, 0.3) 45%," +
        "      rgba(128, 128, 255, 0.8) 50%," +
        "      rgba(128, 128, 255, 0.3) 65%," +
        "      rgba(128, 128, 255, 0) 100%);" +
        "}";
    const node = document.createElement("style");
    node.innerHTML = style;
    document.body.appendChild(node);
};

CaretBrowsing.setInitialCursor = function(noScrollToSelection) {
    if (CaretBrowsing.post_message_down("CaretBrowsing.setInitialCursor")) {
        return;
    }
    const sel = window.getSelection();
    if (!CaretBrowsing.initiated) {
        if (sel.rangeCount == 0) {
            CaretBrowsing.positionCaret();
        }

        CaretBrowsing.injectCaretStyles();
        CaretBrowsing.initiated = true;
    }

    CaretBrowsing.toggle();
    if (CaretBrowsing.isEnabled) {
        // when doing an i-search, the selection is totally cleared (sigh)
        if (sel.rangeCount == 0) {
            CaretBrowsing.positionCaret();
        }
        // do not handle previous selection for now, just removes it.
        CaretBrowsing.markEnabled = sel.type == "Range";
        if (CaretBrowsing.markEnabled) {
            CaretBrowsing.markEnabled = false;
            sel.collapse(sel.anchorNode, sel.anchorOffset);
            window.setTimeout(() => {
                CaretBrowsing.updateCaretOrSelection((!noScrollToSelection));
            }, 0);
        }
    }
};

CaretBrowsing.shutdown = function() {
    if (CaretBrowsing.post_message_down("CaretBrowsing.shutdown")) {
        return;
    }
    CaretBrowsing.toggle(false);
}

CaretBrowsing.setAndValidateSelection = function(start, end) {
    const sel = window.getSelection();
    sel.setBaseAndExtent(start.node, start.index, end.node, end.index);

    if (sel.rangeCount !== 1) {
        return false;
    }

    return (sel.anchorNode === start.node &&
            sel.anchorOffset === start.index &&
            sel.focusNode === end.node &&
            sel.focusOffset === end.index);
};


CaretBrowsing.setCaretElementNormalStyle = function() {
    const element = CaretBrowsing.caretElement;
    element.className = "CaretBrowsing_Caret";
    if (CaretBrowsing.isSelectionCollapsed) {
        element.style.opacity = "1.0";
    } else {
        element.style.opacity = "0.0";
    }
    element.style.left = `${CaretBrowsing.caretX}px`;
    element.style.top = `${CaretBrowsing.caretY}px`;
    element.style.width = `${CaretBrowsing.caretWidth}px`;
    element.style.height = `${CaretBrowsing.caretHeight}px`;
    element.style.color = CaretBrowsing.caretForeground;
};

CaretBrowsing.animateCaretElement = function() {
    const element = CaretBrowsing.caretElement;
    element.style.left = `${CaretBrowsing.caretX - 50}px`;
    element.style.top = `${CaretBrowsing.caretY - 100}px`;
    element.style.width = `${CaretBrowsing.caretWidth + 100}px`;
    element.style.height = `${CaretBrowsing.caretHeight + 200}px`;
    element.className = "CaretBrowsing_AnimateCaret";

    window.setTimeout(() => {
        if (!CaretBrowsing.caretElement) {
            return;
        }
        CaretBrowsing.setCaretElementNormalStyle();
        element.style.transition = "all 0.8s ease-in";
        function listener() {
            element.removeEventListener(
                "transitionend", listener, false);
            element.style.transition = "none";
        }
        element.addEventListener(
            "transitionend", listener, false);
    }, 0);
};

CaretBrowsing.flashCaretElement = function() {
    const x = CaretBrowsing.caretX;
    const y = CaretBrowsing.caretY;

    const vert = document.createElement("div");
    vert.className = "CaretBrowsing_FlashVert";
    vert.style.left = `${x - 6}px`;
    vert.style.top = `${y - 100}px`;
    vert.style.width = "11px";
    vert.style.height = `${200}px`;
    document.body.appendChild(vert);

    window.setTimeout(() => {
        document.body.removeChild(vert);
        if (CaretBrowsing.caretElement) {
            CaretBrowsing.setCaretElementNormalStyle();
        }
    }, 250);
};

CaretBrowsing.createCaretElement = function() {
    const element = document.createElement("div");
    element.className = "CaretBrowsing_Caret";
    document.body.appendChild(element);
    CaretBrowsing.caretElement = element;

    if (CaretBrowsing.onEnable === "anim") {
        CaretBrowsing.animateCaretElement();
    } else if (CaretBrowsing.onEnable === "flash") {
        CaretBrowsing.flashCaretElement();
    } else {
        CaretBrowsing.setCaretElementNormalStyle();
    }
};

CaretBrowsing.getCursorRect = function(cursor) { // eslint-disable-line max-statements,max-len
    let node = cursor.node;
    const index = cursor.index;
    const rect = {
        "left": 0,
        "top": 0,
        "width": 5,
        "height": 0,
    };
    if (node.constructor === Text) {
        let left = index;
        let right = index;
        const max = node.data.length;
        const newRange = document.createRange();
        while (left > 0 || right < max) {
            if (left > 0) {
                left--;
                newRange.setStart(node, left);
                newRange.setEnd(node, index);
                const rangeRect = newRange.getBoundingClientRect();
                if (rangeRect && rangeRect.width && rangeRect.height) {
                    rect.left = rangeRect.right;
                    rect.top = rangeRect.top;
                    rect.height = rangeRect.height;
                    break;
                }
            }
            if (right < max) {
                right++;
                newRange.setStart(node, index);
                newRange.setEnd(node, right);
                const rangeRect = newRange.getBoundingClientRect();
                if (rangeRect && rangeRect.width && rangeRect.height) {
                    rect.left = rangeRect.left;
                    rect.top = rangeRect.top;
                    rect.height = rangeRect.height;
                    break;
                }
            }
        }
    } else {
        rect.height = node.offsetHeight;
        while (node !== null) {
            rect.left += node.offsetLeft;
            rect.top += node.offsetTop;
            node = node.offsetParent;
        }
    }
    rect.left += window.pageXOffset;
    rect.top += window.pageYOffset;
    return rect;
};

CaretBrowsing.updateCaretOrSelection =
    function(scrollToSelection) { // eslint-disable-line max-statements
            const previousX = CaretBrowsing.caretX;
        const previousY = CaretBrowsing.caretY;

        const sel = window.getSelection();
        if (sel.rangeCount === 0) {
            if (CaretBrowsing.caretElement) {
                CaretBrowsing.isSelectionCollapsed = false;
                CaretBrowsing.caretElement.style.opacity = "0.0";
            }
            return;
        }

        const range = sel.getRangeAt(0);
        if (!range) {
            if (CaretBrowsing.caretElement) {
                CaretBrowsing.isSelectionCollapsed = false;
                CaretBrowsing.caretElement.style.opacity = "0.0";
            }
            return;
        }

        if (CaretBrowsing.isControlThatNeedsArrowKeys(
            document.activeElement)) {
            let node = document.activeElement;
            CaretBrowsing.caretWidth = node.offsetWidth;
            CaretBrowsing.caretHeight = node.offsetHeight;
            CaretBrowsing.caretX = 0;
            CaretBrowsing.caretY = 0;
            while (node.offsetParent) {
                CaretBrowsing.caretX += node.offsetLeft;
                CaretBrowsing.caretY += node.offsetTop;
                node = node.offsetParent;
            }
            CaretBrowsing.isSelectionCollapsed = false;
        } else if (range.startOffset !== range.endOffset ||
                range.startContainer !== range.endContainer) {
            const rect = range.getBoundingClientRect();
            if (!rect) {
                return;
            }
            CaretBrowsing.caretX = rect.left + window.pageXOffset;
            CaretBrowsing.caretY = rect.top + window.pageYOffset;
            CaretBrowsing.caretWidth = rect.width;
            CaretBrowsing.caretHeight = rect.height;
            CaretBrowsing.isSelectionCollapsed = false;
        } else {
            const rect = CaretBrowsing.getCursorRect(
                new Cursor(range.startContainer,
                        range.startOffset,
                        TraverseUtil.getNodeText(range.startContainer)));
            CaretBrowsing.caretX = rect.left;
            CaretBrowsing.caretY = rect.top;
            CaretBrowsing.caretWidth = rect.width;
            CaretBrowsing.caretHeight = rect.height;
            CaretBrowsing.isSelectionCollapsed = true;
        }

        if (CaretBrowsing.caretElement) {
            const element = CaretBrowsing.caretElement;
            if (CaretBrowsing.isSelectionCollapsed) {
                element.style.opacity = "1.0";
                element.style.left = `${CaretBrowsing.caretX}px`;
                element.style.top = `${CaretBrowsing.caretY}px`;
                element.style.width = `${CaretBrowsing.caretWidth}px`;
                element.style.height = `${CaretBrowsing.caretHeight}px`;
            } else {
                element.style.opacity = "0.0";
            }
        } else {
            CaretBrowsing.createCaretElement();
        }

        let elem = range.startContainer;
        if (elem.constructor === Text) {
            elem = elem.parentElement;
        }
        CaretBrowsing.caretBackground = "%1";
        CaretBrowsing.caretForeground = "%2";

        if (scrollToSelection) {
            const rect = CaretBrowsing.getCursorRect(
                new Cursor(sel.focusNode, sel.focusOffset,
                        TraverseUtil.getNodeText(sel.focusNode)));

            const yscroll = window.pageYOffset;
            const pageHeight = window.innerHeight;
            const caretY = rect.top;
            const caretHeight = Math.min(rect.height, 30);
            if (yscroll + pageHeight < caretY + caretHeight) {
                window.scroll(0, (caretY + caretHeight - pageHeight + 100));
            } else if (caretY < yscroll) {
                window.scroll(0, (caretY - 100));
            }
        }

        if (Math.abs(previousX - CaretBrowsing.caretX) > 500 ||
            Math.abs(previousY - CaretBrowsing.caretY) > 100) {
            if (CaretBrowsing.onJump === "anim") {
                CaretBrowsing.animateCaretElement();
            } else if (CaretBrowsing.onJump === "flash") {
                CaretBrowsing.flashCaretElement();
            }
        }
    };

CaretBrowsing.toggle = function(enabled) {
    if (enabled == undefined) {
        enabled = !CaretBrowsing.isEnabled;
    }

    CaretBrowsing.isEnabled = enabled;
    const obj = {};
    obj.enabled = CaretBrowsing.isEnabled;
    CaretBrowsing.updateIsCaretVisible();
    if (!obj.enabled) {
        var sel = window.getSelection();
        sel.collapse(sel.focusNode, sel.focusOffset);
    }
};

CaretBrowsing.onClick = function() {
    if (!CaretBrowsing.isEnabled) {
        return true;
    }
    window.setTimeout(() => {
        CaretBrowsing.targetX = null;
        CaretBrowsing.updateCaretOrSelection(false);
    }, 0);
    return true;
};

CaretBrowsing.caretBlinkFunction = function() {
    if (CaretBrowsing.caretElement) {
        if (CaretBrowsing.blinkFlag) {
            CaretBrowsing.caretElement.style.backgroundColor =
                CaretBrowsing.caretForeground;
            CaretBrowsing.blinkFlag = false;
        } else {
            CaretBrowsing.caretElement.style.backgroundColor =
                CaretBrowsing.caretBackground;
            CaretBrowsing.blinkFlag = true;
        }
    }
};

CaretBrowsing.updateIsCaretVisible = function() {
    CaretBrowsing.isCaretVisible =
        (CaretBrowsing.isEnabled && CaretBrowsing.isWindowFocused);
    if (CaretBrowsing.isCaretVisible && !CaretBrowsing.caretElement) {
        // CaretBrowsing.setInitialCursor();
        CaretBrowsing.updateCaretOrSelection(true);
        if (CaretBrowsing.caretElement) {
            CaretBrowsing.blinkFunctionId = window.setInterval(
                CaretBrowsing.caretBlinkFunction, 500);
        }
    } else if (!CaretBrowsing.isCaretVisible &&
            CaretBrowsing.caretElement) {
        window.clearInterval(CaretBrowsing.blinkFunctionId);
        if (CaretBrowsing.caretElement) {
            CaretBrowsing.isSelectionCollapsed = false;
            CaretBrowsing.caretElement.parentElement.removeChild(
                CaretBrowsing.caretElement);
            CaretBrowsing.caretElement = null;
        }
    }
};

CaretBrowsing.onWindowFocus = function() {
    CaretBrowsing.isWindowFocused = true;
    CaretBrowsing.updateIsCaretVisible();
};

CaretBrowsing.onWindowBlur = function() {
    CaretBrowsing.isWindowFocused = false;
    CaretBrowsing.updateIsCaretVisible();
};

CaretBrowsing.init = function() {
    CaretBrowsing.isWindowFocused = document.hasFocus();

    document.addEventListener("click", CaretBrowsing.onClick, false);
    window.addEventListener("focus", CaretBrowsing.onWindowFocus, false);
    window.addEventListener("blur", CaretBrowsing.onWindowBlur, false);
};

window.setTimeout(() => {
    if (!window.caretBrowsingLoaded) {
        window.caretBrowsingLoaded = true;
        CaretBrowsing.init();
    }
}, 0);


CaretBrowsing.move = function(direction, granularity) {
    if (CaretBrowsing.post_message_down("CaretBrowsing.move",
                                        [direction, granularity])) {
        return;
    }
    var sel = window.getSelection();
    var action = CaretBrowsing.markEnabled ? "extend" : "move";
    sel.modify(action, direction, granularity);
    window.setTimeout(() => {
        CaretBrowsing.updateCaretOrSelection(true);
    }, 0);
};


CaretBrowsing.toggleMark = function() {
    if (CaretBrowsing.post_message_down("CaretBrowsing.toggleMark")) {
        return;
    }
    CaretBrowsing.markEnabled = !CaretBrowsing.markEnabled;
    if (!CaretBrowsing.markEnabled) {
        var sel = window.getSelection();
        sel.collapse(sel.focusNode, sel.focusOffset);
        window.setTimeout(() => {
            CaretBrowsing.updateCaretOrSelection(true);
        }, 0);
    }
};

CaretBrowsing.rotateSelection = function() {
        var selection = window.getSelection();
        var pos = [selection.anchorNode, selection.anchorOffset];
        selection.collapse(selection.focusNode, selection.focusOffset);
        selection.extend(pos[0], pos[1]);
        window.setTimeout(() => {
            CaretBrowsing.updateCaretOrSelection(true);
        }, 0);
}

CaretBrowsing.cutSelection = function() {
    if (CaretBrowsing.post_message_down("CaretBrowsing.cutSelection")) {
        return;
    }
    // clear the current selection
    if (CaretBrowsing.markEnabled) { CaretBrowsing.toggleMark(); }
};

CaretBrowsing.post_message_down = function(message_name, arg) {
    if (document.activeElement.tagName === "IFRAME") {
        post_message(document.activeElement.contentWindow, message_name, arg);
        return true;
    }
    return false;
}

if (self !== top) {
    register_message_handler("CaretBrowsing.setInitialCursor",
                            CaretBrowsing.setInitialCursor);
    register_message_handler("CaretBrowsing.shutdown",
                            CaretBrowsing.shutdown);
    register_message_handler("CaretBrowsing.move", function (args) {
        CaretBrowsing.move(args[0], args[1]);
    });
    register_message_handler("CaretBrowsing.toggleMark",
                            CaretBrowsing.toggleMark);
    register_message_handler("CaretBrowsing.cutSelection",
                            CaretBrowsing.cutSelection);
}
