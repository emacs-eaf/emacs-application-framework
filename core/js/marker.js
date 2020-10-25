(function(_) {
    let self;
    _.Marker = self = {};


    function getVisibleElements(filter) {
        let all = Array.from(document.documentElement.getElementsByTagName("*"));
        let visibleElements = [];
        for (let i = 0; i < all.length; i++) {
            let e = all[i];
            // include elements in a shadowRoot.
            if (e.shadowRoot) {
                let cc = e.shadowRoot.querySelectorAll('*');
                for (let j = 0; j < cc.length; j++) {
                    all.push(cc[j]);
                }
            }
            let rect = e.getBoundingClientRect();
            if ( (rect.top <= window.innerHeight) && (rect.bottom >= 0)
                 && (rect.left <= window.innerWidth) && (rect.right >= 0)
                 && rect.height > 0
                 && getComputedStyle(e).visibility !== 'hidden'
               ) {
                filter(e, visibleElements);
            }
        }
        return visibleElements;
    }

    function moveCursorToEnd(el) {
        if (typeof el.selectionStart == "number") {
            el.selectionStart = el.selectionEnd = el.value.length;
        } else if (typeof el.createTextRange != "undefined") {
            el.focus();
            let range = el.createTextRange();
            range.collapse(false);
            range.select();
        }
    }

    function cssSelector(el) {
        let path = [], parent;
        while (parent = el.parentNode) {
            path.unshift(`${el.tagName}:nth-child(${[].indexOf.call(parent.children, el)+1})`);
            el = parent;
        }
        return `${path.join(' > ')}`.toLowerCase();
    }

    function getCoords(node){
        if (node.getBoundingClientRect){
            let rect = node.getBoundingClientRect();
            return [ rect.top, rect.left, rect.right, rect.bottom, cssSelector(node) ];
        }

        return getCoords(node.parentNode); // TextNode not define getBoundingClientRect
    }

    function isElementOnScreen(rect){
        let clientHeight = document.documentElement.clientHeight;
        let clientWidth = document.documentElement.clientWidth;
        return (rect[0] >= 0 && rect[0] <= clientHeight &&
                rect[1] >= 0 && rect[1] <= clientWidth &&
                rect[2] != 0 && rect[3] != 0);
    }

    function isElementOnTop(element, rect){
        let topElement = document.elementFromPoint((rect[1] + rect[2])/2, (rect[0] + rect[3])/2);
        return topElement != undefined && (element.isSameNode(topElement) || element.contains(topElement) || topElement.contains(element));
    }

    function hasCopy(validRects, rect){
        for(let i = 0; i < validRects.length; i++) {
            let each = validRects[i];
            if(each[0] === rect[0] && each[1] === rect[1]){
                return true;
            }
        }
        return false;
    }

    function addElementToRects(validRects, elements){
        let rect;
        for(let i = 0; i < elements.length; i++) {
            rect = getCoords(elements[i]);
            if(!hasCopy(validRects, rect) &&
               isElementOnScreen(rect) &&
               isElementOnTop(elements[i], rect)){
                validRects.push(rect);
            }
        }
    }

    function cAdd1(keyCounter, index, maxDigit){
        if(keyCounter[index] + 1 == maxDigit){
            keyCounter[index] = 0;
            cAdd1(keyCounter, index + 1, maxDigit);
        } else {
            keyCounter[index]++;
        }
    }

    function generateKeys(markerContainer) {
        let lettersString = "%1";
        let letters = lettersString.split("");
        let nodeNum = markerContainer.children.length;
        let keyLen = nodeNum == 1 ? 1 : Math.ceil(Math.log(nodeNum)/Math.log(letters.length));
        let keyCounter = [];
        for(let i = 0; i < keyLen; i++) keyCounter[i] = 0;
        for(let l = 0; l < nodeNum; l++) {
            let keyStr = '';
            for(let k = 0; k < keyLen; k++) {
                let mark = document.createElement('span');
                mark.setAttribute('class', 'eaf-mark');
                let key = letters[keyCounter[k]];
                mark.textContent = key;
                markerContainer.children[l].appendChild(mark);
                keyStr += key;
                cAdd1(keyCounter, 0, letters.length);
            }
            markerContainer.children[l].id = keyStr;
        }
    }


    self.generateMarker = (selectors) => {

        let style = document.createElement('style');
        document.head.appendChild(style);
        style.type = 'text/css';
        style.setAttribute('class', 'eaf-style');
        style.appendChild(document.createTextNode('\
.eaf-mark {\
background: none;\
border: none;\
bottom: auto;\
box-shadow: none;\
color: black !important;\
cursor: auto;\
display: inline;\
float: none;\
font-size: inherit;\
font-variant: normal;\
font-weight: bold;\
height: auto;\
left: auto;\
letter-spacing: 0;\
line-height: 100%;\
margin: 0;\
max-height: none;\
max-width: none;\
min-height: 0;\
min-width: 0;\
opacity: 1;\
padding: 0;\
position: static;\
right: auto;\
text-align: left;\
text-decoration: none;\
text-indent: 0;\
text-shadow: none;\
text-transform: none;\
top: auto;\
vertical-align: baseline;\
white-space: normal;\
width: auto;\
z-index: 100000;\
}'));
        style.appendChild(document.createTextNode('\
.eaf-marker {\
position: fixed;\
display: block;\
white-space: nowrap;\
overflow: hidden;\
font-size: 11.5px;\
background: linear-gradient(to bottom, #ffdd6e 0%, #deb050 100%);\
padding-left: 3px;\
padding-right: 3px;\
border: 1px solid #c38a22;\
border-radius: 3px;\
box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);\
z-index: 100000;\
}'));

        let validRects = [];
        if (typeof(selectors)=="function"){
            addElementToRects(validRects, selectors());
        }else if (typeof(selectors) == "string"){
            selectors = selectors.split(",");
            selectors.forEach((s)=>addElementToRects(validRects, document.querySelectorAll(s.trim())));
        }

        let body = document.querySelector('body');
        let markerContainer = document.createElement('div');
        markerContainer.setAttribute('class', 'eaf-marker-container');
        body.insertAdjacentElement('afterend', markerContainer);
        for(let i = 0; i < validRects.length; i++) {
            let marker = document.createElement('div');
            marker.setAttribute('class', 'eaf-marker');
            marker.setAttribute('style', 'left: ' + validRects[i][1] + 'px; top: ' + validRects[i][0] + 'px;');
            marker.setAttribute('pointed-link', validRects[i][4]);

            markerContainer.appendChild(marker);
        }
        generateKeys(markerContainer);
    }

    self.gotoMarker = (key, callback)=>{
        let markers = document.querySelectorAll('.eaf-marker');
        let match;
        for(let i = 0; i < markers.length; i++) {
            if(markers[i].id === key.toUpperCase()) {
                match = markers[i];
                break;
            }
        }
        if (match !== undefined && callback !== undefined){
            let selectors = match.getAttribute('pointed-link');
            let node = document.querySelector(selectors);
            return callback(node);
        } else
            return "";
    }

    // this is callback function which call by core/browser.py get_mark_link
    self.getMarkerLink = (node) => {
        if(node.nodeName.toLowerCase() === 'select'){
            node.focus();
        }else if(node.nodeName.toLowerCase() === 'input' ||
                 node.nodeName.toLowerCase() === 'textarea') {
            if((node.getAttribute('type') === 'submit') ||
               (node.getAttribute('type') === 'checkbox')){
                node.click();
            } else {
                node.focus();   // focus
                node.click();   // show blink cursor
                moveCursorToEnd(node); // move cursor to the end of line after focus.
            }
        } else if((node.nodeName.toLowerCase() === 'button') || // normal button
                  (node.nodeName.toLowerCase() === 'summary') || // summary button
                  (node.hasAttribute('aria-haspopup')) || // menu button
                  (node.getAttribute('role') === 'button') || // role="button" buttons
                  (node.hasAttribute('ng-click')) || // ng-click buttons
                  (node.classList.contains('btn')) || // class="btn" buttons
                  (node.classList.contains('collapsible')) || // class="collapsible" buttons
                  (node.classList.contains('gap')) || // class="gap" links
                  (node.getAttribute('href') === '') || // special href button
                  (node.getAttribute('href') === '#')){  // special href # button
            node.click();
        } else if(node.nodeName.toLowerCase() === 'p'||
                  node.nodeName.toLowerCase() === 'span') {  // select text section
            window.getSelection().selectAllChildren(node);
        } else if(node.href != undefined && node.href != '' && node.getAttribute('href') != ''){
            return node.href;
        } else if(node.nodeName.toLowerCase() === 'a') {
            node.click(); // most general a tag without href
        }
        return "";
    }


    self.generateTextNodeMarker = () => {

        let elements = getVisibleElements(function(e, v) {
            let aa = e.childNodes;
            for (let i = 0, len = aa.length; i < len; i++) {
                if (aa[i].nodeType == Node.TEXT_NODE && aa[i].data.length > 0) {
                    v.push(e);
                    break;
                }
            }
        });

        elements = Array.prototype.concat.apply([], elements.map(function (e) {
            let aa = e.childNodes;
            let bb = [];
            for (let i = 0, len = aa.length; i < len; i++) {
                if (aa[i].nodeType == Node.TEXT_NODE && aa[i].data.trim().length > 1) {
                    bb.push(aa[i]);
                }
            }
            return bb;
        }));

        return elements;
    }

})(window);
