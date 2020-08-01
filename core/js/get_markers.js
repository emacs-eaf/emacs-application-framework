(function() {
    function cssSelector(el) {
        let path = [], parent;
        while (parent = el.parentNode) {
            path.unshift(`${el.tagName}:nth-child(${[].indexOf.call(parent.children, el)+1})`);
            el = parent;
        }
        return `${path.join(' > ')}`.toLowerCase();
    }

    function getCoords(node){
        let rect = node.getBoundingClientRect();
        return [ rect.top, rect.left, rect.right, rect.bottom, cssSelector(node) ];
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
    let operationString = "%2";
    let validRects = [];
    if (operationString != "select_text") {
        addElementToRects(validRects, document.links); // collect links
        addElementToRects(validRects, document.querySelectorAll('a')); // collect <a> without href
        addElementToRects(validRects, document.querySelectorAll('input')); // collect <input>
        addElementToRects(validRects, document.querySelectorAll('button')); // collect <button>
        addElementToRects(validRects, document.querySelectorAll('[class*="btn"]')); // collect class=btn buttons
        addElementToRects(validRects, document.querySelectorAll('[aria-haspopup]')); // collect menu buttons
        addElementToRects(validRects, document.querySelectorAll('[role="button"]')); // collect role="button"
        addElementToRects(validRects, document.querySelectorAll('textarea')); // collect <textarea>
        addElementToRects(validRects, document.querySelectorAll('select')); // collect <select>
        addElementToRects(validRects, document.querySelectorAll('summary')); // collect <summary>
        addElementToRects(validRects, document.querySelectorAll('[class="gap"]')); // collect class="gap"
        addElementToRects(validRects, document.querySelectorAll('[ng-click]')); // collect ng-click
    }
    else {
        addElementToRects(validRects, document.querySelectorAll('p')); // collect <p>
        let spanElements = document.querySelectorAll('span')
        let FilteredspanElements = [];
        for(let i = 0; i < spanElements.length; i++) {
            if(spanElements[i].innerText != "") {
                isInnerElement = false;
                parNode = spanElements[i].parentNode;
                if(parNode) {
                    while(parNode.nodeName.toLowerCase() != 'html' && parNode.nodeName.toLowerCase() != 'div'){
                        if(parNode.nodeName.toLowerCase() != 'p' && parNode.nodeName.toLowerCase() != 'span') {
                            parNode = parNode.parentNode;
                            if(!parNode) {
                                break
                            }
                        }
                        else {
                            isInnerElement = true;
                            break;
                        }
                    }
                }
                if (!isInnerElement) {
                    // collect <span> elements with text and not being Inner Element
                    FilteredspanElements.push(spanElements[i]);
                }
            }
        }
        addElementToRects(validRects, FilteredspanElements);
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
})();
