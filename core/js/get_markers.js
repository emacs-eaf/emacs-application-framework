(function() {
    function cssSelector(el) {
        let path = [];
        while (el.nodeType === Node.ELEMENT_NODE) {
            let selector = el.nodeName.toLowerCase();
            if (el.id) {
                selector += '#' + el.id;
                path.unshift(selector);
                break;
            } else {
                let sib = el, nth = 1;
                while (sib = sib.previousElementSibling) {
                    if (sib.nodeName.toLowerCase() == selector)
                        nth++;
                }
                if (nth != 1)
                    selector += ":nth-of-type("+nth+")";
            }
            path.unshift(selector);
            el = el.parentNode;
        }
        return path.join(" > ");
    }

    function getCoords(link){
        let rect = link.getBoundingClientRect();
        return [ rect.top, rect.left, rect.right, rect.bottom, cssSelector(link) ];
    }

    function isElementOnScreen(rect){
        let clientHeight = document.documentElement.clientHeight;
        let clientWidth = document.documentElement.clientWidth;
        return (rect[0] >= 0 && rect[0] <= clientHeight &&
                rect[1] >= 0 && rect[1] <= clientWidth &&
                rect[2] != 0 && rect[3] != 0);
    }

    function isElementOnTop(element, rect){
        let topElement = document.elementFromPoint(rect[1], rect[0]);
        return element.isSameNode(topElement) || element.contains(topElement) || topElement.contains(element);
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
            if(isElementOnScreen(rect)
               && isElementOnTop(elements[i], rect)
               && !hasCopy(validRects, rect)){
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
        let letters = ['A', 'S', 'D', 'F', 'Q', 'W', 'E', 'X', 'C', 'H', 'J', 'K', 'L', 'O', 'P', 'N', 'M'];
        let linkNum = markerContainer.children.length;
        let keyLen = linkNum == 1 ? 1 : Math.ceil(Math.log(linkNum)/Math.log(letters.length));
        let keyCounter = [];
        for(let i = 0; i < keyLen; i++) keyCounter[i] = 0;
        for(let l = 0; l < linkNum; l++) {
            let keyStr = '';
            for(let k = 0; k < keyLen; k++) {
                let mark = document.createElement('span');
                mark.setAttribute('class', 'link-mark');
                mark.setAttribute('style', 'font-size: 12px; font-weight: bold;');
                let key = letters[keyCounter[k]];
                mark.textContent = key;
                markerContainer.children[l].appendChild(mark);
                keyStr += key;
                cAdd1(keyCounter, 0, letters.length);
            }
            markerContainer.children[l].setAttribute('key', keyStr);
        }
    }

    let validRects = [];
    addElementToRects(validRects, document.links); // collect links
    addElementToRects(validRects, document.querySelectorAll('input')); // collect inputs
    addElementToRects(validRects, document.querySelectorAll('button')); // collect buttons
    addElementToRects(validRects, document.querySelectorAll('[class*="btn"]')); // collect btn buttons
    addElementToRects(validRects, document.querySelectorAll('[aria-haspopup]')); // collect menu buttons
    addElementToRects(validRects, document.querySelectorAll('[role="button"]')); // collect role="button"
    addElementToRects(validRects, document.querySelectorAll('textarea')); // collect textarea
    addElementToRects(validRects, document.querySelectorAll('select')); // collect select

    let body = document.querySelector('body');
    let markerContainer = document.createElement('div');
    markerContainer.setAttribute('class', 'markerContainer');
    body.insertAdjacentElement('afterend', markerContainer);
    for(let i = 0; i < validRects.length; i++) {
        let marker = document.createElement('div');
        marker.setAttribute('class', 'marker');
        marker.setAttribute('style', 'left: ' +
                            validRects[i][1] + 'px; top: ' +
                            validRects[i][0] + 'px; z-index: 100000; position: fixed; ' +
                            ' background-color: #ffdb60; border: 1px solid #c38a22; padding-left: 3px; padding-right: 3px; border-radius: 3px;');
        marker.setAttribute('pointed-link', validRects[i][4]);

        markerContainer.appendChild(marker);
    }
    generateKeys(markerContainer);
})();
