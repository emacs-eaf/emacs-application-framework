(function() {
    let letters = "ASDFHJKLQWEIOP";
    function generateKeys(markerContainer) {
        let linkNum = markerContainer.children.length;
        let keyLen = Math.floor(linkNum/letters.length);
        console.log(linkNum);
    }
    function getCoords(link){
        let rect = link.getBoundingClientRect();
        return [ rect.top, rect.left, rect.right, rect.bottom ];
    }
    function isElementOnScreen(rect){
        console.log(rect);
        let topOffset  = window.pageYOffset || document.documentElement.scrollTop;
        let leftOffset = window.pageXOffset || document.documentElement.scrollLeft;
        let clientHeight = document.documentElement.clientHeight;
        let clientWidth = document.documentElement.clientWidth;
        return (rect[0] >= 0 && rect[0] <= clientHeight &&
                rect[1] >= 0 && rect[1] <= clientWidth &&
                rect[2] != 0 && rect[3] != 0);
    }
    function addElementToRects(validRects, elements){
        let rect;
        for(let i = 0; i < elements.length; i++) {
            rect = getCoords(elements[i]);
            if(isElementOnScreen(rect)){
                validRects.push(rect);
            }
        }
    }
    let head = document.querySelector('head');
    let style = document.createElement('style');
    head.appendChild(style);
    style.type = 'text/css';
    let validRects = [];

    addElementToRects(validRects, document.links);
    addElementToRects(validRects, document.querySelectorAll('button'));
    addElementToRects(validRects, document.querySelectorAll('input'));

    let body = document.querySelector('body');
    let markerContainer = document.createElement('div');
    markerContainer.setAttribute('class', 'markerContainer');
    body.insertAdjacentElement('afterend', markerContainer);
    for(let i = 0; i < validRects.length; i++) {
        let marker = document.createElement('div');

        marker.setAttribute('class', 'marker');
        marker.setAttribute('style', 'left: ' + validRects[i][1] + 'px; top: '+ validRects[i][0] + 'px; z-index: 2140000010; position: fixed;');
        marker.textContent = "LINK";
        markerContainer.appendChild(marker);
    }
    generateKeys(markerContainer);

})();
