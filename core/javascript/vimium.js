(function() {
    function createMarkerStyle(){
        let head = document.querySelector('head');
        let style = document.createElement('style');
        head.appendChild(style);
        style.type = 'text/css';
    }
    function getLinkCoords(link){
        let rect = link.getBoundingClientRect();
        return [ rect.top, rect.right, rect.bottom, rect.left ];
    }
    let links = document.links;
    let validLinkRects = [];
    let rect;
    for(let i = 0; i < links.length; i++) {
        rect = getLinkCoords(links[i]);
        if(!(rect.top == 0 && rect.right == 0 && rect.bottom == 0 && rect.left == 0)){
            validLinkRects.push(rect);
        }
    }
    let body = document.querySelector('body');
    let markerContainer = document.createElement('div');
    markerContainer.setAttribute('class', 'markerContainer');
    body.insertAdjacentElement('afterend', markerContainer);
})();
