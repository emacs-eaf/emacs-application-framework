(function() {
    function moveCursorToEnd(el) {
        if (typeof el.selectionStart == "number") {
            el.selectionStart = el.selectionEnd = el.value.length;
        } else if (typeof el.createTextRange != "undefined") {
            el.focus();
            var range = el.createTextRange();
            range.collapse(false);
            range.select();
        }
    }

    let key = "%1";
    let markers = document.querySelectorAll('.eaf-marker');
    let match;
    for(let i = 0; i < markers.length; i++) {
        if(markers[i].id === key.toUpperCase()) {
            match = markers[i];
            break;
        }
    }

    if(match != undefined){
        let selector = match.getAttribute('pointed-link');
        let node = document.querySelector(selector);

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
    }
    return "";
})();
