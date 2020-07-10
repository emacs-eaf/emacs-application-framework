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
        let link = document.querySelector(selector);

        if(link.nodeName.toLowerCase() === 'select'){
            link.focus();
        }else if(link.nodeName.toLowerCase() === 'input' ||
           link.nodeName.toLowerCase() === 'textarea') {
            if((link.getAttribute('type') === 'submit') ||
               (link.getAttribute('type') === 'checkbox')){
                link.click();
            } else {
                link.focus();   // focus
                link.click();   // show blink cursor
                moveCursorToEnd(link); // move cursor to the end of line after focus.
            }
        } else if((link.nodeName.toLowerCase() === 'button') || // normal button
                  (link.nodeName.toLowerCase() === 'summary') || // summary button
                  (link.hasAttribute('aria-haspopup')) || // menu button
                  (link.getAttribute('role') === 'button') || // role="button" buttons
                  (link.hasAttribute('ng-click')) || // ng-click buttons
                  (link.classList.contains('btn')) || // class="btn" buttons
                  (link.classList.contains('gap')) || // class="gap" links
                  (link.getAttribute('href') === '') || // special href button
                  (link.getAttribute('href') === '#')){  // special href # button
            link.click();
        } else if(link.href != undefined && link.href != '' && link.getAttribute('href') != ''){
            return link.href;
        } else if(link.nodeName.toLowerCase() === 'a') {
            link.click(); // most general a tag without href
        }
    }
    return "";
})();
