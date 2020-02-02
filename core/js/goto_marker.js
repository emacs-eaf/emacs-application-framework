(function() {
    let key = "%1";
    let markers = document.querySelectorAll('.marker');
    let match;
    for(let i = 0; i < markers.length; i++) {
        if(markers[i].getAttribute('key') === key.toUpperCase()) {
            match = markers[i];
            break;
        }
    }

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

    if(match != undefined){
        let selector = match.getAttribute('pointed-link');
        let link = document.querySelector(selector);
        if(link.nodeName.toLowerCase() === 'input' ||
           link.nodeName.toLowerCase() === 'textarea') {
            if(link.getAttribute('type') === 'submit'){
                link.submit();
            } else if(link.getAttribute('type') === 'checkbox'){
                link.click();
            } else {
                link.focus();

                // We ned move cursor to end of input after focus input.
                moveCursorToEnd(link);
            }
        } else if((link.nodeName.toLowerCase() === 'button') || // normal button
                  (link.hasAttribute('aria-haspopup')) || // menu button
                  (link.getAttribute('role') === 'button') || // role="button" buttons
                  (link.classList.contains('btn')) || // class="btn" buttons
                  (link.getAttribute('href') === '')){ // special href case that's button
            link.click();
        } else if(link.href != undefined && link.href != '' && link.getAttribute('href') != ''){
            return link.href;
        }
    }
    return "";
})();
