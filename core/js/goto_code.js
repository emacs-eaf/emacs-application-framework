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

        return link.textContent;
    }

    return "";
})();
