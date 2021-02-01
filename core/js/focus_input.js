(function() {
    function getVisibleElements(filter) {
        var all = Array.from(document.documentElement.getElementsByTagName("*"));
        var visibleElements = [];
        for (var i = 0; i < all.length; i++) {
            var e = all[i];
            // include elements in a shadowRoot.
            if (e.shadowRoot) {
                var cc = e.shadowRoot.querySelectorAll('*');
                for (var j = 0; j < cc.length; j++) {
                    all.push(cc[j]);
                }
            }
            var rect = e.getBoundingClientRect();
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
    var cssSelector = "input";

    var elements = getVisibleElements(function(e, v) {
        if (e.matches(cssSelector) && !e.disabled && !e.readOnly
            && (e.type === "text" || e.type === "search" || e.type === "password")) {
            v.push(e);
        }
    });

    if (elements.length === 0 && document.querySelector(cssSelector) !== null) {
        document.querySelector(cssSelector).scrollIntoView();
        elements = getVisibleElements(function(e, v) {
            if (e.matches(cssSelector) && !e.disabled && !e.readOnly) {
                v.push(e);
            }
        });
    }

    if (elements.length >= 1) {
        var focusElement = elements[0];
        var value = focusElement.value;
        focusElement.focus();
        focusElement.value = "";
        focusElement.value = value;
    }
})();
