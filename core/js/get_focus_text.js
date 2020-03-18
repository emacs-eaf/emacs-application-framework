(function() {
    const activeElement = document.activeElement;
    var inputs = ["input", "select", "textarea"];

    if (activeElement && inputs.indexOf(activeElement.tagName.toLowerCase()) !== -1) {
        return activeElement.value;
    } else {
        if (window.location.href.startsWith("https://web.telegram.org/") && activeElement.hasAttribute("placeholder")) {
            return activeElement.textContent;
        } else {
            return undefined;
        }
    }
})();
