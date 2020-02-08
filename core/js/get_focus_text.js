(function() {
    const activeElement = document.activeElement;
    var inputs = ["input", "select", "textarea"];

    if (activeElement && inputs.indexOf(activeElement.tagName.toLowerCase()) !== -1) {
        return activeElement.value;
    } else {
        return undefined;
    }
})();
