(function() {
    let newText = "%1";
    const activeElement = document.activeElement;
    activeElement.value = decodeURIComponent(escape(window.atob(newText)));
})();
