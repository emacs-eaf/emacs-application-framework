(function() {
    let newText = "%1";
    const activeElement = document.activeElement;
    activeElement.value = decodeURIComponent(escape(window.atob(newText)));

    let clickEvent = document.createEvent('Events');
    clickEvent.initEvent("click", true, false);
    activeElement.dispatchEvent(clickEvent);
})();
