(function() {
    let newText = "%1";
    const activeElement = document.activeElement;
    activeElement.value = decodeURIComponent(escape(window.atob(newText)));

    // Note: simulate click event on active element after set focus text.
    // Some website need click input element before submit form.
    let clickEvent = document.createEvent('Events');
    clickEvent.initEvent("click", true, false);
    activeElement.dispatchEvent(clickEvent);
})();
