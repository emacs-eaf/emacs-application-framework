(function() {
    let newText = "%1";
    const activeElement = document.activeElement;

    if (window.location.href.startsWith("https://web.telegram.org/")) {
        activeElement.textContent = decodeURIComponent(escape(window.atob(newText)));
    } else {
        activeElement.value = decodeURIComponent(escape(window.atob(newText)));
    }

    // Note: simulate input event on active element after set focus text.
    // Some website need input event before submit form.
    var event = document.createEvent('Event');
    event.initEvent('input', true, true);
    activeElement.dispatchEvent(event);
})();
