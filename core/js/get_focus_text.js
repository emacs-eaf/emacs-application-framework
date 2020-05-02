(function() {
    const activeElement = document.activeElement;
    var inputs = ["input", "select", "textarea"];

    if (window.location.href === "https://mail.qq.com/" && activeElement) {
        // QQ mail have some security mechanism that we can't fetch value of activeElement.
        // So we just return empty string make is_focus method works well in browser.py
        return "";
    } else if (activeElement && inputs.indexOf(activeElement.tagName.toLowerCase()) !== -1) {
        return activeElement.value;
    } else {
        if (window.location.href.startsWith("https://web.telegram.org/") && activeElement.hasAttribute("placeholder")) {
            return activeElement.textContent;
        } else if ((window.location.href.startsWith("https://console.cloud.google.com/") || window.location.href.startsWith("https://ssh.cloud.google.com/")) && activeElement.tagName.toLowerCase() == "iframe") {
            return "";
        } else {
            return undefined;
        }
    }
})();
