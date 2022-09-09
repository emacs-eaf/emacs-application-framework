(function() {
  const activeElement = document.activeElement;
  var inputs = ["input", "select", "textarea"];
  var inputTypes = ["text", "textarea", "email"];
  var pageUrl = window.location.href;
  var tagName = activeElement.tagName.toLowerCase();

  if (pageUrl === "https://mail.qq.com/" && activeElement) {
    // QQ mail have some security mechanism that we can't fetch value of activeElement.
    // So we just return empty string make is_focus method works well in browser.py
    return "";
  } else if (activeElement &&
             inputs.indexOf(tagName) !== -1 &&
             inputTypes.indexOf(activeElement.type) !== -1
             ) {
    return activeElement.value;
  } else if (activeElement.isContentEditable) {
    // For the Rich Text Editor
    return activeElement.textContent;
  } else {
    if (pageUrl.startsWith("https://web.telegram.org/") && activeElement.hasAttribute("placeholder")) {
      return activeElement.textContent;
    } else if ((pageUrl.startsWith("https://console.cloud.google.com/") || pageUrl.startsWith("https://ssh.cloud.google.com/")) && tagName == "iframe") {
      // Make user can type text in Terminal of Google Cloud.
      return "";
    } else {
      return undefined;
    }
  }
})();
