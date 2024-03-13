(function() {
    let newText = "%{new_text_base64}";
    const activeElement = document.activeElement;

    // Decode the base64 encoded text
    const decodedText = decodeURIComponent(escape(window.atob(newText)));

    // Check if the active element is a typical input or textarea
    if ("value" in activeElement) {
	activeElement.value = decodedText;
    } else {
	// For contenteditable elements or others (like divs for Telegram and Google Gemini),
	// directly set textContent. This approach works uniformly across various types of elements.
	activeElement.textContent = decodedText;
    }

    // Simulate input event on the active element or a found contenteditable element
    // after setting the text. Some websites need input event before form submission.
    var event = new Event('input', { bubbles: true, cancelable: true });
    activeElement.dispatchEvent(event);
})();
