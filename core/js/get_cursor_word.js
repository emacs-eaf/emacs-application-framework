(function() {
    const getWordAtPoint = (x, y) => {
        const element = document.elementFromPoint(x, y);

        if (element && (element.tagName === 'INPUT' || element.tagName === 'TEXTAREA')) {
            // Simulate a click at the current cursor position
            const clickEvent = new MouseEvent('click', {
                view: window,
                bubbles: true,
                cancelable: true,
                clientX: x,
                clientY: y,
            });
            document.body.dispatchEvent(clickEvent);

            // Then focus on the form element
            const inputElement = element;
            inputElement.focus();

            // Get the word at the cursor position
            const cursorPosition = inputElement.selectionStart;
            const inputValue = inputElement.value;

            let start = cursorPosition;
            while (start > 0 && !/\s/.test(inputValue[start - 1])) {
                start--;
            }

            let end = cursorPosition;
            while (end < inputValue.length && !/\s/.test(inputValue[end])) {
                end++;
            }

            return inputValue.substring(start, end);
        } else {
            const range = document.caretRangeFromPoint(x, y);
            if (range && range.startContainer.nodeType === Node.TEXT_NODE) {
                const data = range.startContainer.data;
                const offset = range.startOffset;

                let start = offset;
                while (start > 0 && !/\s/.test(data[start - 1])) {
                    start--;
                }

                let end = offset;
                while (end < data.length && !/\s/.test(data[end])) {
                    end++;
                }

                return data.substring(start, end);
            }
        }
        return null;
    };

    var mouseX = parseInt("%{mouse_x}");
    var mouseY = parseInt("%{mouse_y}");
    const word = getWordAtPoint(mouseX, mouseY);
    return word;
})();
