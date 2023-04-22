(function() {
    function getTextNodes(node, nodes = []) {
        if (node.nodeType === Node.TEXT_NODE &&
            node.textContent.trim() &&
            !node.textContent.trim().startsWith('.') &&
            !node.textContent.trim().startsWith('!') &&
            !node.textContent.trim().startsWith('__') &&
            !node.textContent.trim().startsWith('(') &&
            !node.textContent.trim().startsWith('{') &&
            !node.textContent.trim().startsWith('[')) {
            nodes.push(node);
        } else {
            for (const child of node.childNodes) {
                getTextNodes(child, nodes);
            }
        }
        return nodes;
    }

    function isNumeric(str) {
        return /^\d+$/.test(str);
    }

    function addTranslations() {
        const textNodes = getTextNodes(document.body);
        var pageUrl = window.location.href;

        let index = 0;
        let nodeTexts = [];
        for (const textNode of textNodes) {
            const textContent = textNode.textContent;

            if (pageUrl.startsWith("https://www.reddit.com") &&
                (isNumeric(textContent) ||
                    textContent.startsWith("/r/") ||
                    textContent.startsWith("/u/") ||
                    textContent.startsWith("r/") ||
                    textContent.startsWith("u/") ||
                    textContent.startsWith("level ") ||
                    textContent.endsWith(" ago") ||
                    (["Give Award", "Share", "Reply", "Report", "Save", "Follow"].includes(textContent)) ||
                    (textNode.className && textNode.className.includes("button")) ||
                    (textNode.className && textNode.className.includes("icon-comment"))
                )) {
                continue;
            }

            const translatedText = "eaf-translated-node-" + index;
            const translatedTextNode = document.createTextNode(translatedText);
            const translatedSpan = document.createElement("span");
            translatedSpan.appendChild(translatedTextNode);
            translatedSpan.classList.add("eaf-translated");
            translatedSpan.classList.add(translatedText);

            const originalParent = textNode.parentNode;
            originalParent.insertBefore(translatedSpan, textNode.nextSibling);

            nodeTexts.push(textContent);

            console.log(textContent);

            index++;
        }

        return nodeTexts;
    }

    return addTranslations();
})();
