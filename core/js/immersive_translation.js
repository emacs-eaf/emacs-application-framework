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

    function checkString(input) {
        const regex = /^[\d.,!?;:-\s]+$/
        return regex.test(input);
    }

    function addTranslations() {
        const textNodes = getTextNodes(document.body);
        var pageUrl = window.location.href;

        let index = 0;
        let nodeTexts = [];
        for (const textNode of textNodes) {
            const textContent = textNode.textContent;
            const text = textContent.trim();

            if (text.length === 0 ||
                text.length === 1 ||
                checkString(textContent) ||
                (["nil"].includes(text)) ||
                textNode.parentNode.tagName === 'BUTTON') {
                    continue;
                }

            if (pageUrl.startsWith("https://www.reddit.com") &&
                (isNumeric(textContent) ||
                    textContent.startsWith("/r/") ||
                    textContent.startsWith("/u/") ||
                    textContent.startsWith("r/") ||
                    textContent.startsWith("u/") ||
                    textContent.startsWith("level ") ||
                    textContent.endsWith(" ago") ||
                    (["give award", "award", "share", "reply", "cc", "comment as", "posted by", "op",
                        "report", "save", "follow"].includes(text.toLowerCase())) ||
                    (textNode.className && textNode.className.includes("button")) ||
                    (textNode.className && textNode.className.includes("icon-comment"))
                )) {
                    continue;
                }

            const translatedText = "eaf-translated-node-" + index;
            const translatedTextNode = document.createTextNode("");
            const translatedNode = document.createElement("div");

            translatedNode.appendChild(translatedTextNode);
            translatedNode.classList.add("eaf-translated");
            translatedNode.classList.add(translatedText);

            textNode.after(translatedNode);

            nodeTexts.push(textContent);

            index++;
        }
        
        console.log("##### ", nodeTexts);

        return nodeTexts;
    }

    return addTranslations();
})();
