(function() {
    function getTextNodes(node, nodes = []) {
        if (isHeaderOrFooter(node)) {
        } else if (isHideNode(node)) {
        } else if (node.nodeType === Node.TEXT_NODE && node.textContent.trim()) {
            nodes.push(node);
        } else if (isAParagraphNode(node)) {
            nodes.push(node);
        } else {
            for (const child of node.childNodes) {
                getTextNodes(child, nodes);
            }
        }
        return nodes;
    }

    function isHeaderOrFooter(node) {
        if (!node || !node.classList) {
            return false;
        }

        return node.classList.contains('header') || node.classList.contains('footer');
    }

    function isHideNode(node) {
        if (!node || !node.classList) {
            return false;
        }

        var pageUrl = window.location.href;
        if (pageUrl.startsWith("https://www.reddit.com")) {
            if ((node.hasAttribute("data-adclicklocation") && node.getAttribute("data-adclicklocation") === "top_bar") ||
                (node.hasAttribute("data-testid") && node.getAttribute("data-testid") === "post-comment-header") ||
                node.id === 'CommentSort--SortPicker'
            ) {
                return true;
            }
        }

        return false;
    }

    // This node is a P node, and all its child nodes are EM, CODE, A nodes,
    // so as to prevent em nodes from causing newlines in translation.
    function isAParagraphNode(node) {
        return node.tagName === 'P'
    }

    function isNumeric(str) {
        return /^\d+$/.test(str);
    }

    function checkString(input) {
        const regex = /^[\d.,!?;:-\s]+$/
        return regex.test(input);
    }

    function getPageNodes() {
        var pageUrl = window.location.href;
        if (pageUrl.startsWith("https://github.com")) {
            if (document.querySelector("readme-toc")) {
                return getTextNodes(document.querySelector("readme-toc"));
            } else if (document.querySelector(".application-main")) {
                return getTextNodes(document.querySelector(".application-main"));
            } else {
                return getTextNodes(document.body);
            }
        } else if (pageUrl.startsWith("https://www.reddit.com")) {
            if (document.querySelector('[data-testid="post-container"]')) {
                var containerNode = document.querySelector('[data-testid="post-container"]');
                return getTextNodes(containerNode.parentNode);
            } else {
                return getTextNodes(document.body);
            }
        } else {
            return getTextNodes(document.body);
        }
    }

    function addTranslations() {
        const textNodes = getPageNodes();
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
                textNode.parentNode.tagName === 'CODE' ||
                textNode.parentNode.tagName === 'PRE' ||
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
