// Element matches a selector
export function matches(element, selector) {
    const prototype = { Element };

    function match() {
        return Array.from(document.querySelectorAll(selector)).includes(this);
    }

    const matches =
        prototype.matches ||
        prototype.webkitMatchesSelector ||
        prototype.mozMatchesSelector ||
        prototype.msMatchesSelector ||
        match;

    return matches.call(element, selector);
}

export default {};
