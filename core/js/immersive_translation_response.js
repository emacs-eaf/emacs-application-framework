(function() {
    let translates = %1;

    const elements = document.getElementsByClassName("eaf-translated");

    for (let i = 0; i < elements.length; i++) {
        const classNames = elements[i].classList;

        const targetClassName = Array.from(classNames).find((className) =>
            className.startsWith("eaf-translated-node-")
        );

        if (targetClassName) {
            const index = parseInt(targetClassName.split("-")[3]);

            let translatedNode = elements[i];

            translatedNode.style.display = 'block';
            translatedNode.style.whiteSpace = 'pre-wrap';
            translatedNode.style.borderLeft = '4px solid #C53D56 !important';
            translatedNode.style.paddingLeft = '12px !important';
            translatedNode.style.marginTop = '4px';
            translatedNode.style.marginBottom = '4px';
            translatedNode.style.paddingTop = '4px';
            translatedNode.style.paddingBottom = '4px';

            translatedNode.innerHTML = translates[index];
        }
    }

    console.log("***** ", translates);
})();
