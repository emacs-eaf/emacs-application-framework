(function() {
    let key = "%1";
    let newTab = %2;
    let markers = document.querySelectorAll('.marker');
    let match;
    for(let i = 0; i < markers.length; i++) {
        if(markers[i].getAttribute('key') === key.toUpperCase()) {
            match = markers[i];
            break;
        }
    }
    if(match != undefined){
        let selector = match.getAttribute('pointed-link');
        let link = document.querySelector(selector);
        if(link.href != undefined){
            if(newTab){
                window.open(link.href);
            } else {
                window.open(link.href, '_self');
            }
        }else if(link.nodeName.toLowerCase() === 'input'){
            if(link.getAttribute('type') === 'text'){
                link.focus();
                link.select();
            } else if(link.getAttribute('type') === 'submit'){
                link.submit();
            } else if(link.getAttribute('type') === 'checkbox'){
                link.click();
            }
        } else if(link.nodeName.toLowerCase() === 'button'){
            link.click();
        }
    }
})()
