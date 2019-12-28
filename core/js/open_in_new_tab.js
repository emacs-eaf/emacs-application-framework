(function() {
    let e = document.elementFromPoint(%1, %2);
    if (!e)
        return;
    function isMediaElement(e) {
        return e.tagName == 'AUDIO' || e.tagName == 'VIDEO';
    };
    function isEditableElement(e) {
        if (e.isContentEditable)
            return true;
        if (e.tagName === 'INPUT' || e.tagName === 'TEXTAREA')
            return e.getAttribute('readonly') != 'readonly';
        return false;
    };
    function isSelected(e) {
        let selection = window.getSelection();
        if (selection.type !== 'Range')
            return false;
        return window.getSelection().containsNode(e, true);
    };
    let res = {
        baseUrl: document.baseURI,
        alternateText: e.getAttribute('alt'),
        boundingRect: '',
        imageUrl: '',
        contentEditable: isEditableElement(e),
        contentSelected: isSelected(e),
        linkTitle: '',
        linkUrl: '',
        mediaUrl: '',
        tagName: e.tagName.toLowerCase()
    };
    let r = e.getBoundingClientRect();
    res.boundingRect = [r.top, r.left, r.width, r.height];
    if (e.tagName == 'IMG')
        res.imageUrl = e.getAttribute('src');
    if (e.tagName == 'A') {
        res.linkTitle = e.text;
        res.linkUrl = e.getAttribute('href');
    }
    while (e) {
        if (res.linkTitle === '' && e.tagName === 'A') {
            res.linkTitle = e.text;
            if(res.linkUrl === '') {
                res.linkUrl = e.getAttribute('href');
            }
        }
        if (res.mediaUrl === '' && isMediaElement(e)) {
            res.mediaUrl = e.currentSrc;
            res.mediaPaused = e.paused;
            res.mediaMuted = e.muted;
        }
        e = e.parentElement;
    }
    return res;
})()
