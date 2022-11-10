(function() {
  var pageUrl = window.location.href;

  if ((window.innerHeight + window.scrollY) >= document.body.offsetHeight - 1) {
    if (pageUrl.startsWith("https://www.google.com")) {
      var pageIndexes = Array.from(document.getElementsByTagName("td"));
      var pages = pageIndexes.filter(index => index.children.length > 0);
      var pageTypes = pages.map(page => page.children[0].tagName);
      
      var currentPageIndex = pageTypes.lastIndexOf("SPAN");
      
      if (currentPageIndex < pages.length - 2) {
        return pages[currentPageIndex + 1].children[0].href;
      }
    } else if (pageUrl.startsWith("https://www.bing.com")) {
      var pageIndexes = Array.from(document.querySelectorAll(".sb_bp"));
      var pageUrls = pageIndexes.map(page => page.href);
      
      var currentPageIndex = pageUrls.lastIndexOf("");
      
      if (currentPageIndex < pageUrls.length - 2) {
        return pageUrls[currentPageIndex + 1];
      }
    }
  }
  
  return "";
})();
