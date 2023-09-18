document.querySelector('.navframe').addEventListener('load', function() {
    // Get the current page URL without the suffix after #
    var currentPageURL = window.location.href.split('#')[0];

    // Get all detail elements
    var as = document.querySelector('.navframe').contentWindow.document.body.querySelectorAll('a');
    for (const a of as) {
        if (a.href) {
            var href = a.href.split('#')[0];
            if (href === currentPageURL) {
                a.style.fontStyle = 'italic';
                var el = a.parentNode.closest('details');
                while (el) {
                    el.open = true;
                    el = el.parentNode.closest('details');
                }}}}
});
