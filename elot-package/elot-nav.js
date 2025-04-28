document.addEventListener("DOMContentLoaded", function() {
    // Move Table of Contents into a sidebar
    var toc = document.getElementById("table-of-contents");
    if (toc) {
        var sidebar = document.createElement("nav");
        sidebar.id = "elot-sidebar";
        sidebar.innerHTML = toc.innerHTML;
        toc.parentNode.removeChild(toc);
        document.body.appendChild(sidebar);
    }

    // Add copy-link buttons to all headings h1-h15
    var headings = document.querySelectorAll(
        "h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15"
    );
    headings.forEach(function(heading) {
        if (heading.id && heading.id.includes(":")) { // Only resources with CURIE-style IDs
            var button = document.createElement("button");
            button.className = "copy-link-button";
            button.textContent = "🔗";
            button.title = "Copy link";

            button.addEventListener("click", function(event) {
                event.preventDefault();
                const link = window.location.href.split("#")[0] + "#" + heading.id;
                navigator.clipboard.writeText(link).then(function() {
                    button.textContent = "✅";
                    setTimeout(function() {
                        button.textContent = "🔗";
                    }, 1000);
                });
            });

            heading.appendChild(button);
        }
    });
});
