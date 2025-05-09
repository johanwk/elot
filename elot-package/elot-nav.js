document.addEventListener("DOMContentLoaded", function() {
    // Move Table of Contents into a sidebar
    var toc = document.getElementById("table-of-contents");
    if (toc) {
        var sidebar = document.createElement("nav");
        sidebar.id = "elot-sidebar";
        sidebar.innerHTML = toc.innerHTML;
        toc.parentNode.removeChild(toc);
        document.body.appendChild(sidebar);

        // Clean up ToC: hide trailing Elot identifiers, remove section numbers below level 3,
        // and pad entries without children for alignment
        var tocLinks = sidebar.querySelectorAll("a");
        tocLinks.forEach(function(link) {
            // Remove trailing identifier in parentheses
            link.textContent = link.textContent.replace(/\s*\([^)]+\)\s*$/, '');

            var li = link.closest("li");

            // Determine depth by counting ancestor <ul> elements
            var depth = 0;
            var parent = li;
            while (parent !== sidebar && parent.parentNode) {
                if (parent.parentNode.tagName === "UL") {
                    depth++;
                }
                parent = parent.parentNode;
            }

            // For depth >= 3, replace the leading section number with thin spaces
            if (depth >= 3) {
                link.textContent = link.textContent.replace(/^\s*([\d.]+)\s+/, function(match, p1) {
                    var spaceCount = Math.max(1, p1.length - 4);
                    var thinSpaces = '\u2009'.repeat(spaceCount);
                    return thinSpaces + ' ';
                });
            }

            // If this <li> has NO sublist, add a spacer to align with toggle icons
            if (!li.querySelector("ul")) {
                var spacer = document.createElement("span");
                spacer.className = "toc-spacer";
                spacer.textContent = " "; // Unicode U+2003 EM SPACE (~width of ▶)
                link.parentNode.insertBefore(spacer, link);
            }
        });

        // Enhance sidebar: collapsible headings
        var sidebarListItems = sidebar.querySelectorAll("li");
        sidebarListItems.forEach(function(li) {
            var sublist = li.querySelector("ul");
            if (sublist) {
                li.classList.add("has-children");

                // Initial state: expand levels 1 and 2
                var depth = 1;
                var parent = li;
                while (parent !== sidebar && parent.parentNode) {
                    if (parent.parentNode.tagName === "UL") {
                        depth++;
                    }
                    parent = parent.parentNode;
                }

                if (depth <= 2) {
                    sublist.style.display = "block"; // expanded
                } else {
                    sublist.style.display = "none"; // collapsed
                }

                // Add a clickable icon to expand/collapse
                var toggle = document.createElement("span");
                toggle.className = "toc-toggle";
                toggle.textContent = depth <= 2 ? "▼ " : "▶ ";
                toggle.style.cursor = "pointer";
                toggle.addEventListener("click", function(e) {
                    e.preventDefault();
                    if (sublist.style.display === "none") {
                        sublist.style.display = "block";
                        toggle.textContent = "▼ ";
                    } else {
                        sublist.style.display = "none";
                        toggle.textContent = "▶ ";
                    }
                });

                li.insertBefore(toggle, li.firstChild);
            }
        });
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
