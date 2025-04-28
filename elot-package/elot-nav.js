document.addEventListener("DOMContentLoaded", function() {
    var toc = document.getElementById("table-of-contents");
    if (toc) {
        // Create sidebar nav
        var sidebar = document.createElement("nav");
        sidebar.id = "elot-sidebar";

        // Move existing ToC inside sidebar
        sidebar.innerHTML = toc.innerHTML;

        // Remove original ToC container
        toc.parentNode.removeChild(toc);

        // Insert sidebar
        document.body.appendChild(sidebar);
    }
});
