# ELOT — Literate Ontology Engineering

[![MELPA Stable](https://melpa.org/packages/elot-badge.svg)](https://melpa.org/#/elot) [![VS Code Extension Version](https://badgen.net/vs-marketplace/v/johanwk.elot/latest?icon=visualstudio&color=blue&label=VS+Code)](https://marketplace.visualstudio.com/items?itemName=johanwk.elot)

*A single plain-text document that is simultaneously your ontology source, its documentation, its query workspace, and its diagram generator.*

## Why ELOT?

ELOT enables **literate ontology engineering**: a workflow, inspired by Knuth's _literate programming_, in which a single [Org Mode](https://orgmode.org/) plain-text file is the authoritative source for both ontology and documentation.
Headlines are the taxonomy; description lists are the axioms and annotations.
Documentation lives alongside the formal content, and HTML or PDF export captures both.
SPARQL queries and rdfpuml diagrams execute in place, so the document is also a workspace.
Because the source is ordinary plain text, ontology changes produce clean, human-readable diffs under version control — and, for that same reason, the format is naturally consumable by large language models, making ELOT well suited to AI-assisted authoring.

ELOT has been used across scores of ontology projects, including the ISO 23726-3 Industrial Data Ontology.
In collaborative standards work, having a single source for ontology and documentation is a decisive practical advantage.

## The ELOT file format

An ELOT file is a text document — readable by anyone —
to which ELOT assigns ontological meaning through a few
structures familiar from the Org format:

-   **Headlines are the taxonomy.** Outline headings declare classes,
    properties, and individuals; their nesting expresses the
    subclass/subproperty hierarchy.
-   **Description lists are the axioms and annotations.** A list item
    `- key :: value` attaches an annotation (`- rdfs:label :: ...`) or an
    OWL axiom in Manchester Syntax (`- SubClassOf :: ...`) to the entity
    under whose headline it appears.

A small excerpt — Dog is nested under Animal (so `ex:dog rdfs:subClassOf
ex:animal` follows from the structure), while the description-list items
add a definition and a Manchester-Syntax restriction:

```org
*** Animal (ex:animal)
 - iof-av:naturalLanguageDefinition :: A living organism
 - skos:example :: mouse, elephant
**** Dog (ex:dog)
 - iof-av:naturalLanguageDefinition :: A domesticated carnivorous mammal
 - SubClassOf :: ex:chases some ex:cat
```

Because this is *just text*, the very same file is read and
written by every part of the ELOT ecosystem: the Emacs package, the VS
Code extension, the editor-independent `elot-cli`, and — 
the large language models people use to draft and review ontologies. The
format, not any single tool, is what you commit to. Dedicated ELOT
tooling for LLM-assisted authoring is under active development; in the
meantime the plain-text source already pairs well with any chat-based or
in-editor AI assistant.

## Two ways to use ELOT: Emacs and VS Code

ELOT was developed as an Emacs package, and is now also a Visual Studio
Code extension, published on the [Visual Studio
Marketplace](https://marketplace.visualstudio.com/items?itemName=johanwk.elot). Both
editors operate on the same `.org` files:

-   **Emacs** — install from [MELPA](https://melpa.org/#/elot), or clone
    the repository and `(require 'elot-mode)`. This is the reference
    implementation and the maintainer's daily driver, so it stays a
    little ahead: SPARQL querying and rdfpuml diagram generation run
    in-buffer here today.
-   **VS Code** — search for "Elot" in the Extensions panel, or
    install from the
    [Marketplace](https://marketplace.visualstudio.com/items?itemName=johanwk.elot).
    The extension covers the core import, authoring, reading, and export loop
    — Org→OWL, label display, folding, go-to-definition, IntelliSense,
    and HTML export. See [extension README](tools/elot-cli/README.md)
    for keybindings, settings, and screenshots.

| | **VS Code** | **Emacs** |
|---|---|---|
| **Install** | [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=johanwk.elot) | [MELPA](https://melpa.org/#/elot), or clone + `(require 'elot-mode)` |
| **Org→OWL** | ✅ Built-in (WASM) | ✅ Built-in (Elisp) |
| **OWL→Org** | ✅ elot-exporter JAR | ✅ elot-exporter JAR |
| **Label display** | ✅ F5 toggle | ✅ Menu / opt-in F5 |
| **Global label display** | ✅ Any file via shared DB | ✅ Any buffer via shared DB |
| **Folding** | ✅ Tab / gutter | ✅ Native Org cycling |
| **Go to definition** | ✅ F12 / Ctrl+Click | ✅ M-. (xref) |
| **IntelliSense** | ✅ Ctrl+Space | ✅ `completing-read` |
| **SPARQL queries** | — (planned) | ✅ `org-babel` |
| **Diagrams** | — (planned) | ✅ rdfpuml integration |
| **HTML export** | ✅ Built-in (Pandoc) | ✅ `org-export` |

For automation outside any editor, `elot-cli` offers Org→OWL
conversion and HTML export from the command line, handy in CI
pipelines.

## Main features

-   **Org→OWL pipeline** — Generate OWL Manchester Syntax directly
    from the Org source (and, via OBO ROBOT, Turtle).
-   **Content derived from structure** — Ontology content comes
    directly from Org headlines and description lists. A single file
    may declare multiple ontologies.
-   **Readable label display** — Show human-readable labels instead of
    opaque CURIEs, with an in-buffer toggle in both editors. This
    works in any file — Turtle, SPARQL, CSV, source code, log files —
    drawing on a shared SQLite index that fills up silently as you
    work (Emacs: the `elot-global-label-display-mode` minor mode and
    `elot-db`; VS Code: DB-backed hover and F5 in non-Org files).
    BCP-47 language preferences are honoured. See
    [README-global-label-display.org](README-global-label-display.org)
    for the Emacs guide.
-   **In-place SPARQL and diagrams** — Use the existing Org support
    for SPARQL `SELECT`/`CONSTRUCT` queries, and render
    rdfpuml/PlantUML diagrams directly in the document (Emacs; planned
    for VS Code).
-   **Documentation export** — Produce styled, cross-linked HTML that
    captures both the prose and the formal content, with clickable
    CURIE links and stable anchors. With Emacs or Pandoc, a range of
    formats can be generated.
-   **Import existing ontologies** — `elot-exporter` (Java/OWLAPI)
    converts any existing OWL ontology into the ELOT Org format so you
    can use ELOT for ontologies you already have. Download from
    [releases](https://github.com/johanwk/elot/releases); source in
    [`tools/elot-exporter/`](tools/elot-exporter/).
-   **Editor-independent CLI** — `elot-cli` (TypeScript/WASM) provides
    the Org→OWL pipeline and HTML export outside any editor, plus a `db`
    sub-command that manages the shared label index. Source in
    [`tools/elot-cli/`](tools/elot-cli/).

## Getting Started with VS Code

1. **Install the extension** from the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=johanwk.elot)
2. **Open an `.org` ontology file** — try the examples in [`tools/elot-cli/examples/`](tools/elot-cli/examples/)
3. **Hover** over a CURIE to see labels and annotations
4. **Press F5** to toggle visual label display
5. **Press Tab** on a heading to fold/unfold
6. **Press F12** on a CURIE to jump to its definition
7. **Press Ctrl+Space** to search and insert entities

### Importing an existing OWL ontology

Note: **Java 21 or newer is required for `elot-exporter`.** Many
enterprise laptops ship with an older Java (8, 11, or 17); check with
`java -version` and upgrade if needed.


1. Open the Command Palette (`Ctrl+Shift+P`) and run **Elot: Import OWL Ontology**.
2. Choose a local file or paste a URL (e.g. a published ontology IRI).
3. The extension downloads `elot-exporter.jar` automatically and converts the OWL file into ELOT's Org format. 
4. Save the resulting `.org` file; every ELOT feature activates as soon as it is saved.

For more details, see [extension README](tools/elot-cli/README.md).

---

## Getting Started with Emacs

### Install ELOT in Emacs

The quickest way is from [MELPA](https://melpa.org/#/elot): once MELPA
is in your `package-archives`, run `M-x package-install RET elot RET`,
then add `(require 'elot-mode)` to your init file. With `elot-mode`
turned on, check out the ELOT menu.

ELOT is in active development, and is the maintainer's daily driver, so
the Git version stays a little ahead of the MELPA release. If you want
the newest features (or to contribute), *clone* the repository instead.

#### Install ELOT auxiliaries

-   Install [Java](https://www.java.com/en/download/help/download_options.html) to enable advanced features
 -   Turtle output, ontology metrics, and more: install [ROBOT](http://robot.obolibrary.org/)
 -   Ontology diagrams: install [PlantUML](https://plantuml.com/) and [rdfpuml](https://github.com/VladimirAlexiev/rdf2rml)
 -   Open OWL files: Download `elot-exporter` from [releases](https://github.com/johanwk/elot/releases)



### Quick start using ELOT

[Video.webm](https://github.com/user-attachments/assets/71a5ac3e-cafa-4180-b2a4-dbbb425b16c3)


<a id="orgf808072"></a>

#### Adding an ontology

Open the familiar Pizza ontology from `elisp/elot/examples/pizza.org`
for an example of what an ELOT file will look like. Check the menu bar
to locate the ELOT menu.

To create a new ontology, do this in Emacs:

-   Select a directory that you want to work in. Open a new Org Mode
    file, for instance `myontology.org`.
-   Insert the ELOT template for a document header.
    From the bottom of the ELOT menu, select *Insert New Ontology Document Header*.
    Answer the prompts, and a header is produced, like the following.
    
        #+title: My new ontology
        #+subtitle: An OWL ontology
        #+author: John Doe
        #+date: WIP (version of 2024-04-04 12:34)
-   Insert the ELOT template for an ontology skeleton: menu entry
    *Insert New Ontology Skeleton*. Answer the prompts — be
    ready to provide namespaces/prefixes for your ontology and the
    resources it will contain. You should see something like this
    screenshot:

![img](./documentation/images/elot-skeleton1.png)

Now create an OWL file from your new document.

-   Menu entry *Export to OWL* will "tangle" the ontology to a file
    in OWL Manchester Syntax, e.g., with filename
    `myontology.omn`.
-   If you have installed ROBOT, a Turtle file named `myontology.ttl`
    will also be generated.
-   Have Protégé ready, and open the ontology file to inspect it.

![img](./documentation/images/protege-skeleton1.png)

-   Export to an HTML document with menu entry *Export to HTML*. The document
    will open in your web browser.

![img](./documentation/images/firefox-skeleton1.png)


<a id="org2619c43"></a>

#### Adding classes and relations

-   Navigate to the `Classes` heading
-   Select *Insert Primitive/Defined Class template* from the menu to
    declare a class. Some appropriate annotation properties from the
    [Industrial
    Ontology Foundry Annotation Vocabulary (IOF-AV)](https://spec.industrialontologies.org/iof/ontology/core/meta/AnnotationVocabulary/) are included.
-   Navigate to the `Object properties` heading
-   Select *Insert Property template* for an object, data, or annotation
    property.
-   The screenshot shows how checkboxes are included for tracking
    progress. These are completely optional and won't appear in output.

![img](documentation/images/elot-animal1.png)


<a id="orgb39dc69"></a>

#### Adding annotations

ELOT makes it easy to add annotations to both annotations and axioms: 
just put them in a description list (` - term :: value`).
In this screenshot, two annotations are added to the "transitive" characteristic axiom:

![img](documentation/images/elot-annotate-axiom1.png)


<a id="org8c7cf45"></a>

#### Querying the ontology

-   Navigate to the `Prefixes` heading and insert a new heading named
    "Queries".
-   Select menu item *Insert SPARQL Select Block*, then write a query over
    the ontology.
-   Hit `C-c C-c` to run the query.

![img](documentation/images/elot-query1.png)


<a id="org33527b7"></a>

#### Making a diagram

-   A SPARQL *construct* query will produce Turtle code for a
    diagram. Select *Insert SPARQL Construct Block*.
-   Write a query and hit `C-c C-c` to run it.

![img](documentation/images/elot-query2.png)

-   Select *Insert RDFPUML Diagram Block*.
-   When prompted, provide the name of the Turtle source (the name of
    the construct query) and a figure caption.
-   Place the cursor on the row of the  `#+call:` and hit `C-c C-c`. A
    diagram is generated.

![img](documentation/images/elot-rdfpuml1.png)

-   Select *Export to HTML* to view your ontology in a browser.

![img](documentation/images/firefox-diagram1.png)


<a id="org8075a93"></a>

#### Display labels instead of identifiers

ELOT can display readable labels instead of prefixed identifiers
(which are unreadable if the identifiers are not informative), 
and offers quick search across the ontology resources.  Toggle
from the *ELOT* menu or with `M-x elot-toggle-label-display`.

ELOT does not claim a default keystroke for the toggle (single
function keys F5–F9 and `C-c <letter>` are reserved for the
user).  To get a quick keybinding, customize
`elot-toggle-labels-key` (`M-x customize-variable RET
elot-toggle-labels-key`) to a `kbd`-notation string of your
choice — for example `<f5>`, `<f9>`, or `C-c t l`.  When set,
ELOT installs the binding in `elot-mode-map` and in the
ELOT-managed `*xref*` / `*ELOT Describe*` buffers.

![img](documentation/images/elot-label-display1.png)


### Global label-display: labels everywhere, automatically

The minor mode `elot-global-label-display-mode` renders readable
labels in *any* buffer — `.ttl` files, SPARQL queries, CSV exports,
source code and log files. Toggle from the *ELOT* menu or via `M-x
elot-toggle-label-display` (or your chosen keystroke if you set one —
see `elot-toggle-labels-key`).

**id/label mappings are collected silently and automatically as you
edit ELOT Org files**, into a persistent SQLite index (`elot-db`) that
lives across sessions. The more ontologies you touch, the richer the
index becomes. Default sources  can be registered per project via
`.dir-locals.el`.

### Navigating Ontologies with Xref

ELOT supports cross-referencing and navigation using Emacs's built-in
`xref` system. This allows users to find where a resource is used
and to jump directly to its definition from anywhere in an ELOT buffer.

To find all references to a CURIE (e.g., `:BFO_0000015`), place the cursor
on it and type `M-?` (`xref-find-references`). The `*xref*` buffer will open
and show each occurrence, along with the heading and flattened description
list entry for context.

Inside the `*xref*` buffer:

-   Press `RET` to jump to the occurrence.
-   Move the cursor to any other CURIE and press `M-.` (`xref-find-definitions`)
    to jump to its definition (typically the Org headline where it's declared).

Label overlays are enabled automatically in the `*xref*` buffer, so
identifiers appear with readable labels if available. This makes it easier
to explore large ontologies interactively.

This functionality is activated automatically for Org files when `elot-mode` is active.


### HTML style

HTML output from ELOT produces stable anchors for ontology resources,
and shows readable labels with internal, clickable links.
![img](documentation/images/elot-html-format.png)

---

## Tools

Two command-line tools are included under the [`tools/`](tools/) directory:

-   **[elot-exporter](tools/elot-exporter/README.md)** — A Java CLI (OWLAPI + Maven) that converts existing OWL
    ontologies into ELOT's Org-mode format, producing a shaded JAR.

-   **[elot-cli](tools/elot-cli/README.md)** — A TypeScript CLI and VS Code extension that generates OWL
    Manchester Syntax and HTML documentation from Org-mode files (uses orgize WASM + esbuild + Pandoc).
    Also provides a `db` sub-command (`elot-cli db init|register|refresh|list|lookup|attr|remove`)
    that manages the persistent SQLite label index used by the global label-display
    feature; see [the `Label Database` section in the elot-cli README](tools/elot-cli/README.md#label-database-elot-cli-db).
    **Available on the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=johanwk.elot).**
