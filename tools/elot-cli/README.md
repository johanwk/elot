# Elot: Org-mode Ontology Editor for VS Code

**Literate ontology engineering in Visual Studio Code.** Write OWL ontologies as
plain-text Org-mode files — with label display, IntelliSense, syntax checking,
folding, and one-click OWL export.

ELOT (Emacs Literate Ontology Tool) is an established workflow for ontology
engineering in which a single [Org-mode](https://orgmode.org/) document is
simultaneously your ontology source, its documentation, and your analytical
workspace. This extension brings the core ELOT experience to VS Code — no Emacs
required.

## Features at a Glance

| Feature | Shortcut |
|---|---|
| **Org→OWL export** | Right-click → *Tangle to Manchester Syntax* |
| **Import OWL ontology** | Command Palette → *Elot: Import OWL Ontology* |
| **Label display** (hover + toggle) | **F5** (`.org` and other files) |
| **Headline folding** | **Tab** / **Shift+Tab** |
| **Go to definition** | **F12** / **Ctrl+Click** |
| **IntelliSense completion** | **Ctrl+Space** |
| **Org Indent Mode** | **Ctrl+Shift+I** |
| **Export to HTML** | Right-click → *Export to HTML* (requires Pandoc) |
| **OWL axiom syntax checking** | Automatic (red squiggly underlines) |
| **Bold headlines & fontified description lists** | Always on |

## Getting Started

1. **Install the extension** — search for "Elot" in the Extensions panel, or
   install from the
   [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=johanwk.elot).
2. **Open an `.org` ontology file** — try the
   [example files](https://github.com/johanwk/elot/tree/master/tools/elot-cli/examples)
   from the repository.
3. **Hover** over a CURIE to see its label and annotations.
4. **Press F5** to toggle visual label display.
5. **Press Tab** on a heading to fold/unfold.
6. **Press F12** on a CURIE to jump to its definition.
7. **Press Ctrl+Space** to search and insert entities.

### Importing existing OWL ontologies

Have an existing ontology in RDF/XML, Turtle, or another OWL format? The
extension can convert it to ELOT's Org-mode format directly from VS Code — no
command line needed.

1. Open the **Command Palette** (`Ctrl+Shift+P`) and run **Elot: Import OWL
   Ontology**.
2. Choose whether to **enter a URL** or **browse for a local file**.
3. The extension runs `elot-exporter.jar` (downloaded automatically on first
   use) and converts the ontology to Org-mode.
4. After conversion you are prompted for a **file location** to save the result
   with an `.org` extension. The default filename is derived from the top-level
   heading — for example, if the ontology's top-level heading is `* core`, the
   suggested filename is `core.org`.
5. The saved `.org` file opens immediately with **all ELOT features active**
   (label display, IntelliSense, syntax checking, etc.).

> **Why the save prompt?** VS Code features like hover, completion, and
> diagnostics are tied to the file extension. An unsaved buffer doesn't have an
> `.org` extension, so ELOT functions won't activate until the file is saved.
> If you cancel the save dialog the converted text still opens as an untitled
> buffer — just save it as `.org` later to enable ELOT.

#### Java runtime requirement

The OWL importer runs `elot-exporter.jar`, which is compiled for **Java 21**
and requires **Java 21 or newer** on your system. Many enterprise laptops ship
with older Java versions (8, 11, or 17), so a check before first use is
recommended:

```bash
java -version
```

If the major version is below 21, running the importer will fail with

```
UnsupportedClassVersionError: ... has been compiled by a more recent version
of the Java Runtime ... class file version 65.0
```

Install a current JDK from one of:

- [Eclipse Temurin](https://adoptium.net/) (free, open source)
- [Microsoft Build of OpenJDK](https://learn.microsoft.com/java/openjdk/)
- [Oracle JDK](https://www.oracle.com/java/technologies/downloads/)

On managed machines where the system Java cannot be replaced, install a
newer JDK alongside it and point the extension at it via the
**`elot.javaPath`** setting (absolute path to the `java` executable in the
new JDK's `bin/` directory). This avoids touching `JAVA_HOME` or `PATH` and
leaves other Java tools untouched.

The extension defaults `elot.javaPath` to `java` (first match on `PATH`).

You can also run the exporter from the command line:

```bash
java -jar elot-exporter.jar input-ontology.owl -o output.org
```

Download it from the
[releases page](https://github.com/johanwk/elot/releases).

---

## Label Display (CURIE → Human-readable Labels)

Ontology files are full of identifiers like `obo:BFO_0000001` that are hard to
read. ELOT can display human-readable labels instead.

### Hover Information

Hover over any CURIE (e.g. `obo:BFO_0000001`) or angle-bracket URI in an
`.org` file to see a hover card with:

- **Label** (bold) — e.g. *"entity"@en*
- `URI` (monospace)
- *Type* — e.g. `owl:Class`, `owl:ObjectProperty`
- Extra annotations — e.g. `skos:definition`, `rdfs:comment`

Always on by default. Disable via the setting `elot.labelDisplay.hoverEnabled`.

### Visual Label Replacement (Toggle)

Press **F5** (or click the 🏷 icon in the editor title bar, or the status bar
indicator, or right-click → *Elot: Toggle Label Display*) to visually replace
CURIEs with their labels throughout the editor.

When enabled:
- The underlying document text is **not modified** — CURIEs remain in the file
- The CURIE text is visually hidden and replaced by the label via CSS
  pseudo-elements
- Labels are shown in *italic* by default (configurable)
- Copy-paste still copies the real CURIEs
- The status bar shows **🏷 Labels** (highlighted) when ON, **`</>` CURIEs** when OFF

Toggle methods:

| Method | Location |
|---|---|
| **F5** | Keyboard shortcut (when `.org` file is active) |
| **🏷 icon** | Editor title bar (top-right) |
| **Status bar** | Bottom-right indicator (clickable) |
| **Right-click** | Context menu → *Elot: Toggle Label Display* |
| **Command Palette** | `Elot: Toggle Label Display` |

## Headline Folding

Org headings can be folded and unfolded, just like in Emacs Org-mode. Each
heading's fold region extends to just before the next sibling or ancestor
heading.

| Action | Method |
|---|---|
| **Toggle fold at cursor** | Press **Tab** on a heading line |
| **Fold at cursor** | `Ctrl+Shift+[` (`⌘+⌥+[` on Mac) |
| **Unfold at cursor** | `Ctrl+Shift+]` (`⌘+⌥+]` on Mac) |
| **Fold all** | `Shift+Tab` (when some headings are unfolded), or `Ctrl+K Ctrl+0` |
| **Unfold all** | `Shift+Tab` (when all folded), or `Ctrl+K Ctrl+J` |
| **Fold to level N** | `Ctrl+K Ctrl+N` (e.g. `Ctrl+K Ctrl+1` for level 1) |
| **Click gutter** | Click the ▸ / ▾ chevron in the left gutter next to a heading |

The Tab and Shift+Tab bindings are scoped to `.org` files, so they don't
interfere with normal Tab behaviour in other file types.

> **Note:** VS Code's fold toggle is two-state (folded ↔ unfolded), unlike
> Emacs Org-mode's three-state TAB cycling (folded → children → subtree).
> Shift+Tab provides the global overview/show-all toggle.

## Go to Definition (Jump to Entity Heading)

When you see a CURIE like `obo:BFO_0000001` anywhere in an Org file, you can
jump directly to the heading where that entity is declared — the VS Code
equivalent of Emacs's `M-.` (`xref-find-definitions`).

| Action | Method |
|---|---|
| **Go to definition** | **F12**, or **Ctrl+Click** (`Cmd+Click` on Mac) on a CURIE |
| **Peek definition** (inline preview) | **Alt+F12** |
| **Go back** | **Alt+←** (like Emacs `M-,`) |
| **Right-click** | Context menu → *Go to Definition* / *Peek Definition* |

Works purely within the current Org file — no external index or language server
required.

## Org Indent Mode (Visual Indentation)

Toggle visual indentation that mimics Emacs's `org-indent-mode`. When enabled,
sub-headings and their body text are indented proportional to their depth, and
leading stars on sub-headings are hidden — all without modifying the file.

| Headline level | Visual indent |
|---|---|
| `*` (level 1) | 0 (left margin) |
| `**` (level 2) | 2 spaces |
| `***` (level 3) | 4 spaces |
| Body text | Same as its parent heading |

| Action | Method |
|---|---|
| **Keyboard shortcut** | **Ctrl+Shift+I** (in an `.org` file) |
| **Command Palette** | *Elot: Toggle Org Indent Mode* |
| **Status bar** | Click the **Flat** / **Indent** indicator (bottom-right) |
| **Title bar** | Click the indent icon in the editor title area |
| **Right-click** | Context menu → *Elot: Toggle Org Indent Mode* |

## IntelliSense: Insert Existing Resource

Press **Ctrl+Space** to get a dropdown of all OWL entities declared in the
current Org file — searchable by label or CURIE.

| Field | Content |
|---|---|
| **Label** (main text) | Human-readable name, e.g. *"entity"@en* |
| **Description** (right-aligned) | CURIE, e.g. `obo:BFO_0000001` |
| **Detail** (subtitle) | RDF type, e.g. `owl:Class` |
| **Icon** | Class for `owl:Class`, Property for properties, Value for individuals |
| **Documentation panel** | Full detail: label, CURIE, type, and annotations |

Selecting an item inserts the **CURIE** into the document. You can also
auto-trigger by typing `:` after a prefix name (e.g. `obo:`). Items are grouped
by type (Classes → Properties → Individuals), then sorted alphabetically by
label.

## OWL Axiom Syntax Checking

The extension validates OWL Manchester Syntax axioms as you type. Any
description list item whose tag is an OMN keyword (`SubClassOf`,
`EquivalentTo`, `DisjointWith`, `Domain`, `Range`, `SubPropertyOf`,
`InverseOf`, `SubPropertyChain`, `Types`, `Facts`, `SameAs`,
`DifferentFrom`, etc.) is parsed against the OWL Manchester Syntax grammar.

If the axiom value is malformed, a red squiggly underline appears with an error
message. Diagnostics are updated on every save and when the file is opened.

| Keyword | Grammar rule |
|---|---|
| `SubClassOf`, `EquivalentTo`, `DisjointWith`, `Domain`, `Range`, `Types` | `ClassExpressionList` |
| `SubPropertyOf`, `InverseOf` | `ObjectPropertyExpressionList` |
| `SubPropertyChain` | `SubPropertyChain` |
| `Facts` | `Fact` |
| `SameAs`, `DifferentFrom` | `IndividualIRIList` |

Example:

```org
- SubClassOf :: obo:BFO_0000139 only (obo:BFO_0000015 or obo:BFO_0000035)
```

Errors are caught immediately — no need to load the ontology into a reasoner
first.

## Fontification

Two always-on decorations enhance readability:

- **Bold headlines** — All Org headings (`*`, `**`, `***`, …) are rendered in
  **bold**.
- **Subdued description list tags** — In description lists like
  `- rdfs:isDefinedBy :: lis-ont:core`, the tag portion (`- rdfs:isDefinedBy ::`)
  is shown in a muted colour with italic styling, so the value stands out.

Applied automatically — no toggle or configuration needed.

## Settings

| Setting | Default | Description |
|---|---|---|
| `elot.labelDisplay.fontStyle` | `"italic"` | Font style for labels: `italic`, `normal`, or `oblique` |
| `elot.labelDisplay.hoverEnabled` | `true` | Show hover cards for CURIEs in `.org` files |
| `elot.javaPath` | `"java"` | Path to the Java executable (Java 21+ required for OWL import) |

DB-backed (global) label display has its own settings — see
[Label Database](#label-database-elot-cli-db) below
(`elot.dbPath`, `elot.activeLabelSources`, `elot.preferredLanguages`,
`elot.globalLabelDisplay.*`).

Change in VS Code Settings (search for "elot").

## Tip: Word Wrap for Org Files

Org files often have long lines. Press **Alt+Z** to toggle word wrap, or add
this to your `settings.json`:

```json
"[org]": {
    "editor.wordWrap": "on"
}
```

---

## Label Database (`elot-cli db`)

ELOT can resolve identifiers (CURIEs and full URIs) to human-readable labels
across *any* file you open in VS Code — Turtle, SPARQL, CSV, source code, log
files — by consulting a persistent SQLite index. The index is populated and
managed exclusively by the `elot-cli db` sub-command; the VS Code extension
reads from it (read-only).

> **Status (2026-04):** the CLI write/query path and the VS Code read path
> (hover + visual label replacement + status bar + source management
> commands) are shipping. End-to-end smoke tests against a real Extension
> Host are the next milestone (Step 2.3.5b of the plan). Emacs (`elot-mode`)
> shares the same DB schema and binary file format — see
> [README-global-label-display.org](https://github.com/johanwk/elot/blob/master/README-global-label-display.org).

### Using the database from VS Code

Once you have populated a database with `elot-cli db register ...` (see
below), point the extension at it via settings, mark which sources should
be consulted, and the extension provides:

- **DB-backed hover** for CURIEs and IRIs in *non-Org* files (Turtle,
  SPARQL, JSON, source code, plain text, …). Shows the label, the
  resolved id, the `rdf:type`, definitions, comments, and a `[src: NAME]`
  provenance footer.
- **Visual label replacement (F5)** for non-Org files. Same idea as the
  Org-side F5 toggle: the document text is unchanged; CURIEs and IRIs
  are rendered with their labels via CSS pseudo-elements.
- **Status bar indicator** showing active-source count and total
  decoratable ids. Click it to toggle global label display. Hover for
  a per-source breakdown.
- **Command palette** access to the source-management surface
  (Activate / Deactivate / Reorder).

> The Org F5 (`elot.toggleLabels`) and the new global F5
> (`elot.toggleGlobalLabels`) coexist: F5 routes to the Org-side toggle
> in `.org` files and to the DB-backed toggle elsewhere.

#### Settings

| Setting | Default | Description |
|---|---|---|
| `elot.dbPath` | `""` | Absolute path to the SQLite DB. Empty = use the per-platform VS Code `globalStorage` location. Workspace-level overrides user-level. |
| `elot.activeLabelSources` | `[]` | Ordered list of active sources. Each entry is either a string (source name) or `{"source": "name", "dataSource": "path"}`. First match wins on lookup conflicts. |
| `elot.preferredLanguages` | `[]` | Preferred language tags (BCP-47), in priority order. Empty = `["", "en"]` (untagged first, then English). |
| `elot.globalLabelDisplay.hoverEnabled` | `true` | Show DB-backed hovers in non-Org files. |
| `elot.globalLabelDisplay.maxIds` | `500` | Per-file cap on how many CURIE/IRI tokens are decorated. Status bar shows a "capped" warning when reached. |
| `elot.globalLabelDisplay.includeLanguages` | `[]` | VS Code language IDs in which DB-backed hover / decoration apply. Empty = built-in default (`plaintext`, `markdown`, `typescript`, `javascript`, `python`, `json`, `yaml`, `xml`, `turtle`, `sparql`, …). Org is **always** excluded. |

`settings.json` accepts both the shorthand and the canonical form for
active sources:

```jsonc
{
  // Shorthand:
  "elot.activeLabelSources": ["bfo", "ro"],

  // Canonical (also written by the management commands):
  "elot.activeLabelSources": [
    { "source": "bfo", "dataSource": "" },
    { "source": "ro",  "dataSource": "" }
  ],

  "elot.preferredLanguages": ["en", ""]
}
```

> **Beware JSONC commas.** VS Code's `settings.json` parser is forgiving
> but a trailing comma after the last key/value can silently invalidate
> a setting. If a label-related setting "doesn't seem to take effect",
> open the file and look for a stray comma before the closing brace.

#### Commands (Command Palette)

| Command | Effect |
|---|---|
| `Elot: Toggle Global Label Display` (**F5** in non-Org files) | Show/hide DB-backed labels in the current editor. |
| `Elot: Activate Label Source` | Multi-select QuickPick of inactive sources; appends to `elot.activeLabelSources`. |
| `Elot: Deactivate Label Source` | Multi-select QuickPick of active sources; removes from `elot.activeLabelSources`. |
| `Elot: Reorder Active Label Sources` | Two-step QuickPick: pick a source, then move *up* / *down* / *top* / *bottom* / done. (Per-item button reorder lands in 2.3.6.) |
| `Elot: Label DB Info` | Diagnostic: shows the resolved DB path, bridge state, registered sources with id counts, and current settings. Useful as a first stop when something looks off. |

The settings round-trip preserves the original scope of the key (workspace
vs. user) and always writes the canonical `{source, dataSource}` shape so
the resulting `settings.json` is unambiguous after a UI edit.

#### Status bar

Sits in the bottom-right, next to the Org-side label-display indicator.
Format:

```
🏷 Labels (3 src, 12k ids)        ← display ON
</> CURIEs (3 src, 12k ids)        ← display OFF
🏷 Labels (3/5 src, 12k ids, capped) ← scan cap reached
$(database) ELOT: no DB            ← bridge has no file open
```

When active source count differs from total registered sources, the
format collapses to `(N/M src, K ids)`. Hover the indicator for a full
breakdown (path, per-source id counts, prefs).

### Database location

By precedence (extension):

1. Workspace-level `elot.dbPath`
2. User-level `elot.dbPath`
3. `$ELOT_DB_PATH` environment variable
4. `context.globalStorageUri.fsPath` + `/elot.sqlite`:
   - Windows: `%APPDATA%\Code\User\globalStorage\johanwk.elot\elot.sqlite`
   - macOS: `~/Library/Application Support/Code/User/globalStorage/johanwk.elot/elot.sqlite`
   - Linux: `~/.config/Code/User/globalStorage/johanwk.elot/elot.sqlite`

The CLI uses the same logical defaults (its `--db` flag wins, then
`$ELOT_DB_PATH`, then the same per-platform `globalStorage` location,
then `~/.elot/elot.sqlite` as a final fallback). When the publisher id
matches, the extension and the CLI converge on the same file.

The VS Code extension and Emacs each own their own DB by default — they do
not share state. Use `--db` or `$ELOT_DB_PATH` if you want a shared file.

### Commands

```bash
# Create / open the DB (idempotent; creates parent dirs)
elot-cli db init [--db <path>]

# Register a source (replaces any existing rows for the source name)
elot-cli db register <file> --source <name> [--type csv|tsv|json|ttl|rq|org]
                            [--data-source <path-or-url>]
elot-cli db refresh <name>  --file <file>   [--type ...]

# Inspect
elot-cli db list                       [--prefixes] [--format tsv|json|table]
elot-cli db lookup <label>             [--active]   [--format tsv|json]
elot-cli db attr   <id> [<prop>]                    [--format tsv|json]

# Remove a source (cascades to its entities, attributes, prefixes)
elot-cli db remove <name>
```

`<id>` accepts a literal stored id, a CURIE (expanded against the DB's
prefix table), or a full URI (contracted against the same table). All
read commands honour `$ELOT_PREFERRED_LANGUAGES` (BCP-47 tags,
comma-separated; the empty tag means "untagged").

### Source types

| Type   | Parser notes |
|--------|--------------|
| `csv`  | RFC-4180. First row is the header. Required column: `id`. Optional `label` and `lang`. Other columns become attributes. Header `label@en` is shorthand for a tagged label. |
| `tsv`  | Same as CSV with tab delimiter. |
| `json` | Two shapes: flat `[{id, label, lang?, ...attrs}, ...]` and nested `{prefixes?: {pfx: iri}, entities: [{id, label?, kind?, attrs?: {prop: value\|[v..]\|{lang: v}}}]}`. |
| `ttl`  | Runs ROBOT (`$ELOT_ROBOT_JAR` or `robot` on `PATH`) with a default `SELECT ?id ?label (LANG(?label) AS ?lang)` query. Override per-project: place `<root>/.elot/ttl-label-query.rq` (where `<root>` is the nearest ancestor with `.git`, `.elot`, or `.elot-cache`). Prefixes are harvested from the file's `@prefix` lines. |
| `rq`   | Executes a SPARQL `SELECT` query against `--data-source <ttl-file-or-http-endpoint>` via ROBOT. Results are cached in `<root>/.elot-cache/<query>.<sha1>.csv`; an empty result preserves any existing cache (defensive). Prefixes are harvested from the query's `PREFIX` lines. |
| `org`  | Reuses the orgize WASM parser + ELOT slurp builder. Supports the standard `:prefixdefs:` block, ID-suffix CURIE conventions, and language-tagged headings. |

Type is auto-detected from the file extension if `--type` is omitted.

### Worked example

```bash
# Initialise an empty DB
elot-cli db init --db /tmp/elot.sqlite

# Ingest a CSV with English and Korean labels
cat > /tmp/labels.csv <<'EOF'
id,label,lang
ex:widget,Widget,en
ex:widget,위젯,ko
ex:widget,Dings,de
EOF
elot-cli db register /tmp/labels.csv --db /tmp/elot.sqlite --source demo

# Lookup
elot-cli db lookup Widget   --db /tmp/elot.sqlite          # ex:widget
elot-cli db attr ex:widget rdfs:label --db /tmp/elot.sqlite  # Widget (en wins)

# Korean prefs
ELOT_PREFERRED_LANGUAGES=ko \
  elot-cli db attr ex:widget rdfs:label --db /tmp/elot.sqlite  # 위젯

# List in JSON
elot-cli db list --db /tmp/elot.sqlite --format json
```

### Schema

Schema version 3 only. Older databases are refused with a clear message
directing the user to upgrade via Emacs. The canonical DDL is shipped as
`elot-package/schema.sql` and consumed verbatim by both the Elisp writer
and the TS writer; a byte-identical golden round-trip test
(`test/fixtures/golden/`) keeps the two implementations in lockstep.

---

## About ELOT

ELOT introduces **literate ontology engineering**: a workflow where a single
Org-mode plain-text file is the authoritative source for an OWL ontology and its
documentation. Headlines are the taxonomy; description lists are the axioms and
annotations.

This approach has been used across scores of ontology projects, including the
ISO 23726-3 Industrial Data Ontology. See the
[ELOT repository](https://github.com/johanwk/elot) for the full project,
including the Emacs package, examples, and the `elot-exporter` tool.

## Org→OWL Pipeline

The extension includes a complete Org→OWL pipeline built on
[orgize](https://github.com/PoiScript/orgize) (a Rust Org-mode parser compiled
to WebAssembly). It supports:

- Heading-to-entity mapping (URIs, CURIEs, and labels from Org headings)
- Property drawer and keyword extraction
- Description list → OWL annotation/restriction conversion
- Prefix table management
- Ontology, Class, ObjectProperty, DataProperty, AnnotationProperty, and
  Individual frames
- Meta-annotations via nested description lists
- Automatic tangle to the file specified in `:header-args:omn: :tangle`

---

## Developer Documentation

The sections below are for contributors who want to build the extension from
source.

### Prerequisites

- [Node.js](https://nodejs.org/) ≥ 18
- [Rust](https://rustup.rs/) toolchain (for building the WASM parser)
- [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/) (`cargo install wasm-pack`)

### Setup (after cloning)

```bash
cd tools/elot-cli
npm install
```

### Building

```bash
# Full build (WASM + esbuild bundle):
npm run bundle
```

This runs two stages:

1. **`npm run build:wasm`** — runs `wasm-pack build --target nodejs` inside
   `elot-orgize/`, producing `src/wasm/elot_orgize.js` and
   `src/wasm/elot_orgize_bg.wasm`.

2. **`node esbuild.mjs`** — cleans `dist/`, copies the `.wasm` file, then
   bundles `src/extension.ts` → `dist/extension.js` and
   `src/cli.ts` → `dist/cli.js` via esbuild.

Generated files in `src/wasm/` and `dist/` are **gitignored** — run
`npm run bundle` after every fresh clone.

### CLI Usage

```bash
# Generate OMN, write to stdout
node dist/cli.js examples/bfo-core.org -

# Write to a specific file
node dist/cli.js examples/bfo-core.org output.omn

# Uses the :tangle target from the Org file, or stdout
node dist/cli.js examples/bfo-core.org

# Export to styled HTML (requires Pandoc on PATH)
node dist/cli.js --html examples/bfo-core.org

# Export to HTML with explicit output path
node dist/cli.js --html examples/bfo-core.org output.html
```

The `--html` flag exports to styled HTML via Pandoc instead of generating
Manchester Syntax. It applies the same CURIE linkification, label resolution,
and ELOT CSS/JS theming as the VS Code "Export to HTML" command. If no output
path is given, the `.org` extension is replaced with `.html`.

### VS Code Extension Packaging

```bash
npx @vscode/vsce package
```

Produces `elot-<version>.vsix` containing `dist/extension.js`, `dist/cli.js`,
`dist/elot_orgize_bg.wasm` (~284 KB), `package.json`, `LICENSE`, and this
README.

### Testing

```bash
npm test                                        # Golden-file tests
npx tsx src/tests/entityFromHeader.test.ts      # Unit tests
npx tsx src/tests/annotationValue.test.ts
npx tsx src/tests/omnKeywords.test.ts
```

### Rebuilding After Changes

| What changed | What to run |
|---|---|
| Rust code in `elot-orgize/src/` | `make bundle` (rebuilds WASM + re-bundles) |
| TypeScript in `src/` | `make bundle` (re-bundles) |
| Only type-checking | `make build` |
| Fresh `.vsix` | `make package` (runs bundle via prepublish) |
| Forgot the commands | `make help` |

### Architecture

```
Org text → [orgize WASM] → JSON AST → parseOrg() → ElotNode tree → generateOmn() → OMN
```

1. **`parseOrgWasm.ts`** — Calls orgize WASM, maps JSON to `ElotNode` tree
2. **`cli.ts`** — CLI entry point
3. **`extension.ts`** — VS Code extension entry point
4. **`generateOmn.ts`** — Assembles OMN from node tree
5. **`omnDeclarations.ts`** — Generates frames for each node
6. **`omnFrame.ts`** — Formats individual OWL frames

The Rust crate `elot-orgize/` contains `lib.rs` (WASM entry), `parse.rs`
(AST walker), and `types.rs` (data structures).
