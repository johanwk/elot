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
| **Label display** (hover + toggle) | **F5** |
| **Headline folding** | **Tab** / **Shift+Tab** |
| **Go to definition** | **F12** / **Ctrl+Click** |
| **IntelliSense completion** | **Ctrl+Space** |
| **Org Indent Mode** | **Ctrl+Shift+I** |
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

### Converting existing OWL files

Have an existing ontology in RDF/XML, Turtle, or another format? Download
**elot-exporter** from the
[releases page](https://github.com/johanwk/elot/releases) — a standalone Java
tool that converts any OWL file into ELOT's Org-mode format:

```bash
java -jar elot-exporter.jar input-ontology.owl -o output.org
```

Open the resulting `.org` file in VS Code and all extension features work
immediately. Requires Java 11+.

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
| `elot.labelDisplay.hoverEnabled` | `true` | Show hover cards for CURIEs |

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
```

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
