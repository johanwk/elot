# elot-cli

A CLI tool and VS Code extension for generating OWL Manchester Syntax (OMN)
ontologies from Org-mode outlines.

`elot-cli` is a TypeScript port of the core [ELOT](https://github.com/johanwk/elot)
Org→OMN pipeline (originally implemented in Emacs Lisp). It uses
[orgize](https://github.com/PoiScript/orgize) (a Rust Org-mode parser compiled
to WebAssembly) to read Org-mode files and transforms the structure into valid
OWL Manchester Syntax.

## Features

- **Heading-to-Entity Mapping**: Automatically extracts URIs, CURIEs, and labels
  from Org-mode headings.
- **Property Extraction**: Reads `:PROPERTIES:` drawers and Org-mode keywords.
- **Description List Support**: Converts Org-mode description lists into OWL
  annotations and restrictions.
- **Prefix Management**: Supports prefix tables within Org files for CURIE
  expansion.
- **Ontology Frames**: Generates `Ontology:`, `Class:`, `ObjectProperty:`, and
  `Individual:` frames based on heading structure.
- **Meta-annotations**: Supports nested annotations via nested description lists.
- **Tangle Integration**: Automatically writes output to the file specified in
  `:header-args:omn: :tangle`.
- **Label Display**: Hover over CURIEs to see labels and annotations; press F5
  to visually replace CURIEs with human-readable labels (see below).
- **Headline Folding**: Click the fold/unfold gutter chevrons, or press Tab on a
  heading to toggle folding — like Emacs Org-mode's visibility cycling.
- **Go to Definition**: Ctrl+Click or press F12 on a CURIE to jump to the heading
  where that entity is declared — the VS Code equivalent of Emacs's `M-.`
  (`xref-find-definitions`).
- **Org Indent Mode**: Toggle visual indentation that mirrors Emacs's
  `org-indent-mode` — sub-headings and body text are visually indented without
  modifying the file.
- **Headline Fontification**: Org headings are rendered in **bold** automatically.
- **Description List Tags**: The tag portion of Org description lists (e.g.
  `- rdfs:isDefinedBy ::`) is rendered in a subdued colour to make values stand
  out.
- **IntelliSense Completion**: Press Ctrl+Space to get a dropdown of all OWL
  entities declared in the file — searchable by label or CURIE, showing type
  and annotations. Selecting an item inserts the CURIE.
- **OWL Axiom Syntax Checking**: Real-time validation of OWL Manchester Syntax
  axioms in description lists. Invalid expressions (e.g. `SubClassOf`,
  `EquivalentTo`, `Domain`, `Range`, `Facts`, etc.) are highlighted with red
  squiggly underlines and error messages.

## Label Display (CURIE → Human-readable Labels)

Ontology files are full of identifiers like `obo:BFO_0000001` that are hard to
read. ELOT can display human-readable labels instead, similar to the Emacs ELOT
feature that uses text properties to toggle between CURIEs and labels.

### Hover Information

When you hover over any CURIE (e.g. `obo:BFO_0000001`) or angle-bracket URI
in an `.org` file, a hover card shows:

- **Label** (bold) — e.g. *"entity"@en*
- `URI` (monospace)
- *Type* — e.g. `owl:Class`, `owl:ObjectProperty`
- Extra annotations — e.g. `skos:definition`, `rdfs:comment`

This is always on by default. Disable via the setting
`elot.labelDisplay.hoverEnabled`.

### Visual Label Replacement (Toggle)

Press **F5** (or click the 🏷 icon in the editor title bar, or the status bar
indicator, or right-click → *Elot: Toggle Label Display*) to visually replace
CURIEs with their labels throughout the editor.

When enabled:
- The underlying document text is **not modified** — CURIEs remain in the file
- The CURIE text is visually hidden and replaced by the label via CSS
  pseudo-elements
- Labels are shown in *italic* by default (configurable)
- Copy-paste still copies the real CURIEs (correct behaviour)
- The status bar shows **🏷 Labels** (highlighted) when ON, **`</>` CURIEs** when OFF

Toggle methods:

| Method | Location |
|---|---|
| **F5** | Keyboard shortcut (when `.org` file is active) |
| **🏷 icon** | Editor title bar (top-right) |
| **Status bar** | Bottom-right indicator (clickable) |
| **Right-click** | Context menu → *Elot: Toggle Label Display* |
| **Command Palette** | `Elot: Toggle Label Display` |

### Settings

| Setting | Default | Description |
|---|---|---|
| `elot.labelDisplay.fontStyle` | `"italic"` | Font style for labels: `italic`, `normal`, or `oblique` |
| `elot.labelDisplay.hoverEnabled` | `true` | Show hover cards for CURIEs |

These can be changed in VS Code Settings (search for "elot").

## Headline Folding

Org headings can be folded and unfolded, just like in Emacs Org-mode. The
extension registers a VS Code
[FoldingRangeProvider](https://code.visualstudio.com/api/references/vscode-api#FoldingRangeProvider)
that understands Org headline hierarchy — each heading's fold region extends to
just before the next sibling or ancestor heading.

### How to fold

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
jump directly to the heading where that entity is declared. This is the VS Code
equivalent of Emacs's `M-.` (`xref-find-definitions`).

| Action | Method |
|---|---|
| **Go to definition** | **F12**, or **Ctrl+Click** (`Cmd+Click` on Mac) on a CURIE |
| **Peek definition** (inline preview) | **Alt+F12** |
| **Go back** | **Alt+←** (like Emacs `M-,`) |
| **Right-click** | Context menu → *Go to Definition* / *Peek Definition* |

This works purely within the current Org file — it scans headlines for entity
declarations (using the same `entityFromHeader` parser as the build pipeline)
and jumps to the matching heading. No external index or language server is
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

### How to toggle

| Action | Method |
|---|---|
| **Keyboard shortcut** | **Ctrl+Shift+I** (in an `.org` file) |
| **Command Palette** | *Elot: Toggle Org Indent Mode* |
| **Status bar** | Click the **Flat** / **Indent** indicator (bottom-right) |
| **Title bar** | Click the indent icon ($(indent)) in the editor title area |
| **Right-click** | Context menu → *Elot: Toggle Org Indent Mode* |

### How it works

- Uses VS Code's `TextEditorDecorationType` API with `before` pseudo-elements
  to prepend invisible spacing — the file content is never changed.
- Leading stars on sub-headings are visually hidden (font-size: 0), so
  `*** Heading` appears as `* Heading` at the indented position.
- The status bar shows **Indent** (highlighted) when ON, **Flat** when OFF.

## Fontification

Two always-on decorations enhance readability of Org files:

- **Bold headlines** — All Org headings (`*`, `**`, `***`, …) are rendered in
  **bold**.
- **Subdued description list tags** — In description lists like
  `- rdfs:isDefinedBy :: lis-ont:core`, the tag portion (`- rdfs:isDefinedBy ::`)
  is shown in a muted colour with italic styling, so the description value
  stands out.

These are applied automatically when an `.org` file is opened — no toggle or
configuration needed.

## IntelliSense: Insert Existing Resource

The extension provides a VS Code *CompletionItemProvider* (IntelliSense) that
lets you insert any OWL entity declared in the current Org file via an
autocomplete dropdown — the VS Code equivalent of Emacs's `completing-read` for
ontology resources.

When triggered, the dropdown shows every entity from the document's heading
hierarchy:

| Field | Content |
|---|---|
| **Label** (main text) | Human-readable name, e.g. *"entity"@en* |
| **Description** (right-aligned) | CURIE, e.g. `obo:BFO_0000001` |
| **Detail** (subtitle) | RDF type, e.g. `owl:Class` |
| **Icon** | Class icon for `owl:Class`, Property for properties, Value for individuals |
| **Documentation panel** | Full detail with label, CURIE, type, and description properties (same info as hover) |

Selecting an item inserts the **CURIE** into the document.

### How to trigger

| Action | Method |
|---|---|
| **Trigger completion** | **Ctrl+Space** (standard VS Code IntelliSense) |
| **Auto-trigger** | Type `:` after a prefix name (e.g. `obo:`) |
| **Browse the list** | Arrow keys, then **Enter** or **Tab** to insert |
| **See full details** | The documentation panel opens for the selected item |

### Filtering and sorting

You can filter by typing either the **label** or the **CURIE** — for example,
typing `entity` or `obo:BFO` both find `obo:BFO_0000001`. Items are grouped
by type (Classes → Properties → Individuals → Other), then sorted
alphabetically by label within each group.

### No configuration needed

The provider registers programmatically — no `package.json` contributes entry
or language server is required. It works for any file matched as `{ language:
"org" }` or `**/*.org`.

## OWL Axiom Syntax Checking

The extension validates OWL Manchester Syntax axioms as you type. Any
description list item whose tag is an OMN keyword (`SubClassOf`,
`EquivalentTo`, `DisjointWith`, `Domain`, `Range`, `SubPropertyOf`,
`InverseOf`, `SubPropertyChain`, `Types`, `Facts`, `SameAs`,
`DifferentFrom`, etc.) is parsed against the OWL Manchester Syntax grammar.

If the axiom value is malformed, a red squiggly underline appears on the
offending text with an error message explaining what the parser expected.
Diagnostics are updated on every save and when the file is opened.

### What is checked

| Keyword | Grammar rule |
|---|---|
| `SubClassOf`, `EquivalentTo`, `DisjointWith`, `Domain`, `Range`, `Types` | `ClassExpressionList` |
| `SubPropertyOf`, `InverseOf` | `ObjectPropertyExpressionList` |
| `SubPropertyChain` | `SubPropertyChain` |
| `Facts` | `Fact` |
| `SameAs`, `DifferentFrom` | `IndividualIRIList` |

### Example

```org
- SubClassOf :: obo:BFO_0000139 only (obo:BFO_0000015 or obo:BFO_0000035)
```

If you accidentally wrote `only` where `or` should be, or used a malformed
IRI, the error is caught immediately — no need to load the ontology into a
reasoner or external tool first.

The grammar is shared between the Emacs ELOT package and the VS Code extension
(maintained as a single Peggy/PEG source file in `syntax/owl-manchester.peggy`).

## Tip: Word Wrap for Org Files

Org files often have long lines. VS Code can wrap them visually (like Emacs's
`visual-line-mode`) without modifying the file. Press **Alt+Z** to toggle, or
add this to your VS Code `settings.json` for a persistent per-language setting:

```json
"[org]": {
    "editor.wordWrap": "on"
}
```

## Converting OWL Files to Org (elot-exporter)

If you have an existing OWL ontology (in RDF/XML, Turtle, or any standard RDF
format) and want to edit it as an Org file with the Elot VS Code extension, use
**elot-exporter** — a standalone Java tool that converts OWL files into Elot's
Org-mode format.

### Obtaining elot-exporter

Download the latest `elot-exporter.jar` from the
[Elot releases page](https://github.com/johanwk/elot/releases).

**Prerequisite:** Java (JRE 11 or later) must be installed on your system.
Verify with:

```bash
java -version
```

### Usage

```bash
# Convert an OWL file to Org format
java -jar elot-exporter.jar input-ontology.owl -o output.org

# Pipe from stdin
cat ontology.ttl | java -jar elot-exporter.jar -f turtle -o output.org
```

The resulting `.org` file can be opened directly in VS Code with the Elot
extension, giving you label display, folding, IntelliSense, and all other
features described above.

### Bundling the JAR inside the extension?

The `elot-exporter.jar` is currently **not** bundled inside the VSIX extension.
The JAR adds approximately 20–30 MB to the download size (it includes embedded
dependencies like the OWL API and Apache Jena). While VS Code extensions can
technically include JARs (the Java and Red Hat extensions do this), it would
significantly increase the extension size — the current VSIX is well under 1 MB.

For now, downloading the JAR separately from the releases page is the
recommended approach. Users are responsible for having Java installed. A future
version may add a VS Code command that auto-downloads the JAR on first use.

## Quick Start — `make help`

Not sure what to run? Just type:

```bash
make help
```

This prints a colour-coded list of every available target (`install`, `bundle`,
`test`, `package`, …) with a short description. It is the single best starting
point when you first clone the repo or come back to it after a break.

## Prerequisites

- [Node.js](https://nodejs.org/) ≥ 18
- [Rust](https://rustup.rs/) toolchain (for building the WASM parser)
- [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/) (`cargo install wasm-pack`)

## Setup (after cloning)

```bash
npm install
```

## Building

The build has two stages: first compile the Rust Org parser to WebAssembly,
then bundle the TypeScript into `dist/`.

```bash
# Full build (WASM + esbuild bundle) — this is the only command you need:
npm run bundle
```

This runs the following two stages automatically:

1. **`npm run build:wasm`** — runs `wasm-pack build --target nodejs` inside
   `elot-orgize/`, producing `src/wasm/elot_orgize.js` and
   `src/wasm/elot_orgize_bg.wasm`.

2. **`node esbuild.mjs`** — cleans `dist/`, copies the `.wasm` file into
   `dist/`, then bundles `src/extension.ts` → `dist/extension.js` and
   `src/cli.ts` → `dist/cli.js` via esbuild.

The generated files in `src/wasm/` and `dist/` are **gitignored** — you must
run `npm run bundle` after every fresh clone.

### What ends up in `dist/`

| File | Description |
|---|---|
| `extension.js` | VS Code extension entry point (CJS bundle) |
| `cli.js` | CLI entry point (CJS bundle with shebang) |
| `elot_orgize_bg.wasm` | orgize Org parser (~284 KB), loaded at runtime via `fs.readFileSync` |

### Key build dependencies

| Dependency | Version | Purpose |
|---|---|---|
| orgize (Rust) | 0.10.0-alpha.10 | Org-mode parser, compiled to WASM |
| wasm-bindgen | 0.2 | Rust↔JS binding for WASM |
| esbuild | ≥ 0.25 | Bundles TS → JS, copies `.wasm` |
| wasm-pack | (latest) | Builds the Rust crate into a Node.js-compatible WASM package |

Pinned versions are recorded in `elot-orgize/Cargo.toml` and
`elot-orgize/Cargo.lock`.

### Other build commands

```bash
# Type-check only (via tsc, no bundling)
npm run build

# Watch mode — recompile TypeScript on every file change
npm run watch
```

## Usage

### CLI

```bash
# Generate OMN, write to stdout
node dist/cli.js examples/bfo-core.org -

# Write to a specific file
node dist/cli.js examples/bfo-core.org output.omn

# Omit output path — uses the :tangle target from the Org file, or stdout
node dist/cli.js examples/bfo-core.org
```

During development (without bundling), you can also run directly via `tsx`:

```bash
npm run elot -- examples/bfo-core.org
```

Note: on Windows, `/dev/stdout` does not exist. Use `-` instead for stdout.

## VS Code Extension Packaging

```bash
# Package into a .vsix (runs npm run bundle automatically via prepublish)
npx @vscode/vsce package
```

This produces `elot-<version>.vsix`. The VSIX contains:

- `dist/extension.js` — bundled extension code
- `dist/cli.js` — bundled CLI
- `dist/elot_orgize_bg.wasm` — the WASM binary (~284 KB)
- `package.json`, `LICENSE`, `readme.md`

Files excluded from the VSIX are listed in `.vscodeignore` (source code,
Rust crate, examples, dev tooling, etc.).

To install the extension locally:

```bash
code --install-extension elot-0.2.0.vsix
```

## Testing

The project uses a "golden file" test that compares generated output against a
reference `.omn` file.

```bash
# Run all tests
npm test

# Run specific unit tests
npx tsx src/tests/entityFromHeader.test.ts
npx tsx src/tests/annotationValue.test.ts
npx tsx src/tests/omnKeywords.test.ts
```

## Debugging

To diagnose issues with Org parsing or hierarchy extraction:

```bash
# See the raw orgize WASM AST structure
npx tsx src/debug-ast.ts examples/bfo-core.org

# See the parsed ElotNode hierarchy (after property extraction)
npx tsx src/debug-hierarchy.ts examples/bfo-core.org
```

## Rebuilding After Changes

| What changed | What to run |
|---|---|
| Rust code in `elot-orgize/src/` | `make bundle` (rebuilds WASM + re-bundles) |
| TypeScript in `src/` | `make bundle` (re-bundles; WASM rebuild is fast if unchanged) |
| Only want to type-check TS | `make build` |
| Want a fresh `.vsix` | `make package` (runs `bundle` via prepublish) |
| Forgot the commands | `make help` |

### Quick-reference: full clean rebuild from scratch

```bash
git clone <repo>
cd elot-cli
make all                # does everything: install → bundle → test → package
```

Or step by step:

```bash
make help               # see all available targets
make install            # npm install
make bundle             # builds WASM + bundles dist/
make test               # golden-file test
make package            # produces .vsix
```

## Architecture

The project follows a recursive transformation pipeline:

```
Org text → [orgize WASM] → JSON AST → parseOrg() → ElotNode tree → generateOmn() → OMN
```

1. **`parseOrgWasm.ts`**: Calls the orgize WASM parser, maps the JSON result
   to an `ElotNode` tree with properties, descriptions, and prefix tables.
2. **`cli.ts`**: Entry point for command-line use, handles file I/O.
3. **`extension.ts`**: VS Code extension entry point.
4. **`generateOmn.ts`**: Assembles the final OMN document from the node tree.
5. **`omnDeclarations.ts`**: Recursively generates frames for each node.
6. **`omnFrame.ts`**: Formats individual OWL frames (Annotations and
   Restrictions).

The Rust crate `elot-orgize/` contains:

- **`lib.rs`**: WASM entry point (`parse_org_to_elot`)
- **`parse.rs`**: Walks the orgize AST to extract headlines, property drawers,
  prefix tables, description lists (with meta-annotations), and `#+begin_src omn`
  blocks
- **`types.rs`**: Rust data structures mirroring the TypeScript `ElotNode` type

See `PLAN.org` for the detailed implementation roadmap and function mapping
from the original Elisp source.
