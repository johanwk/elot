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
code --install-extension elot-0.1.0.vsix
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
