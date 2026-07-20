# AGENTS.md

## Repository overview

ELOT is a literate ontology-engineering project with three main implementations:

- `elot-package/`: the Emacs Lisp package and reference implementation.
- `tools/elot-cli/`: the TypeScript CLI and VS Code extension, with a Rust/WASM
  Org parser.
- `tools/elot-exporter/`: the Java/OWLAPI OWL-to-Org converter.

The repository also contains the shared Manchester Syntax grammar in `syntax/`,
ERT tests in `test/`, ontology examples in `examples/`, and user documentation
in `documentation/`.

## Source-of-truth rules

- The Emacs Lisp files `elot-package/elot.el`, `elot-package/elot-mode.el`,
  `elot-package/elot-tangle.el`, and `elot-package/elot-label-display.el` are
  tangled from the corresponding root Org files. Make substantive changes in
  `elot-defs.org`, `elot-mode.org`, `elot-tangling.org`, or
  `elot-label-display.org`, then tangle them; do not update only the generated
  `.el` file.
- `syntax/owl-manchester.peggy` is the source of truth for the Manchester
  Syntax grammar. Never hand-edit `syntax/elot-owl-grammar.el` or
  `elot-package/elot-owl-grammar.el`.
- After grammar changes, run `make -C syntax install test`. The CLI parser is
  generated from the same Peggy grammar by `npm run build:parser` in
  `tools/elot-cli/`.
- Generated CLI files under `tools/elot-cli/src/wasm/` and
  `tools/elot-cli/dist/` are gitignored. Rebuild them rather than committing
  them.
- Keep changes focused. Do not modify generated artifacts, fixtures, golden
  files, or unrelated tests unless the behavior being changed requires it.

## Development workflows

### Emacs package

Use Emacs 30.1 with SQLite support. From the repository root:

```sh
make stable-check   # byte-compile, full ERT suite, and load smoke test
make test           # full ERT suite only
make byte-compile
make smoke
make package-lint
```

Tests live in `test/`. Run a focused Make target there while iterating (see
`make -C test help`), then run `make stable-check` before finalizing an Emacs
code change. ROBOT- and network-dependent tests are designed to skip when
their dependencies are unavailable; network tests require
`ELOT_TEST_NETWORK=1`.

When changing tangled Emacs code, preserve the existing lexical-binding
headers and package conventions. Add ERT coverage in `test/` for behavioral
changes.

### Manchester Syntax grammar

```sh
make -C syntax install test
```

Update `syntax/test-cases.json` for new accepted or rejected syntax. These
cases are shared with the TypeScript implementation.

### TypeScript CLI and VS Code extension

Prerequisites are Node.js 18 or newer, Rust, and `wasm-pack`.

```sh
cd tools/elot-cli
npm install
npm run build       # TypeScript type-check
npm run bundle      # Rust/WASM, parser generation, and JS bundles
npm test            # golden tests
make help           # lists focused test targets
```

Use the focused `make test-*` targets during development and run the relevant
broader suites before finalizing. HTML integration tests require Pandoc; some
database tests require ROBOT or network access.

### Java exporter

Java 21 or newer is expected by the repository-level documentation; Maven
3.6 or newer is required.

```sh
cd tools/elot-exporter
make build
make test
```

The formatter classes under `src/main/java/com/elotexport/` own output
formatting. Keep shared markers and property names in `OntologyConstants`.

## Validation expectations

- Run the smallest relevant check while iterating and the complete check for
  each changed component before finalizing.
- For documentation-only changes, review links, paths, commands, and
  terminology; code builds are not required unless documentation generation
  is affected.
- Do not “fix” known package-lint warnings unrelated to the change. The
  repository documents two accepted `with-eval-after-load` warnings.
- Do not enable network-dependent tests by default.
- Check `git diff` and `git status` to ensure generated or temporary files are
  not included.
