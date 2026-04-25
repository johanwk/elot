# Changelog

All notable changes to the ELOT VS Code extension and `elot-cli` are
documented in this file. The format is loosely based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the
project follows [Semantic Versioning](https://semver.org/) once it
reaches 1.0.

## [Unreleased]

### Added

- **DB-backed (global) label display.** Resolve CURIEs and full URIs
  to human-readable labels across *any* file in VS Code (Turtle,
  SPARQL, JSON, source code, plain text, …), backed by a persistent
  SQLite index populated by `elot-cli db`. Independent from the
  Org-only label display; Org files keep their existing F5 toggle
  unchanged.

  - **Hover provider** — non-Org buffers show a Markdown card with
    the label, resolved id, `rdf:type`, definitions, comments, and
    a `[src: NAME]` provenance footer. Two-pass id resolution
    (literal → CURIE-expand → URI-contract) so the same hover works
    for `obo:BFO_0000001`, `<http://purl.obolibrary.org/obo/BFO_0000001>`,
    and the bare URI form. Setting:
    `elot.globalLabelDisplay.hoverEnabled` (default `true`).

  - **Visual label replacement (F5).** Same idea as the Org-side
    toggle: document text is unchanged; CURIEs and IRIs are
    rendered with their labels via CSS pseudo-elements. The
    new command `elot.toggleGlobalLabels` is bound to **F5** when
    the active editor is *not* an `.org` file (the existing Org F5
    keeps working in `.org` files). Settings:
    `elot.globalLabelDisplay.maxIds` (default `500`),
    `elot.globalLabelDisplay.includeLanguages` (default = a curated
    list of common formats; Org is always excluded),
    `elot.labelDisplay.fontStyle` (shared with the Org toggle).

  - **Status-bar indicator** showing active-source count and total
    decoratable ids, e.g. `🏷 Labels (3 src, 12k ids)`. Click to
    toggle global label display. Tooltip shows a per-source
    breakdown and capped-warning state.

  - **Source-management commands**:
    - `Elot: Activate Label Source` — multi-select QuickPick of
      inactive sources.
    - `Elot: Deactivate Label Source` — multi-select QuickPick of
      active sources.
    - `Elot: Reorder Active Label Sources` — pick a source, then
      *up* / *down* / *top* / *bottom* / done. (Per-item button
      reorder is planned in 2.3.6.)
    - `Elot: Label DB Info` — diagnostic showing resolved DB path,
      bridge state, registered sources with id counts, and current
      settings. Useful first stop when something looks off.

  - **Settings** for the new feature:
    - `elot.dbPath` (workspace overrides user; empty = per-platform
      VS Code `globalStorage` location).
    - `elot.activeLabelSources` (ordered; first match wins; accepts
      shorthand strings or canonical `{source, dataSource}` objects).
    - `elot.preferredLanguages` (BCP-47 tags in priority order;
      empty = `["", "en"]` — untagged first, then English).
    - `elot.globalLabelDisplay.{hoverEnabled, maxIds, includeLanguages}`.

### Added (CLI)

- **`elot-cli db` sub-command** — full DB management surface:
  - `db init` — create / open the SQLite database (idempotent).
  - `db register <file> --source <name> [--type csv|tsv|json|ttl|rq|org]`
    with parsers for CSV/TSV (RFC-4180), JSON (flat + nested
    shapes), Turtle (via ROBOT, with per-project query override
    at `<root>/.elot/ttl-label-query.rq`), SPARQL `.rq` (against
    a TTL file or HTTP endpoint, with `.elot-cache/` defensive
    caching), and ELOT Org files (reuses the orgize WASM parser
    + slurp builder).
  - `db refresh <name> --file <file>` for symmetric re-ingest.
  - `db list [--prefixes] [--format tsv|json|table]`,
    `db lookup <label> [--active] [--format tsv|json]`,
    `db attr <id> [<prop>] [--format tsv|json]`,
    `db remove <name>` (cascades to entities, attributes, prefixes).
  - `<id>` accepts a literal stored id, a CURIE (expanded), or a
    full URI (contracted). Read commands honour
    `$ELOT_PREFERRED_LANGUAGES`.

- **Schema v3 only.** Older databases are refused with a clear
  message directing the user to upgrade via Emacs. Canonical DDL
  shipped as `elot-package/schema.sql` and consumed verbatim by
  both the Elisp and TS writers; a byte-identical golden
  round-trip test (`test/fixtures/golden/`) keeps the two
  implementations in lockstep.

### Notes

- The CLI is the **sole writer**; the VS Code extension opens the
  database **read-only** and re-loads on file-mtime change. This
  means a single shared DB can safely be edited from a terminal
  while VS Code is open — the extension picks up changes within a
  debounce window.
- The Emacs and VS Code defaults point at *different* per-platform
  files (no cross-editor sharing by default). Use `--db` or
  `$ELOT_DB_PATH` for a shared file when desired.
