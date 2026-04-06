# ELOT Examples — Round-Trip Testing

This directory contains a **round-trip test suite** for
[ELOT](https://github.com/johanwk/elot).  The tests exercise the full
ELOT pipeline on a collection of real-world ontologies, checking
whether an ontology survives the journey from OWL through ELOT's Org
format and back again.

## Motivation

ELOT lets you author OWL ontologies in Emacs Org mode.  A key part of
the workflow is *importing* an existing OWL ontology into Org (via
`elot-exporter`) and *exporting* it back to OWL (via `elot-tangle`).
Ideally this round-trip would be lossless — the output ontology would
be logically identical to the input.  In practice, some information
may be lost or transformed:

- **Annotation patterns** that ELOT does not yet represent may be
  dropped or simplified during export to Org.
- **Axiom structures** such as complex class expressions, property
  chains, or GCIs may not survive the Org representation faithfully.
- **Serialisation differences** (blank node IDs, ordering, syntactic
  sugar) can produce superficial diffs even when the ontology is
  logically equivalent.

Running these tests regularly serves two purposes:

1. **Bug detection.**  A regression that silently drops axioms shows
   up as new diff lines in the report.
2. **Documenting limitations.**  The diffs that persist across runs
   make explicit what ELOT cannot (yet) represent.  This sets clear
   expectations for users who import third-party ontologies.

## How It Works

The pipeline has seven steps, orchestrated by `make`:

```
 ┌──────────────┐
 │ ontology-list│   Ontology IDs, source types, and URIs
 │    .tsv      │
 └──────┬───────┘
        │
        ▼
 ┌──────────────┐
 │  1. Fetch    │   Download remote OWL files (or locate local ones)
 │   sources/   │   → sources/<id>.<ext>
 └──────┬───────┘
        │
        ▼
 ┌──────────────┐
 │  2. Baseline │   robot convert → OMN (canonical form for diffing)
 │   OMN        │   → sources/<id>-baseline.omn
 └──────┬───────┘
        │
        ▼
 ┌──────────────┐
 │  3. Export   │   elot-exporter.jar: OWL → ELOT Org
 │   to Org     │   → org/<id>.org
 └──────┬───────┘
        │
        ▼
 ┌──────────────┐
 │  4. Tangle   │   Emacs batch: Org → OMN via elot-tangle
 │   to OMN     │   → output/<id>.omn
 └──────┬───────┘
        │
        ▼
 ┌──────────────┐
 │  5. Diff     │   robot diff: baseline vs. round-tripped OMN
 │              │   → reports/<id>.diff, reports/<id>.diff.html
 └──────┬───────┘
        │
        ▼
 ┌──────────────┐
 │  6. Report   │   JSON summary per ontology
 │   JSON       │   → reports/<id>.json
 └──────┬───────┘
        │
        ▼
 ┌──────────────────────┐
 │  7. Summary report   │   Markdown table of all results
 │  roundtrip-report.md │
 └──────────────────────┘
```

### Step 1 — Fetch Sources

Ontologies are listed in `ontology-list.tsv` (tab-separated: `id`,
`source_type`, `source_uri`).  Remote ontologies (`url` type) are
downloaded with `curl`; local ones (`local` type) are used in place.

### Step 2 — Baseline OMN

Each source is converted to OWL Manchester Syntax (`.omn`) using
[ROBOT](http://robot.obolibrary.org/).  This normalised OMN serves as
the reference for the diff — it removes serialisation variation from
the comparison.

### Step 3 — Export to Org

The ELOT exporter (`elot-exporter.jar`) reads the source OWL file and
writes an ELOT-format Org file.  This is the "import" half of the
round-trip.

### Step 4 — Tangle to OMN

Emacs runs in batch mode, loads `elot-mode`, visits the generated Org
file, and tangles it back to OMN.  This is the "export" half of the
round-trip.

### Step 5 — Diff

ROBOT's `diff` command compares the baseline OMN (step 2) against the
round-tripped OMN (step 4).  Both a plain-text diff and an HTML
report are produced for each ontology.

### Step 6 — JSON Report

A small JSON file per ontology records metadata (source URI,
timestamp, ELOT version) alongside the diff outcome (status and number
of diff lines).

### Step 7 — Summary Report

The script `generate-roundtrip-report.sh` reads all JSON reports and
produces `roundtrip-report.md` — a Markdown table showing each
ontology's round-trip status at a glance.

## Quick Start

```sh
# Prerequisites: java, emacs, curl, and ROBOT on PATH (or configured below).

# Run the full pipeline:
make

# Run in parallel (recommended for many ontologies):
make -j4

# Process a single ontology end-to-end:
make pizza.roundtrip

# Download sources only:
make fetch-sources

# See all available targets:
make help
```

### Configuration

Override tool paths via environment variables or on the `make`
command line:

| Variable             | Default                          | Description                          |
|----------------------|----------------------------------|--------------------------------------|
| `ELOT_EXPORTER_JAR`  | `java -jar ~/bin/elot-exporter.jar` | ELOT exporter command             |
| `ROBOT`              | `java -jar ~/bin/robot.jar`      | ROBOT command                        |
| `EMACS`              | `emacs`                          | Emacs binary                         |
| `ELOT_PACKAGE_DIR`   | `../elot-package`                | Path to elot-package elisp directory |

## Adding a New Ontology

Append a line to `ontology-list.tsv`:

```tsv
my-ontology	url	https://example.com/my-ontology.owl
```

Or for a local file already in `sources/`:

```tsv
my-ontology	local	sources/my-ontology.omn
```

Then run `make my-ontology.roundtrip` to test it, or just `make` for
the full suite.

## Interpreting Results

The summary in `roundtrip-report.md` shows:

- **✅ identical** — the round-trip preserved all axioms.  The
  ontology is fully representable in ELOT's Org format.
- **⚠️ differences** — some axioms differ.  Inspect the diff
  (`reports/<id>.diff`) or the HTML report (`reports/<id>.diff.html`)
  to understand what was lost or changed.
- **❌ error** — a pipeline step failed (download, conversion,
  tangle, or diff).

A persistent diff is not necessarily a bug — it may reflect a known
limitation of ELOT's Org representation.  Diffs that *appear* after a
code change, however, likely indicate a regression.

## Cleanup

```sh
make clean       # Remove derived files (org/, output/, reports/), keep downloads
make distclean   # Also remove downloaded sources (local files are kept)
```
