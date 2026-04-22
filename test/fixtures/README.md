# Test fixtures for `elot-sources.el` (Step 1.3)

Small, hand-written, hermetic inputs for the parser test suite. Kept
deliberately tiny so that failures are diagnosable by eye and so that
the fixtures can be asserted against with exact equality.

| File                       | Parser under test                | Shape                                                   |
|----------------------------|----------------------------------|---------------------------------------------------------|
| `minimal-ontology.org`     | `elot-source-parse-org`          | 2 classes + 1 object property, with prefixes            |
| `labels.csv`               | `elot-source-parse-csv`          | header row: `id,label,definition,kind`; 3 rows          |
| `labels.tsv`               | `elot-source-parse-tsv`          | UC3 shape: opaque `EMP-NNN` ids, 3 rows                 |
| `labels-flat.json`         | `elot-source-parse-json` (flat)  | `{"id": "label", ...}`                                  |
| `labels-nested.json`       | `elot-source-parse-json` (nested)| `{"id": {"label": ..., "prop": ...}, ...}`              |
| `rq/wd-labels.rq`          | `elot-source-parse-rq`           | SPARQL query; never executed by the suite               |
| `rq/data.ttl`              | `elot-source-parse-rq`           | Local data source; only its mtime matters               |
| `rq/rq-cache/`             | `elot-source-parse-rq`           | Directory where the cache-roundtrip test writes a CSV   |

Design notes:

- The CSV, TSV and JSON fixtures intentionally overlap in content (Widget /
  Gadget / connectsTo) so that a single set of expected-value constants
  in the test file can cover all three shapes.
- The TSV fixture uses UC3-style opaque employee codes so that the
  "non-ontology project" path is exercised alongside the ontology path.
- Live-network and ROBOT-dependent tests are gated by
  `ELOT_TEST_NETWORK=1` and live outside this directory.
