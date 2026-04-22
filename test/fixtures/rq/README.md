# SPARQL .rq parser fixtures

Used by Step 1.3 tests for `elot-source-parse-rq`. ROBOT is **never**
invoked from the test suite.

Files:

- `wd-labels.rq` - a trivial SELECT; content never executed.
- `data.ttl` - local data source referenced by the test; only its
  mtime matters.
- `rq-cache/` - pre-baked CSV cache that the parser is expected to
  consume directly when newer than both `wd-labels.rq` and `data.ttl`.
  The cache filename encodes `(query-path, data-source)` via a short
  hash; the test constructs the expected path through the same helper
  that the parser uses (`elot-source-rq-cache-path`), so no filename
  is hard-coded in the fixture directory beyond what the test writes.

Live-execution coverage (actual ROBOT / network calls) is gated by
the `ELOT_TEST_NETWORK=1` environment variable and lives in separate,
skipped-by-default ERT cases.
