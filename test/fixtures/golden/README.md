# Golden round-trip fixtures (Step 2.2.5 slice 3a)

Canonical-JSON dumps of an ELOT DB after ingesting a known input
fixture.  Both the Elisp writer (`elot-db-update-source`) and the
TypeScript writer (`ElotDb.updateSource`) must produce **byte-
identical** output here -- that is the whole point of these fixtures.

## Bootstrap

The goldens are NOT pre-committed.  Bootstrap them with EITHER side
(the TS side is faster) and then verify the other:

```sh
# TS-side bootstrap:
ELOT_GOLDEN_REGEN=1 npx tsx tools/elot-cli/src/tests/db/golden.test.ts

# Elisp-side verification (no regen flag):
make -C test golden-test
```

If the Elisp test passes, writer parity is verified end-to-end.
If it fails, run the Elisp side with `ELOT_GOLDEN_REGEN=1`, then
`diff -u` the two regenerations of each `*.golden.json` to see the
exact byte where the two writers diverge.

## Format

- Minified, single-line UTF-8 JSON, **trailing newline**.
- Object keys emitted alphabetically at every level.
- Top-level shape:
  ```
  {"attributes":[...],"entities":[...],"prefixes":[...],"sources":[...]}
  ```
- Excluded columns: `sources.last_modified`, `sources.last_updated`
  (timestamps); `global_prefixes` (seed data, identical on both sides).
- Row sort orders (matching dumper's `ORDER BY`):
  - `attributes`: `(id, source, data_source, prop, lang, value)`
  - `entities`:   `(id, source, data_source)`
  - `prefixes`:   `(source, data_source, prefix)`
  - `sources`:    `(source, data_source)`
- String escapes: `\"`, `\\`, `\n`, `\r`, `\t`, `\u00XX` for other
  ASCII control chars; non-ASCII emitted as raw UTF-8 bytes.  Forward
  slashes are NOT escaped.

## Files

| Golden file                       | Input fixture            | Source name | Type |
|-----------------------------------|--------------------------|-------------|------|
| `labels-csv.golden.json`          | `../labels.csv`          | `labels`    | csv  |
| `labels-nested-json.golden.json`  | `../labels-nested.json`  | `labels`    | json |
