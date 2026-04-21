# ELOT test harness

ERT-based tests for the ELOT Elisp codebase, starting with the
SQLite-backed global-slurp subsystem described in
[`ELOT-DB-PLAN.org`](../ELOT-DB-PLAN.org).

This directory is **not** shipped to MELPA users — it lives outside
`elot-package/` deliberately.

## Layout

- `elot-db-test.el` — Step 1.1: schema creation, idempotent init,
  cascade delete, `schema_version` seeding, migration-mismatch error.
- `Makefile` — local runner, mirroring the pattern used by `syntax/`.
- `fixtures/` — (future) shared CSV/TSV/Org/TTL fixtures for Step 1.3+.

## Running

```sh
make -C test            # all tests
make -C test db-test    # just the DB-layer tests
```

Requires Emacs 29+ (built with SQLite support) — run locally on Linux.
No GitHub Actions workflow is provided; see the *Cross-cutting: Test
Infrastructure* section of `ELOT-DB-PLAN.org` for rationale.

Network-dependent tests (none yet at Step 1.1) are gated behind the
environment variable `ELOT_TEST_NETWORK=1`.
