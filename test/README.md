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

## Known intermittent failure: clipboard interference

`elot-gptel-move-test-revalidation-rolls-back` (and other
move-resource tests) can fail intermittently with a raw Org error:

```
ERROR: The kill is not a (set of) tree(s).  Use `C-y' to yank anyway
```

instead of the expected `ERROR: revalidation failed …` message.

Cause: `elot-move-resource` relocates subtrees via Org's
`org-cut-subtree` / `org-paste-subtree`, which communicate through the
kill-ring. `org-paste-subtree` verifies the pending kill really is the
subtree it cut (via `org-kill-is-subtree-p`, comparing `current-kill`
against `org-subtree-clip`). When `select-enable-clipboard` is on,
`current-kill` consults the **system clipboard** — so if you perform an
ordinary cut/copy in another application *while the tests are running*,
that foreign clipboard entry poisons the check and the paste aborts.

This is a test-environment artifact, not a code bug. Simply re-run the
tests without touching the clipboard, or run the suite in a session
where nothing else uses the clipboard.
