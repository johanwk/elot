EMACS ?= emacs

PACKAGE_DIR := ../elot-package

# Canned recipe: run one ERT test file in batch with the shared helper.
# Loads test-helper.el from the current directory so that
# `elot-db-sync-on-slurp' is nil and `elot-db-file' is redirected to a
# per-process temp sqlite for every target.  Tests that genuinely need
# the slurp -> DB write path can `let'-bind both variables locally.
#
# Usage in a recipe:   $(call run-ert-test,elot-foo-test.el)
define run-ert-test
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l ./test-helper.el \
	  -l $(1) \
	  -f ert-run-tests-batch-and-exit
endef

.PHONY: all test test-helper-guard-test db-test prefix-test crud-test remove-source-test sources-test register-test active-sources-test slurp-integration-test global-mode-test active-sources-ui-test ttl-prefix-test menu-test step-1-11-test step-1-12-test step-1-12-item-b-test step-1-13-test step-1-14-test step-1-15-test step-1-16-1-test step-1-16-2-test step-1-16-3-test step-1-16-4-test step-1-16-5-test step-1-16-6-test step-1-16-7-test step-1-16-8-test step-2-1-test sparql-merge-prefixes-test sparql-exec-helpers-test sparql-advice-test sparql-roundtrip-test tangle-roundtrip-test load-without-sparql-test golden-test gptel-test robot-test omn-validate-test omn-report-test gptel-diff-test sparql-tool-test reasoning-test readonly-test db-query-tool-test db-schema-tool-test db-shims-test db-shims-wave-b-test db-search-test db-borrow-test db-borrow-compose-test conventions-test lint-content-test check-test subclass-in-dl-test lint-oops-test corpus-test corpus-oops-test lint-axiom-signature-test id-test id-insert-test id-set-scheme-test id-rename-test gptel-rename-test move-resource-test gptel-move-test gptel-insert-test gptel-confirm-mutations-test hh-stale-test tangle-blank-omn-row-test lint-blank-omn-row-test gptel-db-activate-source-test gptel-delete-reference-scan-test gptel-replace-parents-test gptel-replace-with-parent-test gptel-rename-merge-test gptel-delete-apply-test gptel-delete-resource-test clean help

all: test

# Run the full ELOT ERT test suite.
test: test-helper-guard-test db-test prefix-test crud-test remove-source-test sources-test register-test active-sources-test slurp-integration-test global-mode-test active-sources-ui-test ttl-prefix-test menu-test step-1-11-test step-1-12-test step-1-12-item-b-test step-1-13-test step-1-14-test step-1-15-test step-1-16-1-test step-1-16-2-test step-1-16-3-test step-1-16-4-test step-1-16-5-test step-1-16-6-test step-1-16-7-test step-1-16-8-test step-2-1-test sparql-merge-prefixes-test sparql-exec-helpers-test sparql-advice-test sparql-roundtrip-test tangle-roundtrip-test load-without-sparql-test golden-test gptel-test

# Regression guard: verifies test-helper.el is loaded for every
# batch run (elot-db-sync-on-slurp is nil; elot-db-file is under
# temporary-file-directory).  See briefings/test-helper-rollout-
# and-source-removal.org, Task 1.4.
test-helper-guard-test:
	$(call run-ert-test,elot-test-helper-guard-test.el)

# Step 1.1 tests: SQLite DB layer.
db-test:
	$(call run-ert-test,elot-db-test.el)

# Step 1.1.1 tests: prefix handling (schema v2).
prefix-test:
	$(call run-ert-test,elot-db-prefixes-test.el)

# Step 1.2 tests: write/read primitives.
crud-test:
	$(call run-ert-test,elot-db-crud-test.el)

# Briefing test-helper-rollout-and-source-removal.org, Task 2.6:
# `elot-db-remove-source' (exact + LIKE-pattern modes) and the
# companion `elot-db-list-sources-like' preview helper.
remove-source-test:
	$(call run-ert-test,elot-db-remove-source-test.el)

# Step 1.3 + 1.3.1 tests: source parsers (org / csv / tsv / json / rq-cache)
# plus ROBOT-local Tier 1 tests (skipped cleanly if ROBOT absent).
sources-test:
	$(call run-ert-test,elot-sources-test.el)

# Step 1.4 tests: source registration UI
# (register/unregister/refresh/refresh-all/list-sources).
register-test:
	$(call run-ert-test,elot-sources-register-test.el)

# Step 1.5 tests: per-project active-sources configuration.
active-sources-test:
	$(call run-ert-test,elot-active-sources-test.el)

# Step 1.6 tests: elot-slurp-to-vars integration with the DB +
# two-tier lookup in elot-codelist-id-label / elot-attriblist-label-value.
slurp-integration-test:
	$(call run-ert-test,elot-slurp-integration-test.el)

# Step 1.7 tests: elot-global-label-display-mode (DB-driven font-lock
# regex; decorates identifiers in any buffer / any major mode).
global-mode-test:
	$(call run-ert-test,elot-global-mode-test.el)

# Step 1.7.2 tests: active-sources priority UI
# (move-up/down, tabulated list, change hook, auto-repersist).
active-sources-ui-test:
	$(call run-ert-test,elot-active-sources-ui-test.el)

# Step 1.7.3 tests: TTL @prefix harvest + dual-form (IRI/CURIE)
# global-mode matcher + safety-net cap.  ROBOT-gated tests skip
# cleanly when ROBOT is absent.
ttl-prefix-test:
	$(call run-ert-test,elot-ttl-prefix-test.el)

# Step 1.11 tests: `elot-global-label-display-mode' polish
# (no-sources hint, collision resolution, autoload cookies).
step-1-11-test:
	$(call run-ert-test,elot-step-1-11-test.el)

# Step 1.12 Item A tests: `elot-label-lookup' generalisation
# (DB-backed label collection, dispatcher behaviour, autoload smoke test).
step-1-12-test:
	$(call run-ert-test,elot-label-lookup-test.el)

# Step 1.12 Item B tests: ELOT menu visibility in non-ELOT buffers
# (menu attachment to `elot-global-label-display-mode-map',
# `:enable' guards on ELOT-only entries, no duplicate menu when
# both modes are active).
step-1-12-item-b-test:
	$(call run-ert-test,elot-menu-item-b-test.el)

# Step 1.13 tests: scope choice (local / external / both) for
# `elot-label-lookup' + union dedup + wrapper commands.
step-1-13-test:
	$(call run-ert-test,elot-label-lookup-scope-test.el)

# Step 1.14 Tier 1 tests: rich hover/eldoc summary.  Asserts that
# `help-echo' on a fontified token in a non-ELOT buffer is a
# function that returns a rich one-line summary (id, label,
# rdf:type, [src: ...]) drawn from the DB.
step-1-14-test:
	$(call run-ert-test,elot-label-display-hover-test.el)

# Step 1.15 tests: disambiguating many ids sharing one label
# (two-stage selection; `C-u C-u' flat-presentation override;
# slurp path unchanged).
step-1-15-test:
	$(call run-ert-test,elot-label-lookup-stage-test.el)

# Step 1.16.1 tests: pure language-picker helper
# (`elot-db--select-by-language', `elot-db--effective-language-prefs',
# `elot-preferred-languages' defcustom + safe-local-variable predicate).
# No DB fixture needed; helper is pure.
step-1-16-1-test:
	$(call run-ert-test,elot-lang-prefs-test.el)

# Step 1.16.2 tests: schema v3 migration (additive `lang' column
# on `attributes' + idx_attrs_prop_lang index).  No readers or
# writers consume the column yet; proves additivity.
step-1-16-2-test:
	$(call run-ert-test,elot-db-schema-v3-test.el)

# Step 1.16.3 tests: writer accepts PLIST cons-form (VALUE LANG)
# and writes LANG to attributes.lang; bare strings continue to
# write lang = ''.  No reader consumes lang yet.
step-1-16-3-test:
	$(call run-ert-test,elot-db-lang-write-test.el)

# Step 1.16.4 tests: read path widened to SELECT lang and collapse
# via `elot-db--pick-value-by-lang' (which in 1.16.4 is `car', so
# behaviour is identical to the pre-widening path).
step-1-16-4-test:
	$(call run-ert-test,elot-db-lang-read-test.el)

# Step 1.16.5 tests: activate `elot-db--select-by-language' on the
# read path.  Affects `elot-db-get-attr', `elot-db-get-all-attrs',
# `elot-db-get-label' (now consults `rdfs:label' attribute rows
# when present), and `elot-db-all-active-labels'.
step-1-16-5-test:
	$(call run-ert-test,elot-db-lang-picker-test.el)

# Step 1.16.6 tests: CSV/TSV/JSON language conventions
# (`lang' header column for row-level tag; `label@TAG' header
# suffixes for per-column tagged rdfs:label rows).
step-1-16-6-test:
	$(call run-ert-test,elot-sources-lang-test.el)

# Step 1.16.7 tests: default TTL label-query projects LANG(?label),
# so TTL/RQ ingestion via ROBOT preserves language tags end-to-end.
# ROBOT-gated: ingestion tests skip cleanly if ROBOT is not available.
step-1-16-7-test:
	$(call run-ert-test,elot-sources-ttl-lang-test.el)

# Step 1.16.8 tests: stage-1 completion-display disambiguator.
# Annotator appends `@LANG' when a singleton label's id has
# multiple `rdfs:label' language variants in the winning source.
step-1-16-8-test:
	$(call run-ert-test,elot-label-lookup-lang-suffix-test.el)

# Step 1.10 tests: ELOT menu `Labels & sources' submenu
# (structure, entry resolution, :enable / :style toggle guards).
menu-test:
	$(call run-ert-test,elot-menu-test.el)

# Step 2.1 tests: canonical schema.sql file pins with the embedded
# fallback copy (byte-identical); fresh DB init via the file path
# produces the v3 shape.
step-2-1-test:
	$(call run-ert-test,elot-db-schema-sql-test.el)

# Step 2.2.5 slice 3a: byte-identical golden round-trip vs the TS
# writer.  Set ELOT_GOLDEN_REGEN=1 to (re)write the golden files.
golden-test:
	$(call run-ert-test,elot-db-golden-test.el)

# ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.2: pure-helper tests
# for `elot--sparql-merge-prefixes' (no ROBOT required).
sparql-merge-prefixes-test:
	$(call run-ert-test,elot-sparql-merge-prefixes-test.el)

# ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.4: pure-helper tests
# for the SPARQL advice failure modes (`elot--sparql-resolve-format',
# `elot--sparql-classify-url', `elot--sparql-result-empty-p').
# No ROBOT required.
sparql-exec-helpers-test:
	$(call run-ert-test,elot-sparql-exec-helpers-test.el)

# ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.5 / Acceptance check:
# verifies that the SPARQL around-advice on `org-babel-execute:sparql'
# is NOT installed at `(require 'elot)' time and is installed/removed
# only as a side effect of enabling/disabling `elot-mode' in a buffer.
# Reference-counted across multiple ELOT buffers.  No ROBOT required.
sparql-advice-test:
	$(call run-ert-test,elot-sparql-advice-test.el)

# ELPA-SUBMISSION-PLAN.org Milestone 1, Sub-step 1.6.2: end-to-end
# regression for the rewritten SPARQL advice against a real ROBOT
# invocation, driven by test/fixtures/sparql-roundtrip.org.
# ROBOT-gated: tests skip cleanly when ROBOT is absent.
sparql-roundtrip-test:
	$(call run-ert-test,elot-sparql-roundtrip-test.el)

# ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.7 Verification:
# tangle round-trip guard.  Re-tangles each literate source
# (elot-defs.org, elot-mode.org) into a temp dir and asserts the
# produced .el file is byte-identical to the checked-in copy under
# elot-package/.  No ROBOT required.
tangle-roundtrip-test:
	$(call run-ert-test,elot-tangle-roundtrip-test.el)

# ELPA-SUBMISSION-PLAN.org Milestone 4, Sub-step 4.2.B: assert
# `(require 'elot)' succeeds in a child Emacs with the optional
# external packages (sparql-mode, ob-sparql, ob-plantuml, htmlize,
# omn-mode, ht) all shadowed unavailable, and that `elot-mode' is
# bound afterwards.  No ROBOT, no network.
load-without-sparql-test:
	$(call run-ert-test,elot-load-without-sparql-test.el)

# ELOT-GPTEL-PLAN.org Milestone 1 Step 1.5: tests for elot-gptel.el
# (lint tool wrapper, path-traversal refusal, severity filter,
# registration helper).  No gptel install required.
gptel-test:
	$(call run-ert-test,elot-gptel-test.el)

# ELOT-GPTEL-PLAN.org Milestone 2 Steps 2.1-2.2: shared ROBOT
# process layer.  Live ROBOT tests skip cleanly when ROBOT is
# unavailable.
robot-test:
	$(call run-ert-test,elot-robot-test.el)

# ELOT-GPTEL-PLAN.org Milestone 3 Steps 3.1-3.2: elot_omn_validate
# tool.  Live ROBOT tests skip cleanly when ROBOT is unavailable.
omn-validate-test:
	$(call run-ert-test,elot-omn-validate-test.el)

# ELOT-GPTEL-PLAN.org Milestone 8 Step 8.4: elot_omn_report tool
# (thin wrapper around `robot report'; defers query catalogue to
# ROBOT).  Live ROBOT tests skip cleanly when ROBOT is unavailable.
omn-report-test:
	$(call run-ert-test,elot-gptel-omn-report-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.5: elot_diff tool (thin
# wrapper around `robot diff'; defers structural-diff content to
# ROBOT).  Live ROBOT tests skip cleanly when ROBOT is unavailable.
gptel-diff-test:
	$(call run-ert-test,elot-gptel-diff-test.el)

# ELOT-GPTEL-PLAN.org Milestone 4 Step 4.1: elot_sparql tool.
# Live ROBOT tests skip cleanly when ROBOT is unavailable.
sparql-tool-test:
	$(call run-ert-test,elot-sparql-tool-test.el)

# ELOT-GPTEL-PLAN.org Milestone 5 Step 5.1: elot_unsatisfiable
# (and, when 5.2 lands, elot_consistency).  Live ROBOT tests skip
# cleanly when ROBOT is unavailable.
reasoning-test:
	$(call run-ert-test,elot-reasoning-test.el)

# ELOT-GPTEL-PLAN.org Milestone 6 Step 6.1: read-only SQL gate
# (`elot-db-execute-readonly').  Pure tests, no ROBOT.
readonly-test:
	$(call run-ert-test,elot-db-readonly-test.el)

# ELOT-GPTEL-PLAN.org Milestone 6 Step 6.2: elot_db_query tool.
# Pure tests for the cell/row formatter plus integration tests
# against a seeded SQLite cache; no ROBOT required.
db-query-tool-test:
	$(call run-ert-test,elot-gptel-db-query-test.el)

# ELOT-GPTEL-PLAN.org Milestone 6 Step 6.3: elot_db_schema tool.
# Pure tests that the fenced SQL block matches the canonical
# schema.sql and that the stored/code-side schema_version are
# both reported; no ROBOT required.
db-schema-tool-test:
	$(call run-ert-test,elot-gptel-db-schema-test.el)

# ELOT-GPTEL-PLAN.org Milestone 6 Step 6.4 (Wave A): convenience
# shims `elot_db_get_label', `elot_db_list_sources',
# `elot_db_expand_curie'.  Pure-Elisp tests, no ROBOT required.
db-shims-test:
	$(call run-ert-test,elot-gptel-db-shims-test.el)

# ELOT-GPTEL-PLAN.org Milestone 6 Step 6.4 (Wave B): attribute-driven
# SQL-select shims `elot_db_get_attributes', `elot_db_supertypes',
# `elot_db_individual_types'.  Pure-Elisp tests, no ROBOT required.
db-shims-wave-b-test:
	$(call run-ert-test,elot-gptel-db-shims-wave-b-test.el)

# ELOT-GPTEL-PLAN.org Milestone 6 Step 6.5 (first half):
# `elot_db_search_label' -- reuse-before-mint cross-ontology
# search across the entire DB.  Pure-Elisp tests, no ROBOT required.
db-search-test:
	$(call run-ert-test,elot-gptel-db-search-test.el)

# ELOT-GPTEL-PLAN.org Milestone 6 Step 6.5 (second half):
# `elot_db_borrow_term' -- ELOT description-list snippet with
# `rdfs:isDefinedBy' back-pointer for reusing a DB-known term.
# Pure-Elisp tests, no ROBOT required.
db-borrow-test:
	$(call run-ert-test,elot-gptel-db-borrow-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.7: composite
# `elot_borrow_term' -- one-call search + borrow.
# Pure-Elisp tests, no ROBOT required.
db-borrow-compose-test:
	$(call run-ert-test,elot-gptel-db-borrow-compose-test.el)

# ELOT-GPTEL-PLAN.org Milestone 7.5 Step 7.5.2: optional `content'
# argument on `elot_lint' and `elot_omn_validate'.  Pure-Elisp tests
# for path resolution (existence-less), size cap, content-override-
# disk semantics, traversal refusal, and tool-spec/dispatcher arity.
# No ROBOT required for the lint half; the omn-validate dispatcher
# arity check stays pure too.
lint-content-test:
	$(call run-ert-test,elot-gptel-lint-content-test.el)

# ELOT-GPTEL-PLAN.org Milestone 7.5 Step 7.5.1: `elot_conventions'
# tool.  Pure-Elisp tests; no ROBOT required.  Verifies the on-disk
# `elot-conventions.md' resolves, the tool returns its contents
# verbatim, the embedded fallback is non-empty, and the tool spec
# is registered with zero args.
conventions-test:
	$(call run-ert-test,elot-gptel-conventions-test.el)

# ELOT-GPTEL-PLAN.org Milestone 7.5 Step 7.5.6: composite
# `elot_check' tool (lint -> OMN parse -> consistency ->
# unsatisfiable, fail-fast).  Pure-Elisp tests covering the
# stage-classifier helpers and end-to-end framing with the
# atomic tools stubbed via `cl-letf'.  No ROBOT required.
check-test:
	$(call run-ert-test,elot-gptel-check-test.el)

# Step 9.2.b.0 tests: signature/keyword lint diagnostics pinned for
# the LLM authoring loop (shared kind <-> section bridge, actionable
# undeclared-CURIE wording, crafted fixture, pets.org clean run).
lint-axiom-signature-test:
	$(call run-ert-test,elot-lint-axiom-signature-test.el)

# Milestone 8 (OOPS!-for-lint sliver): elot/oops-missing-annotations
# (P08), elot/oops-recursive-definition (P24), elot/oops-duplicate-labels
# (P32) -- inline OOPS!-derived checkers in elot-lint.el.  Pure-Elisp;
# no ROBOT required.
lint-oops-test:
	$(call run-ert-test,elot-lint-oops-test.el)

# ELOT-GPTEL-PLAN.org Milestone 8 Step 8.1: external-quality test
# corpus scaffolding -- discovery + provenance-drawer parsing +
# meta-test that every fixture under test/fixtures/corpus/ has a
# well-formed drawer and a resolvable `:corpus-expects:'.  Pure
# Elisp; no ROBOT required.
corpus-test:
	$(call run-ert-test,elot-corpus-test.el)

# ELOT-GPTEL-PLAN.org Milestone 8 Step 8.2: end-to-end harness over
# `test/fixtures/corpus/oops/' that drives `elot_lint' against every
# citation-bearing OOPS! fixture and asserts the expected
# `elot/oops-*' checker fires (and no sibling OOPS! checker fires
# spuriously).  Pure Elisp; no ROBOT required.
corpus-oops-test:
	$(call run-ert-test,elot-corpus-oops-test.el)

# Step 7.5.4 tests: elot/subclass-in-description-list lint checker.
subclass-in-dl-test:
	$(call run-ert-test,elot-lint-subclass-in-dl-test.el)

# Milestone 10 tests: pluggable identifier minting (`elot-id.el')
# plus the LLM-facing `elot_mint_identifier' / `elot_verify_identifier'
# tools.  Pure-Elisp; no ROBOT required.
id-test:
	$(call run-ert-test,elot-id-test.el)

# Milestone 10 Step 10.6 tests: interactive insert-resource commands
# `elot-insert-sibling-resource' / `elot-insert-child-resource'.
# Pure-Elisp; no ROBOT required.
id-insert-test:
	$(call run-ert-test,elot-id-insert-test.el)

# Milestone 10 Step 10.7 tests: per-ontology scheme picker
# `elot-id-set-scheme' / `elot-id-show-scheme'.  Pure-Elisp.
id-set-scheme-test:
	$(call run-ert-test,elot-id-set-scheme-test.el)

# Milestone 12 Step 12.4 tests: LLM-facing `elot_rename_resource'
# gptel tool wrapper.  Pure-Elisp; lint/OMN-validate stages are
# stubbed so no ROBOT is required.
gptel-rename-test:
	$(call run-ert-test,elot-gptel-rename-test.el)

# Milestone 12 Step 12.1 tests: `elot-move-resource' -- relocate a
# resource heading within an ontology, within the current file.
# Pure-Elisp; no ROBOT required.
move-resource-test:
	$(call run-ert-test,elot-move-resource-test.el)

# Milestone 12 Step 12.1 tests: LLM-facing `elot_move_resource'
# gptel tool wrapper.  Pure-Elisp; lint/OMN-validate stages are
# stubbed so no ROBOT is required.
gptel-move-test:
	$(call run-ert-test,elot-gptel-move-test.el)

# Milestone 9 Step 9.3 tests: LLM-facing `elot_insert_sibling_resource',
# `elot_insert_child_resource', `elot_insert_resource_tree' gptel
# tool wrappers.  Pure-Elisp; lint/OMN-validate stages are stubbed
# so no ROBOT is required.
gptel-insert-test:
	$(call run-ert-test,elot-gptel-insert-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.a: tests for the
# read-only `elot_axiom_keywords' authoring helper.  Pure Elisp;
# no ROBOT, no DB.
gptel-axiom-keywords-test:
	$(call run-ert-test,elot-gptel-axiom-keywords-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.b: tests for the
# read-only `elot_axiom_check' pre-flight validator.  ROBOT-gated
# OK-path tests skip when ROBOT is not configured.
gptel-axiom-check-test:
	$(call run-ert-test,elot-gptel-axiom-check-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.c: tests for the
# mutating `elot_edit_axiom' writer.  Pure-Elisp; lint /
# OMN-validate stages are stubbed so no ROBOT is required.
gptel-edit-axiom-test:
	$(call run-ert-test,elot-gptel-edit-axiom-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.c.1: tests for the
# batch `elot_edit_axioms' writer (atomic + dry-run).  Pure-Elisp;
# lint / OMN-validate stages are stubbed so no ROBOT is required.
gptel-edit-axioms-test:
	$(call run-ert-test,elot-gptel-edit-axioms-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F5: tests for the
# `elot-gptel-confirm-mutations' defcustom and the
# `elot-gptel--confirm-effective-p' policy helper.
gptel-confirm-mutations-test:
	$(call run-ert-test,elot-gptel-confirm-mutations-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F8: O(1) cache-staleness
# primitives (`elot-headline-hierarchy-mark-stale' /
# `elot-headline-hierarchy-ensure-fresh') plus the chained
# insert-then-rename / move-then-rename agent-loop regressions.
# Pure-Elisp; no ROBOT required.
hh-stale-test:
	$(call run-ert-test,elot-headline-hierarchy-stale-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F2: tangle path silently
# skips description-list rows whose KEY is an OMN frame keyword
# (Domain / Range / SubClassOf / DisjointClasses / ...) and whose
# VALUE is blank.  Pure-Elisp predicate + emission tests; no ROBOT
# required.
tangle-blank-omn-row-test:
	$(call run-ert-test,elot-tangle-blank-omn-row-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F4: companion lint
# checker `elot/blank-omn-axiom-row' that warns about blank
# description-list rows whose KEY is an OMN frame keyword.
# Pure-Elisp; no ROBOT required.
lint-blank-omn-row-test:
	$(call run-ert-test,elot-lint-blank-omn-row-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.1: `elot_db_activate_source'
# -- register-if-needed + activate FILE as a label source.  Pure-Elisp;
# no ROBOT required.
gptel-db-activate-source-test:
	$(call run-ert-test,elot-gptel-db-activate-source-test.el)

# Milestone 12 Step 12.4 tests: `elot-rename-resource' --
# substitute one resource CURIE for another, everywhere it
# appears in an ELOT .org file.  Pure-Elisp; the cross-prefix
# test uses an explicit `:target-iri' argument so no live
# `elot-db' is required.
id-rename-test:
	$(call run-ert-test,elot-id-rename-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9: shared reference-scan
# helper `elot-gptel--delete-reference-scan' -- "what would dangle
# if SUBJECT were deleted?".  Pure-Elisp; no ROBOT required.
gptel-delete-reference-scan-test:
	$(call run-ert-test,elot-gptel-delete-reference-scan-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9.F1: parent-enumeration
# helper `elot-gptel--replace--immediate-parents' for the
# `elot_replace_with_parent' guard.  Pure-Elisp; no ROBOT required.
gptel-replace-parents-test:
	$(call run-ert-test,elot-gptel-replace-parents-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9.F1: `elot_replace_with_parent'
# tool wrapper -- thin composite over `elot_rename_resource' with a
# parent-guard.  The underlying rename is stubbed in these tests; no
# ROBOT required.
gptel-replace-with-parent-test:
	$(call run-ert-test,elot-gptel-replace-with-parent-test.el)

# ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9 Slice A.3: `op=merge'
# mode on `elot_rename_resource' / `elot-rename-resource' --
# rewrite SOURCE -> TARGET when TARGET is already declared,
# remove SOURCE's heading, promote SOURCE's children one outline
# level.  Pure-Elisp; lint / OMN-validate stages are stubbed so
# no ROBOT is required.
gptel-rename-merge-test:
	$(call run-ert-test,elot-gptel-rename-merge-test.el)

# Milestone 9 Step 9.9 Slice B.1 tests: `elot-gptel--delete-apply'
# core primitive -- byte-level heading delete + child handling.
# Pure-Elisp; no ROBOT required (no revalidate at the B.1 layer).
gptel-delete-apply-test:
	$(call run-ert-test,elot-gptel-delete-apply-test.el)

# Milestone 9 Step 9.9 Slice B.2 tests: `elot_delete_resource'
# singleton tool wrapper -- pre-flight reference scan + cascade
# branch + write-back contract.  Lint / OMN-validate stages are
# stubbed so no ROBOT is required.
gptel-delete-resource-test:
	$(call run-ert-test,elot-gptel-delete-resource-test.el)

clean:
	rm -f *.elc

help:
	@echo "Targets:"
	@echo "  all          (default) run all ELOT ERT tests"
	@echo "  test-helper-guard-test  run test/elot-test-helper-guard-test.el (rollout regression guard)"
	@echo "  db-test      run test/elot-db-test.el (Step 1.1)"
	@echo "  prefix-test  run test/elot-db-prefixes-test.el (Step 1.1.1)"
	@echo "  crud-test    run test/elot-db-crud-test.el (Step 1.2)"
	@echo "  remove-source-test  run test/elot-db-remove-source-test.el (briefing Task 2.6)"
	@echo "  sources-test run test/elot-sources-test.el (Step 1.3)"
	@echo "  register-test run test/elot-sources-register-test.el (Step 1.4)"
	@echo "  active-sources-test run test/elot-active-sources-test.el (Step 1.5)"
	@echo "  slurp-integration-test run test/elot-slurp-integration-test.el (Step 1.6)"
	@echo "  global-mode-test run test/elot-global-mode-test.el (Step 1.7)"
	@echo "  active-sources-ui-test run test/elot-active-sources-ui-test.el (Step 1.7.2)"
	@echo "  ttl-prefix-test run test/elot-ttl-prefix-test.el (Step 1.7.3)"
	@echo "  menu-test    run test/elot-menu-test.el (Step 1.10)"
	@echo "  step-1-11-test run test/elot-step-1-11-test.el (Step 1.11)"
	@echo "  step-1-12-test run test/elot-label-lookup-test.el (Step 1.12 Item A)"
	@echo "  step-1-12-item-b-test run test/elot-menu-item-b-test.el (Step 1.12 Item B)"
	@echo "  step-1-13-test run test/elot-label-lookup-scope-test.el (Step 1.13)"
	@echo "  step-1-14-test run test/elot-label-display-hover-test.el (Step 1.14 Tier 1)"
	@echo "  step-1-15-test run test/elot-label-lookup-stage-test.el (Step 1.15)"
	@echo "  step-1-16-1-test run test/elot-lang-prefs-test.el (Step 1.16.1)"
	@echo "  step-1-16-2-test run test/elot-db-schema-v3-test.el (Step 1.16.2)"
	@echo "  step-1-16-3-test run test/elot-db-lang-write-test.el (Step 1.16.3)"
	@echo "  step-1-16-4-test run test/elot-db-lang-read-test.el (Step 1.16.4)"
	@echo "  step-1-16-5-test run test/elot-db-lang-picker-test.el (Step 1.16.5)"
	@echo "  step-1-16-6-test run test/elot-sources-lang-test.el (Step 1.16.6)"
	@echo "  step-1-16-7-test run test/elot-sources-ttl-lang-test.el (Step 1.16.7)"
	@echo "  step-1-16-8-test run test/elot-label-lookup-lang-suffix-test.el (Step 1.16.8)"
	@echo "  step-2-1-test run test/elot-db-schema-sql-test.el (Step 2.1)"
	@echo "  gptel-test   run test/elot-gptel-test.el (Milestone 1 Step 1.5)"
	@echo "  robot-test   run test/elot-robot-test.el (Milestone 2 Steps 2.1-2.2)"
	@echo "  omn-validate-test run test/elot-omn-validate-test.el (Milestone 3 Steps 3.1-3.2)"
	@echo "  omn-report-test   run test/elot-gptel-omn-report-test.el (Milestone 8 Step 8.4)"
	@echo "  gptel-diff-test   run test/elot-gptel-diff-test.el (Milestone 9 Step 9.5)"
	@echo "  sparql-tool-test  run test/elot-sparql-tool-test.el (Milestone 4 Step 4.1)"
	@echo "  reasoning-test    run test/elot-reasoning-test.el (Milestone 5 Steps 5.1, 5.2)"
	@echo "  readonly-test     run test/elot-db-readonly-test.el (Milestone 6 Step 6.1)"
	@echo "  db-query-tool-test run test/elot-gptel-db-query-test.el (Milestone 6 Step 6.2)"
	@echo "  db-schema-tool-test run test/elot-gptel-db-schema-test.el (Milestone 6 Step 6.3)"
	@echo "  db-shims-test run test/elot-gptel-db-shims-test.el (Milestone 6 Step 6.4 Wave A)"
	@echo "  db-shims-wave-b-test run test/elot-gptel-db-shims-wave-b-test.el (Milestone 6 Step 6.4 Wave B)"
	@echo "  db-search-test    run test/elot-gptel-db-search-test.el (Milestone 6 Step 6.5 first half)"
	@echo "  db-borrow-test    run test/elot-gptel-db-borrow-test.el (Milestone 6 Step 6.5 second half)"
	@echo "  db-borrow-compose-test run test/elot-gptel-db-borrow-compose-test.el (Milestone 9 Step 9.7)"
	@echo "  conventions-test  run test/elot-gptel-conventions-test.el (Milestone 7.5 Step 7.5.1)"
	@echo "  lint-content-test run test/elot-gptel-lint-content-test.el (Milestone 7.5 Step 7.5.2)"
	@echo "  check-test        run test/elot-gptel-check-test.el (Milestone 7.5 Step 7.5.6)"
	@echo "  subclass-in-dl-test run test/elot-lint-subclass-in-dl-test.el (Milestone 7.5 Step 7.5.4)"
	@echo "  lint-oops-test    run test/elot-lint-oops-test.el (Milestone 8 -- OOPS! P08/P24/P32 sliver)"
	@echo "  corpus-test       run test/elot-corpus-test.el (Milestone 8 Step 8.1 -- corpus scaffolding meta-test)"
	@echo "  corpus-oops-test  run test/elot-corpus-oops-test.el (Milestone 8 Step 8.2 -- OOPS! corpus harness)"
	@echo "  lint-axiom-signature-test run test/elot-lint-axiom-signature-test.el (Milestone 9 Step 9.2.b.0)"
	@echo "  id-test           run test/elot-id-test.el (Milestone 10)"
	@echo "  id-insert-test    run test/elot-id-insert-test.el (Milestone 10 Step 10.6)"
	@echo "  id-set-scheme-test run test/elot-id-set-scheme-test.el (Milestone 10 Step 10.7)"
	@echo "  id-rename-test     run test/elot-id-rename-test.el (Milestone 12 Step 12.4)"
	@echo "  gptel-rename-test  run test/elot-gptel-rename-test.el (Milestone 12 Step 12.4 -- gptel tool)"
	@echo "  move-resource-test run test/elot-move-resource-test.el (Milestone 12 Step 12.1)"
	@echo "  gptel-move-test    run test/elot-gptel-move-test.el (Milestone 12 Step 12.1 -- gptel tool)"
	@echo "  gptel-insert-test  run test/elot-gptel-insert-test.el (Milestone 9 Step 9.3 -- gptel tools)"
	@echo "  gptel-confirm-mutations-test  run test/elot-gptel-confirm-mutations-test.el (Milestone 9 Step 9.3.F5)"
	@echo "  hh-stale-test  run test/elot-headline-hierarchy-stale-test.el (Milestone 9 Step 9.3.F8)"
	@echo "  tangle-blank-omn-row-test  run test/elot-tangle-blank-omn-row-test.el (Milestone 9 Step 9.3.F2)"
	@echo "  lint-blank-omn-row-test  run test/elot-lint-blank-omn-row-test.el (Milestone 9 Step 9.3.F4)"
	@echo "  gptel-db-activate-source-test  run test/elot-gptel-db-activate-source-test.el (Milestone 9 Step 9.1)"
	@echo "  golden-test  run test/elot-db-golden-test.el (Step 2.2.5 slice 3a)"
	@echo "               (set ELOT_GOLDEN_REGEN=1 to regenerate the goldens)"
	@echo "  sparql-merge-prefixes-test run test/elot-sparql-merge-prefixes-test.el (Milestone 1 Step 1.2)"
	@echo "  sparql-exec-helpers-test run test/elot-sparql-exec-helpers-test.el (Milestone 1 Step 1.4)"
	@echo "  sparql-advice-test run test/elot-sparql-advice-test.el (Milestone 1 Step 1.5)"
	@echo "  sparql-roundtrip-test run test/elot-sparql-roundtrip-test.el (Milestone 1 Sub-step 1.6.2)"
	@echo "  tangle-roundtrip-test run test/elot-tangle-roundtrip-test.el (Milestone 1 Step 1.7 Verification)"
	@echo "  load-without-sparql-test run test/elot-load-without-sparql-test.el (Milestone 4 Sub-step 4.2.B)"
	@echo "  clean        remove compiled artifacts"
	@echo "  help         show this help"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 test"
