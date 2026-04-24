EMACS ?= emacs

PACKAGE_DIR := ../elot-package

.PHONY: all test db-test prefix-test crud-test sources-test register-test active-sources-test slurp-integration-test global-mode-test active-sources-ui-test ttl-prefix-test menu-test step-1-11-test step-1-12-test step-1-12-item-b-test step-1-13-test step-1-14-test step-1-15-test step-1-16-1-test step-1-16-2-test step-1-16-3-test step-1-16-4-test clean help

all: test

# Run the full ELOT ERT test suite.
test: db-test prefix-test crud-test sources-test register-test active-sources-test slurp-integration-test global-mode-test active-sources-ui-test ttl-prefix-test menu-test step-1-11-test step-1-12-test step-1-12-item-b-test step-1-13-test step-1-14-test step-1-15-test step-1-16-1-test step-1-16-2-test step-1-16-3-test step-1-16-4-test

# Step 1.1 tests: SQLite DB layer.
db-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -l ert \
	  -l elot-db-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.1.1 tests: prefix handling (schema v2).
prefix-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -l ert \
	  -l elot-db-prefixes-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.2 tests: write/read primitives.
crud-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -l ert \
	  -l elot-db-crud-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.3 + 1.3.1 tests: source parsers (org / csv / tsv / json / rq-cache)
# plus ROBOT-local Tier 1 tests (skipped cleanly if ROBOT absent).
sources-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-sources-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.4 tests: source registration UI
# (register/unregister/refresh/refresh-all/list-sources).
register-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-sources-register-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.5 tests: per-project active-sources configuration.
active-sources-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-active-sources-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.6 tests: elot-slurp-to-vars integration with the DB +
# two-tier lookup in elot-codelist-id-label / elot-attriblist-label-value.
slurp-integration-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-slurp-integration-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.7 tests: elot-global-label-display-mode (DB-driven font-lock
# regex; decorates identifiers in any buffer / any major mode).
global-mode-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-global-mode-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.7.2 tests: active-sources priority UI
# (move-up/down, tabulated list, change hook, auto-repersist).
active-sources-ui-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-active-sources-ui-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.7.3 tests: TTL @prefix harvest + dual-form (IRI/CURIE)
# global-mode matcher + safety-net cap.  ROBOT-gated tests skip
# cleanly when ROBOT is absent.
ttl-prefix-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-ttl-prefix-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.11 tests: `elot-global-label-display-mode' polish
# (no-sources hint, collision resolution, autoload cookies).
step-1-11-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-step-1-11-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.12 Item A tests: `elot-label-lookup' generalisation
# (DB-backed label collection, dispatcher behaviour, autoload smoke test).
step-1-12-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-label-lookup-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.12 Item B tests: ELOT menu visibility in non-ELOT buffers
# (menu attachment to `elot-global-label-display-mode-map',
# `:enable' guards on ELOT-only entries, no duplicate menu when
# both modes are active).
step-1-12-item-b-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-menu-item-b-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.13 tests: scope choice (local / external / both) for
# `elot-label-lookup' + union dedup + wrapper commands.
step-1-13-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-label-lookup-scope-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.14 Tier 1 tests: rich hover/eldoc summary.  Asserts that
# `help-echo' on a fontified token in a non-ELOT buffer is a
# function that returns a rich one-line summary (id, label,
# rdf:type, [src: ...]) drawn from the DB.
step-1-14-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-label-display-hover-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.15 tests: disambiguating many ids sharing one label
# (two-stage selection; `C-u C-u' flat-presentation override;
# slurp path unchanged).
step-1-15-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-label-lookup-stage-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.16.1 tests: pure language-picker helper
# (`elot-db--select-by-language', `elot-db--effective-language-prefs',
# `elot-preferred-languages' defcustom + safe-local-variable predicate).
# No DB fixture needed; helper is pure.
step-1-16-1-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-lang-prefs-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.16.2 tests: schema v3 migration (additive `lang' column
# on `attributes' + idx_attrs_prop_lang index).  No readers or
# writers consume the column yet; proves additivity.
step-1-16-2-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-db-schema-v3-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.16.3 tests: writer accepts PLIST cons-form (VALUE LANG)
# and writes LANG to attributes.lang; bare strings continue to
# write lang = ''.  No reader consumes lang yet.
step-1-16-3-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-db-lang-write-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.16.4 tests: read path widened to SELECT lang and collapse
# via `elot-db--pick-value-by-lang' (which in 1.16.4 is `car', so
# behaviour is identical to the pre-widening path).
step-1-16-4-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-db-lang-read-test.el \
	  -f ert-run-tests-batch-and-exit

# Step 1.10 tests: ELOT menu `Labels & sources' submenu
# (structure, entry resolution, :enable / :style toggle guards).
menu-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -L . \
	  -l ert \
	  -l elot-menu-test.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

help:
	@echo "Targets:"
	@echo "  all          (default) run all ELOT ERT tests"
	@echo "  db-test      run test/elot-db-test.el (Step 1.1)"
	@echo "  prefix-test  run test/elot-db-prefixes-test.el (Step 1.1.1)"
	@echo "  crud-test    run test/elot-db-crud-test.el (Step 1.2)"
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
	@echo "  clean        remove compiled artifacts"
	@echo "  help         show this help"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 test"
