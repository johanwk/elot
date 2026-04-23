EMACS ?= emacs

PACKAGE_DIR := ../elot-package

.PHONY: all test db-test prefix-test crud-test sources-test register-test active-sources-test slurp-integration-test global-mode-test active-sources-ui-test ttl-prefix-test menu-test step-1-11-test clean help

all: test

# Run the full ELOT ERT test suite.
test: db-test prefix-test crud-test sources-test register-test active-sources-test slurp-integration-test global-mode-test active-sources-ui-test ttl-prefix-test menu-test step-1-11-test

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
	@echo "  clean        remove compiled artifacts"
	@echo "  help         show this help"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 test"
