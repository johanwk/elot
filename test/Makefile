EMACS ?= emacs

PACKAGE_DIR := ../elot-package

.PHONY: all test db-test prefix-test crud-test sources-test register-test active-sources-test clean help

all: test

# Run the full ELOT ERT test suite.
test: db-test prefix-test crud-test sources-test register-test active-sources-test

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
	@echo "  clean        remove compiled artifacts"
	@echo "  help         show this help"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 test"
