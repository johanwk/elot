EMACS ?= emacs

PACKAGE_DIR := ../elot-package

.PHONY: all test db-test prefix-test clean help

all: test

# Run the full ELOT ERT test suite (currently: elot-db + prefixes).
test: db-test prefix-test

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

clean:
	rm -f *.elc

help:
	@echo "Targets:"
	@echo "  all          (default) run all ELOT ERT tests"
	@echo "  db-test      run test/elot-db-test.el (Step 1.1)"
	@echo "  prefix-test  run test/elot-db-prefixes-test.el (Step 1.1.1)"
	@echo "  clean        remove compiled artifacts"
	@echo "  help         show this help"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 test"
