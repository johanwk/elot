EMACS ?= emacs

PACKAGE_DIR := ../elot-package

.PHONY: all test db-test clean help

all: test

# Run the full ELOT ERT test suite (currently: elot-db).
test: db-test

# Step 1.1 tests: SQLite DB layer.
db-test:
	$(EMACS) --batch \
	  -L $(PACKAGE_DIR) \
	  -l ert \
	  -l elot-db-test.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

help:
	@echo "Targets:"
	@echo "  all       (default) run all ELOT ERT tests"
	@echo "  db-test   run test/elot-db-test.el (Step 1.1)"
	@echo "  clean     remove compiled artifacts"
	@echo "  help      show this help"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 test"
