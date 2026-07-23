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

# ---------------------------------------------------------------------------
# Single source of truth: the test files on disk.
#
# Every ERT test file is named `elot-*-test.el' and is run uniformly via
# `run-ert-test'.  Globbing them here means adding a new test file is
# picked up automatically -- there is no hand-maintained target list to
# keep in sync (and therefore no list-drift bug where `make test'
# silently drops tests it was never told about).
#
# ROBOT-gated files `skip-unless' cleanly when ROBOT is absent, so the
# globbed suite stays green without Java.  Setting ELOT_REQUIRE_ROBOT=1
# flips those skips into hard failures (see test-helper.el), giving CI a
# real ROBOT gate with zero duplicated test names.
# ---------------------------------------------------------------------------
TEST_FILES := $(wildcard elot-*-test.el)

.PHONY: all test clean help $(TEST_FILES)

all: test

# Run the full ELOT ERT test suite: every `elot-*-test.el' on disk.
test: $(TEST_FILES)

# Pattern recipe: run one test file.  The target name *is* the file
# name, so `make elot-db-test.el' runs just that file, and `make test'
# runs them all.
$(TEST_FILES):
	$(call run-ert-test,$@)

clean:
	rm -f *.elc

# NOTE: the help block below is hand-written *documentation only*.
# Unlike the old `test:' / `.PHONY' lists it carries no
# target-resolution logic, so it is not a drift risk for "which tests
# run" -- the glob above is the sole authority for that.
help:
	@echo "Targets:"
	@echo "  all          (default) run all ELOT ERT tests (glob: elot-*-test.el)"
	@echo "  test         same as all: run every elot-*-test.el on disk"
	@echo "  <file>.el    run a single test file, e.g. make elot-db-test.el"
	@echo "  clean        remove compiled artifacts"
	@echo "  help         show this help"
	@echo ""
	@echo "ROBOT gate:   ELOT_REQUIRE_ROBOT=1 make test"
	@echo "               ROBOT-gated tests then FAIL (instead of skipping)"
	@echo "               when ROBOT is unavailable."
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 test"
