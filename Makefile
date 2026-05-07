# Top-level Makefile for ELOT MELPA-submission housekeeping.
#
# This file exists to give a single command for the checks that
# the MELPA submission plan requires after every milestone.  See
# ELPA-SUBMISSION-PLAN.org, section "Stable-status checklist".
#
# Targets:
#   stable-check  - run byte-compile + tests + load smoke test (the
#                   three checks of the stable-status checklist) and
#                   print a one-line PASS/FAIL summary at the end.
#   byte-compile  - byte-compile elot-package/*.el; warnings allowed,
#                   errors fail the target.
#   native-compile - native-compile elot-package/*.el (requires an
#                   Emacs built --with-native-compilation).  Honours
#                   the no-native-compile file-local cookie.
#   test          - delegate to test/Makefile (full ERT suite).
#   smoke         - load elot-mode in a fresh batch Emacs.
#   baseline      - capture the Baseline files described in
#                   ELPA-SUBMISSION-PLAN.org Steps B.1-B.3 under /tmp/.
#   clean         - remove .elc files left in elot-package/.
#   help          - this list.
#
# Override Emacs with:  make EMACS=emacs30 stable-check

EMACS        ?= emacs
PACKAGE_DIR  := elot-package
EL_FILES     := $(wildcard $(PACKAGE_DIR)/*.el)

BASELINE_DIR    ?= /tmp
BASELINE_WARN   := $(BASELINE_DIR)/elot-baseline-warnings.txt
BASELINE_TESTS  := $(BASELINE_DIR)/elot-baseline-tests.txt
BASELINE_SYMS   := $(BASELINE_DIR)/elot-baseline-symbols.txt

# Output of the most recent stable-check, retained so a milestone
# acceptance check can paste it into the plan's example block.
STABLE_LOG_DIR  := tmp/stable-check
BC_LOG          := $(STABLE_LOG_DIR)/byte-compile.log
TEST_LOG        := $(STABLE_LOG_DIR)/test.log
SMOKE_LOG       := $(STABLE_LOG_DIR)/smoke.log

.PHONY: all stable-check byte-compile native-compile test smoke baseline \
        baseline-warnings baseline-tests baseline-symbols \
        clean help

all: help

## ---------------------------------------------------------------------------
## Stable-status checklist (run after every milestone)
## ---------------------------------------------------------------------------

stable-check:
	@mkdir -p $(STABLE_LOG_DIR)
	@echo "==> [1/3] byte-compile  ($(BC_LOG))"
	@$(MAKE) --no-print-directory byte-compile > $(BC_LOG) 2>&1; \
	  bc_status=$$?; \
	  warn_count=$$(grep -cE "Warning:" $(BC_LOG) || true); \
	  err_count=$$(grep -cE "Error:"   $(BC_LOG) || true); \
	  echo "    warnings: $$warn_count   errors: $$err_count   exit: $$bc_status"; \
	  echo "==> [2/3] tests         ($(TEST_LOG))"; \
	  $(MAKE) --no-print-directory test > $(TEST_LOG) 2>&1; \
	  t_status=$$?; \
	  failed=$$(grep -cE "^Test .* (failed|aborted)" $(TEST_LOG) || true); \
	  passed=$$(grep -cE "^Test .* passed"           $(TEST_LOG) || true); \
	  echo "    passed: $$passed   failed: $$failed   exit: $$t_status"; \
	  echo "==> [3/3] load smoke    ($(SMOKE_LOG))"; \
	  $(MAKE) --no-print-directory smoke > $(SMOKE_LOG) 2>&1; \
	  s_status=$$?; \
	  echo "    exit: $$s_status"; \
	  echo ""; \
	  echo "==> Summary"; \
	  if [ $$bc_status -eq 0 ] && [ $$t_status -eq 0 ] && [ $$s_status -eq 0 ]; then \
	    echo "    PASS  (byte-compile clean, tests green, mode loads)"; \
	    echo "    Logs: $(STABLE_LOG_DIR)/"; \
	    exit 0; \
	  else \
	    echo "    FAIL  byte-compile=$$bc_status  test=$$t_status  smoke=$$s_status"; \
	    echo "    See logs under $(STABLE_LOG_DIR)/"; \
	    exit 1; \
	  fi

## ---------------------------------------------------------------------------
## Individual checks
## ---------------------------------------------------------------------------

# Byte-compile every .el under elot-package/ from a fresh Emacs.
# Errors fail; warnings are merely counted (compare to baseline).
byte-compile:
	cd $(PACKAGE_DIR) && $(EMACS) --batch -L . \
	  --eval "(setq byte-compile-error-on-warn nil)" \
	  -f batch-byte-compile $(notdir $(EL_FILES))

# Native-compile every .el under elot-package/ from a fresh Emacs.
# Requires an Emacs built --with-native-compilation; bails out with a
# clear message otherwise.  Files carrying the `no-native-compile: t'
# cookie are skipped automatically by `native-compile-async' /
# `batch-native-compile'.  Async compilation is forced synchronous so
# the target's exit status reflects compile success/failure.
native-compile:
	@$(EMACS) --batch --eval "(unless (and (fboundp 'native-comp-available-p) (native-comp-available-p)) (message \"ERROR: this Emacs has no native compilation support\") (kill-emacs 2))"
	@mkdir -p $(STABLE_LOG_DIR)
	cd $(PACKAGE_DIR) && $(EMACS) --batch -L . \
	  --eval "(setq native-comp-async-report-warnings-errors 'silent)" \
	  --eval "(setq native-comp-speed 2)" \
	  -f batch-native-compile $(notdir $(EL_FILES))

# Delegate to the existing per-test Makefile in test/.
test:
	$(MAKE) -C test EMACS=$(EMACS)

# Load elot-mode in a fresh batch Emacs - catches missing requires
# or top-level errors that byte-compile would not surface.
smoke:
	$(EMACS) --batch -L $(PACKAGE_DIR) \
	  --eval "(require 'elot-mode)" \
	  --eval "(message \"OK: elot-mode loaded\")"

## ---------------------------------------------------------------------------
## Baseline capture (run once, before Milestone 1; see Step B.1-B.3).
## ---------------------------------------------------------------------------

baseline: baseline-warnings baseline-tests baseline-symbols
	@echo "Baseline written:"
	@echo "  $(BASELINE_WARN)"
	@echo "  $(BASELINE_TESTS)"
	@echo "  $(BASELINE_SYMS)"

# B.1 - byte-compile warnings (count + full text).
baseline-warnings:
	@echo "==> Capturing byte-compile baseline -> $(BASELINE_WARN)"
	@( cd $(PACKAGE_DIR) && $(EMACS) --batch -L . \
	     --eval "(setq byte-compile-error-on-warn nil)" \
	     -f batch-byte-compile $(notdir $(EL_FILES)) ) \
	  > $(BASELINE_WARN) 2>&1 || true
	@warn_count=$$(grep -cE "Warning:" $(BASELINE_WARN) || true); \
	  err_count=$$(grep -cE "Error:"   $(BASELINE_WARN) || true); \
	  echo "# Captured $(shell date -Iseconds)"            >> $(BASELINE_WARN); \
	  echo "# Warnings: $$warn_count  Errors: $$err_count" >> $(BASELINE_WARN); \
	  echo "    warnings: $$warn_count  errors: $$err_count"

# B.2 - passing test list.
baseline-tests:
	@echo "==> Capturing test baseline -> $(BASELINE_TESTS)"
	@$(MAKE) --no-print-directory test > $(BASELINE_TESTS) 2>&1 || true
	@passed=$$(grep -cE "^Test .* passed"           $(BASELINE_TESTS) || true); \
	  failed=$$(grep -cE "^Test .* (failed|aborted)" $(BASELINE_TESTS) || true); \
	  echo "# Captured $(shell date -Iseconds)"           >> $(BASELINE_TESTS); \
	  echo "# Passed: $$passed  Failed/aborted: $$failed" >> $(BASELINE_TESTS); \
	  echo "    passed: $$passed  failed: $$failed"

# B.3 - public defuns / defvars / defcustoms per file.
baseline-symbols:
	@echo "==> Capturing symbol baseline -> $(BASELINE_SYMS)"
	@: > $(BASELINE_SYMS)
	@for f in $(EL_FILES); do \
	  echo "### $$f"                              >> $(BASELINE_SYMS); \
	  grep -nE "^\(def(un|var|custom|macro|group|subst) [a-zA-Z]" $$f \
	    | grep -vE "^\S+:\(def(un|var|custom|macro|subst) [a-zA-Z]+--" \
	    >> $(BASELINE_SYMS) || true; \
	  echo ""                                     >> $(BASELINE_SYMS); \
	done
	@count=$$(grep -cE "^[^#]" $(BASELINE_SYMS) || true); \
	  echo "    public symbols (approx): $$count"

## ---------------------------------------------------------------------------
## Misc
## ---------------------------------------------------------------------------

clean:
	rm -f $(PACKAGE_DIR)/*.elc
	rm -rf $(PACKAGE_DIR)/eln-cache

help:
	@echo "ELOT housekeeping targets (top-level Makefile):"
	@echo ""
	@echo "  make stable-check   run byte-compile + tests + load smoke,"
	@echo "                      print PASS/FAIL summary, logs in $(STABLE_LOG_DIR)/"
	@echo "  make byte-compile   byte-compile $(PACKAGE_DIR)/*.el"
	@echo "  make native-compile native-compile $(PACKAGE_DIR)/*.el"
	@echo "                      (requires Emacs --with-native-compilation)"
	@echo "  make test           full ERT suite (delegates to test/Makefile)"
	@echo "  make smoke          (require 'elot-mode) in a fresh batch Emacs"
	@echo "  make baseline       capture Step B.1-B.3 baselines under /tmp/"
	@echo "                      (override with BASELINE_DIR=...)"
	@echo "  make clean          remove .elc files in $(PACKAGE_DIR)"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 stable-check"
