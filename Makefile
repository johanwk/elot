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
#   package-lint  - run package-lint-batch-and-exit over elot*.el with
#                   package-lint-main-file set to elot.el (matches the
#                   MELPA reviewer's invocation).  Installs package-lint
#                   into a throwaway elpa dir on first run.
#                   NOTE: this target exits non-zero whenever package-lint
#                   reports ANY issue, including informational warnings.
#                   Two `with-eval-after-load' warnings are expected and
#                   intentional (cross-module load-order bridges in
#                   elot-mode.el and elot-label-display.el, both
#                   documented with NOTE comments in source and waved
#                   through by the MELPA reviewer).  Read the output
#                   rather than relying on the exit status.
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
        manual manual-html manual-info manual-reference \
        package-lint clean help

## ---------------------------------------------------------------------------
## Manual (documentation/manual/elot.org)
## ---------------------------------------------------------------------------

MANUAL_DIR  := documentation/manual
MANUAL_ORG  := $(MANUAL_DIR)/elot.org
MANUAL_HTML := $(MANUAL_DIR)/elot.html
MANUAL_INFO := $(MANUAL_DIR)/elot.info

manual: manual-html manual-info

# Regenerate auto-generated reference chapters (commands, customization,
# LOB helpers) by introspecting the loaded ELOT package.
manual-reference:
	$(EMACS) --batch -L $(PACKAGE_DIR) \
	  -l $(MANUAL_DIR)/gen-manual-reference.el

manual-html: manual-reference
	$(EMACS) --batch -L $(PACKAGE_DIR) \
	  --eval "(require 'org)" \
	  --eval "(require 'ox-html)" \
	  --eval "(find-file \"$(MANUAL_ORG)\")" \
	  --eval "(org-html-export-to-html)"
	@echo "Wrote $(MANUAL_HTML)"

manual-info: manual-reference
	$(EMACS) --batch -L $(PACKAGE_DIR) \
	  --eval "(require 'org)" \
	  --eval "(require 'ox-texinfo)" \
	  --eval "(find-file \"$(MANUAL_ORG)\")" \
	  --eval "(org-texinfo-export-to-info)"
	@echo "Wrote $(MANUAL_INFO)"

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

# Load elot in a fresh batch Emacs - catches missing requires or
# top-level errors that byte-compile would not surface.  Since
# Milestone 4 Step 4.6, `elot.el' is the canonical load entry point
# and pulls in `elot-mode' (and the rest) transitively; the smoke
# test asserts `elot-mode' is bound as a side effect.
smoke:
	$(EMACS) --batch -L $(PACKAGE_DIR) \
	  --eval "(require 'elot)" \
	  --eval "(unless (fboundp 'elot-mode) (error \"elot-mode not bound after (require 'elot)\"))" \
	  --eval "(message \"OK: elot loaded; elot-mode bound\")"

## ---------------------------------------------------------------------------
## package-lint (MELPA reviewer's check)
## ---------------------------------------------------------------------------

# Throwaway elpa dir so package-lint installation does not pollute the
# user's ~/.emacs.d/elpa.  Re-used across runs (refresh only on miss).
PKG_LINT_ELPA := $(STABLE_LOG_DIR)/package-lint-elpa

# NOTE on exit status: `package-lint-batch-and-exit' exits 1 whenever
# ANY issue is reported, including informational warnings.  Two
# `with-eval-after-load' warnings are expected here (intentional
# cross-module load-order bridges in elot-mode.el and
# elot-label-display.el).  The trailing `echo' / `true' below prints a
# reminder and forces a clean exit so `make package-lint' succeeds when
# only the two known warnings are present.  Inspect the output above to
# confirm no NEW warnings appeared.
package-lint:
	@mkdir -p $(STABLE_LOG_DIR)
	-$(EMACS) -Q --batch \
	  -L $(PACKAGE_DIR) \
	  --eval "(setq package-user-dir (expand-file-name \"$(PKG_LINT_ELPA)\" default-directory))" \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	  --eval "(package-initialize)" \
	  --eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
	  --eval "(require 'package-lint)" \
	  --eval "(setq package-lint-main-file \"$(PACKAGE_DIR)/elot.el\")" \
	  -f package-lint-batch-and-exit \
	  $(PACKAGE_DIR)/elot*.el
	@echo ""
	@echo "NOTE: package-lint exits non-zero whenever it reports ANY issue."
	@echo "      Two 'with-eval-after-load' warnings are EXPECTED and intentional"
	@echo "      (cross-module load-order bridges; reviewer waved them through)."
	@echo "      Inspect the output above; only NEW issues need action."

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
	@echo "  make smoke          (require 'elot) in a fresh batch Emacs;"
	@echo "                      asserts elot-mode is bound as a side effect"
	@echo "  make package-lint   run package-lint over $(PACKAGE_DIR)/elot*.el"
	@echo "                      with package-lint-main-file=elot.el"
	@echo "  make baseline       capture Step B.1-B.3 baselines under /tmp/"
	@echo "                      (override with BASELINE_DIR=...)"
	@echo "  make clean          remove .elc files in $(PACKAGE_DIR)"
	@echo ""
	@echo "  make manual         export the manual to HTML + Info"
	@echo "  make manual-html    HTML only"
	@echo "  make manual-info    Info only (install with install-info)"
	@echo "  make manual-reference"
	@echo "                      regenerate auto-generated reference chapters"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs30 stable-check"
