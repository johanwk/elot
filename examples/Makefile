# examples/Makefile — Declarative round-trip testing for ELOT examples
#
# Usage:
#   make                   # download, convert, round-trip, diff — everything
#   make -j4               # parallel processing
#   make fetch-sources     # just fetch/copy source ontologies
#   make clean             # remove derived files (keep downloads)
#   make distclean         # remove everything including downloads
#
# Configuration (override via environment):
#   ELOT_EXPORTER_JAR  — path to elot-exporter jar
#   ROBOT              — robot command (wrapper script or "java -jar robot.jar")
#   EMACS              — Emacs binary
#   ELOT_PACKAGE_DIR   — path to the elot-package/ elisp directory

SHELL := /bin/bash
.DELETE_ON_ERROR:

# ── Tool paths (override via environment) ────────────────────────────
ELOT_EXPORTER_JAR ?= java -jar ~/bin/elot-exporter.jar
ROBOT             ?= java -jar ~/bin/robot.jar
EMACS             ?= emacs
ELOT_PACKAGE_DIR  ?= ../elot-package

# ── Directories ──────────────────────────────────────────────────────
SOURCES_DIR := sources
ORG_DIR     := org
OUTPUT_DIR  := output
REPORTS_DIR := reports

# ── Derived version / timestamp ──────────────────────────────────────
ELOT_VERSION ?= $(shell git -C .. describe --tags --always 2>/dev/null || echo unknown)
TIMESTAMP     = $(shell date -u +%Y-%m-%dT%H:%M:%SZ)

# ── Read ontology list ───────────────────────────────────────────────
# Each non-comment, non-blank line: id <TAB> source_type <TAB> source_uri
ENTRIES := $(shell awk -F'\t' '/^[^#]/ && NF>=3 {print $$1}' ontology-list.tsv)

# ── Phony targets ────────────────────────────────────────────────────
.PHONY: all fetch-sources baselines org roundtrip diff report clean distclean help

all: diff report  ## Full pipeline: download → convert → export → tangle → diff → report

fetch-sources:  ## Download all remote source ontologies
	@echo "All sources fetched."

baselines: $(patsubst %,$(SOURCES_DIR)/%-baseline.omn,$(ENTRIES))  ## Convert all sources to baseline OMN via robot

org: $(patsubst %,$(ORG_DIR)/%.org,$(ENTRIES))  ## Export all sources to ELOT Org format

roundtrip: $(patsubst %,$(OUTPUT_DIR)/%.omn,$(ENTRIES))  ## Tangle all Org files back to OMN

diff: $(patsubst %,$(REPORTS_DIR)/%.json,$(ENTRIES)) $(patsubst %,$(REPORTS_DIR)/%.diff.html,$(ENTRIES))  ## Run robot diff and write JSON + HTML reports

report: roundtrip-report.md  ## Generate roundtrip-report.md from reports

help:  ## Show this help
	@echo "ELOT Examples — Round-Trip Testing"
	@echo ""
	@echo "Targets:"
	@grep -E '^[a-zA-Z_%-]+:.*?## ' $(MAKEFILE_LIST) | \
		sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'
	@echo ""
	@echo "Per-ontology targets (replace <id> with ontology name from ontology-list.tsv):"
	@printf "  %-28s %s\n" "<id>.roundtrip" "Run full pipeline for a single ontology"
	@printf "  %-28s %s\n" "sources/<id>.<ext>" "Download/copy a single source (extension from URI)"
	@printf "  %-28s %s\n" "sources/<id>-baseline.omn" "Convert a single source to baseline OMN"
	@printf "  %-28s %s\n" "org/<id>.org" "Export a single source to Org"
	@printf "  %-28s %s\n" "output/<id>.omn" "Tangle a single Org to OMN"
	@echo ""
	@echo "Available ontology IDs and source files:"
	@awk -F'\t' '/^[^#]/ && NF>=3 { \
		printf "  %-36s ← %s  [%s]\n", $$1, $$3, $$2 \
	}' ontology-list.tsv
	@echo ""
	@echo "Configuration (override via environment):"
	@echo "  ELOT_EXPORTER_JAR  $(ELOT_EXPORTER_JAR)"
	@echo "  ROBOT              $(ROBOT)"
	@echo "  EMACS              $(EMACS)"
	@echo "  ELOT_PACKAGE_DIR   $(ELOT_PACKAGE_DIR)"
	@echo ""
	@echo "Examples:"
	@echo "  make                         # run full pipeline"
	@echo "  make -j4                     # run in parallel"
	@echo "  make pizza.roundtrip         # single ontology end-to-end"
	@echo "  make sources/pizza.owl       # download just pizza"
	@echo "  make fetch-sources           # download all"
	@echo "  make clean                   # remove derived files"
	@echo "  make distclean               # remove everything incl. downloads"

# ── Directory creation (order-only prerequisites) ────────────────────
# Using stamp files to avoid collisions with phony target names.
.stamps:
	@mkdir -p .stamps

.stamps/dir-sources: | .stamps
	@mkdir -p $(SOURCES_DIR)
	@touch $@

.stamps/dir-org: | .stamps
	@mkdir -p $(ORG_DIR)
	@touch $@

.stamps/dir-output: | .stamps
	@mkdir -p $(OUTPUT_DIR)
	@touch $@

.stamps/dir-reports: | .stamps
	@mkdir -p $(REPORTS_DIR)
	@touch $@

# ════════════════════════════════════════════════════════════════════
# STEP 1: Download / copy source ontology
# ════════════════════════════════════════════════════════════════════
# Source files keep their original extension from the URL/local path.
# A per-entry rule is generated from ontology-list.tsv at Makefile-read time.
# Files won't be re-downloaded as long as they exist on disk.
#
# Local entries whose source_uri already points into sources/ are
# treated as pre-existing (committed to Git); no copy/download rule
# is generated — the file is simply used where it sits.

# Helper: extract file extension from a URI.
# Recognizable extensions are kept; otherwise defaults to .owl
# e.g. http://example.com/ont.rdf → .rdf ; http://example.com/gistCore → .owl
_known_exts := .owl .rdf .ttl .omn .owx .ofn .obo .xml .jsonld .nt .nq
_get_ext = $(if $(filter $(_known_exts),$(suffix $(1))),$(suffix $(1)),.owl)

# --- URL download rule ---
# $(1)=id, $(2)=source_uri, $(3)=extension
# Fault-tolerant: logs a warning on failure instead of aborting,
# so other ontologies can still be processed.
define download_rule_url
$(SOURCES_DIR)/$(1)$(3): | .stamps/dir-sources
	@if curl -fsSL -o $$@.tmp \
		-H "Accept: application/rdf+xml, application/owl+xml, text/turtle, */*" \
		"$(2)"; then \
		mv $$@.tmp $$@; \
		echo "Downloaded: $(2) → $$@"; \
	else \
		rm -f $$@.tmp; \
		echo "WARNING: failed to download $(2) — skipping $(1)" >&2; \
	fi

fetch-sources: $(SOURCES_DIR)/$(1)$(3)
endef

# Parse ontology-list.tsv and emit download rules.
# Also build a map from id → source file path (SOURCE_FILE_<id>).
#
# For "local" entries:
#   - If source_uri is already under sources/, use it directly (no rule needed).
#   - The file is expected to exist and be committed to Git.
# For "url" entries:
#   - Generate a curl-based download rule.
$(foreach line,$(shell awk -F'\t' '/^[^#]/ && NF>=3 {print $$1 "|" $$2 "|" $$3}' ontology-list.tsv),\
  $(eval _id   := $(word 1,$(subst |, ,$(line))))\
  $(eval _type := $(word 2,$(subst |, ,$(line))))\
  $(eval _uri  := $(word 3,$(subst |, ,$(line))))\
  $(eval _ext  := $(call _get_ext,$(_uri)))\
  $(if $(filter local,$(_type)),\
    $(eval SOURCE_FILE_$(_id) := $(_uri)),\
    $(eval SOURCE_FILE_$(_id) := $(SOURCES_DIR)/$(_id)$(_ext))\
    $(eval $(call download_rule_url,$(_id),$(_uri),$(_ext)))))

# ════════════════════════════════════════════════════════════════════
# STEP 2: Normalize source to OMN (baseline for diffing)
# ════════════════════════════════════════════════════════════════════
# We can't use a simple pattern rule since source extensions vary.
# Generate a per-id rule: <id>-baseline.omn depends on SOURCE_FILE_<id>.
# If the source is already .omn, robot convert still normalizes it.
#
# Baseline OMN files go into sources/ so the diff step can find them.
define omn_convert_rule
$(SOURCES_DIR)/$(1)-baseline.omn: $$(SOURCE_FILE_$(1))
	@if $$(ROBOT) convert --input $$< --format omn --output $$@; then \
		echo "Baseline OMN: $$@"; \
	else \
		rm -f $$@; \
		echo "WARNING: robot convert failed for $(1) — skipping baseline" >&2; \
	fi
endef

$(foreach id,$(ENTRIES),$(eval $(call omn_convert_rule,$(id))))

# ════════════════════════════════════════════════════════════════════
# STEP 3: Export source to ELOT Org
# ════════════════════════════════════════════════════════════════════
# Uses elot-exporter.jar to convert OWL → Org.
# --force: always overwrite (these are generated files).
# Fault-tolerant: logs a warning on failure instead of aborting.
define org_export_rule
$(ORG_DIR)/$(1).org: $$(SOURCE_FILE_$(1)) | .stamps/dir-org
	@if $$(ELOT_EXPORTER_JAR) --force --tangle-target=../$(OUTPUT_DIR)/$(1).omn $$< -o $$@; then \
		echo "Exported to Org: $$@"; \
	else \
		rm -f $$@; \
		echo "WARNING: elot-exporter failed for $(1) — skipping Org export" >&2; \
	fi
endef

$(foreach id,$(ENTRIES),$(eval $(call org_export_rule,$(id))))

# ════════════════════════════════════════════════════════════════════
# STEP 4: Tangle Org back to OMN via Emacs batch
# ════════════════════════════════════════════════════════════════════
# Loads elot-mode (which pulls in elot-tangle) from ELOT_PACKAGE_DIR.
# Sets elot-robot-jar-path to "" so the post-tangle ROBOT→TTL step
# is skipped in batch mode (we only need the OMN here).
# The tangle target in the Org header points to ../output/<id>.omn,
# so the tangled file lands directly in output/.
$(OUTPUT_DIR)/%.omn: $(ORG_DIR)/%.org | .stamps/dir-output
	$(EMACS) --batch \
		--eval '(push (expand-file-name "$(ELOT_PACKAGE_DIR)") load-path)' \
		--eval '(load-library "elot-mode")' \
		--eval '(setq elot-robot-jar-path "")' \
		--visit "$<" \
		--eval '(elot-tangle-buffer-to-omn)' \
		2>&1 | tail -20
	@if [ ! -f "$@" ]; then \
		echo "ERROR: tangled OMN not found for $*" >&2; exit 1; \
	fi
	@echo "Tangled to OMN: $@"

# ════════════════════════════════════════════════════════════════════
# STEP 5: Diff original OMN vs round-tripped OMN
# ════════════════════════════════════════════════════════════════════
$(REPORTS_DIR)/%.diff: $(SOURCES_DIR)/%-baseline.omn $(OUTPUT_DIR)/%.omn | .stamps/dir-reports
	$(ROBOT) diff \
		--left $< \
		--right $(word 2,$^) \
		--output $@ 2>&1 || true
	@if [ -s $@ ]; then \
		echo "⚠️  DIFFERENCES: $*"; \
	else \
		echo "✅ IDENTICAL: $*"; \
	fi

# HTML-formatted diff report for human inspection
$(REPORTS_DIR)/%.diff.html: $(SOURCES_DIR)/%-baseline.omn $(OUTPUT_DIR)/%.omn | .stamps/dir-reports
	$(ROBOT) diff \
		--left $< \
		--right $(word 2,$^) \
		--format html \
		--output $@ 2>&1 || true
	@echo "HTML diff report: $@"

# ════════════════════════════════════════════════════════════════════
# STEP 6: Write per-ontology JSON report
# ════════════════════════════════════════════════════════════════════
$(REPORTS_DIR)/%.json: $(REPORTS_DIR)/%.diff ontology-list.tsv
	@source_type=$$(awk -F'\t' '/^$*\t/ {print $$2}' ontology-list.tsv); \
	source_uri=$$(awk -F'\t'  '/^$*\t/ {print $$3}' ontology-list.tsv); \
	diff_lines=0; diff_status="identical"; \
	if [ -s "$<" ]; then \
		diff_lines=$$(wc -l < "$<" | tr -d ' '); \
		diff_status="differences"; \
	fi; \
	printf '{\n  "id": "%s",\n  "source_type": "%s",\n  "source_uri": "%s",\n  "timestamp": "%s",\n  "elot_version": "%s",\n  "diff_status": "%s",\n  "diff_lines": %s,\n  "diff_file": "%s",\n  "diff_html": "%s"\n}\n' \
		"$*" "$$source_type" "$$source_uri" "$(TIMESTAMP)" "$(ELOT_VERSION)" \
		"$$diff_status" "$$diff_lines" "$*.diff" "$*.diff.html" > $@

# ════════════════════════════════════════════════════════════════════
# STEP 7: Generate roundtrip-report.md from all reports
# ════════════════════════════════════════════════════════════════════
roundtrip-report.md: $(patsubst %,$(REPORTS_DIR)/%.json,$(ENTRIES)) generate-roundtrip-report.sh
	bash generate-roundtrip-report.sh

# ════════════════════════════════════════════════════════════════════
# Convenience: per-ontology end-to-end target
# ════════════════════════════════════════════════════════════════════
%.roundtrip: $(REPORTS_DIR)/%.json
	@echo "Round-trip complete for $*"

# ════════════════════════════════════════════════════════════════════
# Cleanup
# ════════════════════════════════════════════════════════════════════
clean:  ## Remove derived files (keep downloads and baselines)
	rm -rf $(ORG_DIR) $(OUTPUT_DIR) $(REPORTS_DIR) .stamps
	rm -f roundtrip-report.md

clean-baselines:  ## Remove baseline OMN files
	rm -f $(SOURCES_DIR)/*-baseline.omn

distclean: clean  ## Remove everything incl. downloaded sources
	@# Remove downloaded (non-local) source files and baseline OMNs.
	@# Local files in sources/ are committed to Git and left alone.
	rm -f $(SOURCES_DIR)/*-baseline.omn
	@awk -F'\t' '/^[^#]/ && $$2=="url" && NF>=3 {print $$1}' ontology-list.tsv | \
		while read id; do rm -f $(SOURCES_DIR)/$$id.*; done
