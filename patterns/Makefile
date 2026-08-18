# patterns/Makefile -- vendored upstream ontologies for offline import closure.
#
# The files fetched here are VERBATIM upstream copies, not hand-written
# stubs.  catalog-v001.xml maps the ontology IRIs onto them so that ROBOT
# can resolve owl:imports deterministically and offline.
#
#   make vendor    fetch skos.ttl, prov.ttl, and iof-core.ttl when absent
#   make force     refresh all vendored ontologies from upstream
#   make check     verify each vendored file parses
#   make elot-check  run the composite ELOT check (lint -> OMN parse ->
#                  consistency -> unsatisfiable) over every pattern .org
#                  file; FILES=... to restrict to a subset
#   make tangle-patterns  tangle every *-pattern-*.org file to OMN
#   make test      run the pattern-library ERT integrity/check suite
#   make clean     remove the vendored files
#
# Requires: curl, and a `robot' executable on PATH.

CURL  ?= curl -fsSL --retry 3
ROBOT ?= robot
EMACS ?= emacs
PACKAGE_DIR ?= ../elot-package

# Pattern .org sources; override with e.g. FILES="SKOS-pattern-broader.org"
FILES ?= $(sort $(wildcard *.org))
PATTERN_FILES ?= $(sort $(wildcard *-pattern-*.org))

SKOS_IRI     := http://www.w3.org/2004/02/skos/core
PROV_IRI     := http://www.w3.org/ns/prov-o
IOF_CORE_URL := https://raw.githubusercontent.com/iofoundry/ontology/refs/heads/master/core/Core.rdf

VENDORED := skos.ttl prov.ttl iof-core.ttl

.PHONY: all vendor check elot-check tangle-patterns test clean force

all: vendor

vendor: $(VENDORED)

# SKOS core is served as RDF/XML from the namespace document; ask for
# Turtle and let ROBOT normalise if the server ignores the Accept header.
skos.ttl:
	$(CURL) -H 'Accept: text/turtle' '$(SKOS_IRI)' -o $@.tmp
	$(ROBOT) convert --input $@.tmp --format ttl --output $@
	rm -f $@.tmp

# PROV-O publishes Turtle directly at prov-o.ttl.
prov.ttl:
	$(CURL) -H 'Accept: text/turtle' '$(PROV_IRI).ttl' -o $@.tmp
	$(ROBOT) convert --input $@.tmp --format ttl --output $@
	rm -f $@.tmp

# IOF Core is published upstream as RDF/XML.  Vendor a Turtle conversion so
# patterns can import it through the local ROBOT catalog.
iof-core.ttl:
	$(CURL) '$(IOF_CORE_URL)' -o $@.tmp
	$(ROBOT) convert --input $@.tmp --format ttl --output $@
	rm -f $@.tmp

# Re-fetch even when the files are present.
force:
	rm -f $(VENDORED)
	$(MAKE) vendor

check: $(VENDORED)
	@for f in $(VENDORED); do \
	  echo "== $$f"; \
	  $(ROBOT) measure --input $$f --format tsv || exit 1; \
	done

# Batch equivalent of the `elot_check' gptel tool, one report per file.
# Exits non-zero if any file fails a stage.
elot-check:
	$(EMACS) --batch -L $(PACKAGE_DIR) -l ./check-patterns.el $(FILES)

# Tangle each pattern ontology using the same command as an interactive ELOT
# buffer.  A pattern source is any .org filename containing "-pattern-".
tangle-patterns:
	$(EMACS) --batch -L $(PACKAGE_DIR) -l elot-mode \
		--eval '(progn (dolist (file command-line-args-left) (with-current-buffer (find-file-noselect file) (org-mode) (elot-mode 1) (elot-tangle-buffer-to-omn) (kill-buffer))) (setq command-line-args-left nil))' \
		$(PATTERN_FILES)

# Run the same pattern-library tests used by the repository's normal ERT/CI
# suite.  Keeping the canonical test under test/ means `make test' at the
# repository root discovers it automatically, while this target gives the
# patterns/ directory a self-contained entry point.
test:
	$(MAKE) -C ../test EMACS=$(EMACS) elot-patterns-test.el

clean:
	rm -f $(VENDORED) *.tmp
