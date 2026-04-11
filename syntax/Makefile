EMACS ?= emacs

PEGGY_SRC   = owl-manchester.peggy
GRAMMAR_EL  = elot-owl-grammar.el
CONVERTER   = peggy-to-peg.el
TEST_FILE   = test-grammar.el
PACKAGE_DIR = ../elot-package

.PHONY: all grammar test install clean help

all: install test

# Regenerate elot-owl-grammar.el from the .peggy source
grammar: $(GRAMMAR_EL)

$(GRAMMAR_EL): $(PEGGY_SRC) $(CONVERTER)
	$(EMACS) --batch \
	  --eval '(require (quote cl-lib))' \
	  -l $(CONVERTER) \
	  --eval '(peggy-to-peg-convert-file "$(PEGGY_SRC)" "$(GRAMMAR_EL)")'

# Copy the generated grammar into elot-package/
install: $(GRAMMAR_EL)
	cp $(GRAMMAR_EL) $(PACKAGE_DIR)/$(GRAMMAR_EL)

# Run the test suite
test: $(GRAMMAR_EL)
	$(EMACS) --batch -l $(TEST_FILE)

clean:
	rm -f $(GRAMMAR_EL)

help:
	@echo "Targets:"
	@echo "  all       (default) runs install then test"
	@echo "  grammar   regenerate elot-owl-grammar.el from owl-manchester.peggy"
	@echo "  install   regenerate grammar, then copy it to ../elot-package/"
	@echo "  test      regenerate grammar, then run test-grammar.el"
	@echo "  clean     remove the generated elot-owl-grammar.el"
	@echo "  help      show this help"
	@echo ""
	@echo "Override Emacs:  make EMACS=emacs29 test"
