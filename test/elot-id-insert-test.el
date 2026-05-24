;;; elot-id-insert-test.el --- Tests for elot-insert-{sibling,child}-resource  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-id-insert-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 10 Step 10.6 -- tests for the
;; interactive insert-resource commands.  Each test sets up an Org
;; buffer in-memory (no disk fixture required), invokes the command,
;; and asserts on heading shape, point placement, scheme behaviour,
;; and description-list inheritance.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defconst elot-id-insert-test--repo-root
  (let ((this (or load-file-name buffer-file-name
                  (locate-library "elot-id-insert-test"))))
    (unless this
      (error "elot-id-insert-test: cannot determine source location"))
    (file-name-directory
     (directory-file-name
      (file-name-directory (file-truename this)))))
  "Absolute path to the ELOT repo root, captured at load time.")

(add-to-list 'load-path
             (expand-file-name "elot-package" elot-id-insert-test--repo-root))

(require 'elot-id)
(require 'elot-id-insert)


;;; ---------------------------------------------------------------------------
;;; Fixture helpers
;;; ---------------------------------------------------------------------------

(defun elot-id-insert-test--fixture (scheme-spec &optional extra)
  "Return an ELOT-shaped Org buffer string.
SCHEME-SPEC is the value of `:ELOT-id-scheme:'.  EXTRA, when non-nil,
is inserted after the Animal subtree (used to test
sibling-after-subtree semantics)."
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-id-scheme: " scheme-spec "\n"
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   " - [X] iof-av:naturalLanguageDefinition :: A living organism\n"
   " - [X] iof-av:isPrimitive :: true\n"
   " - DisjointClasses :: ex:dog\n"
   "**** Dog (ex:dog)\n"
   " - [X] iof-av:naturalLanguageDefinition :: A domesticated mammal\n"
   (or extra "")
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - [X] skos:example :: Fido chases Max\n"
   "** Datatypes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-datatypes\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** myType (ex:myType)\n"
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** spooky (ex:spooky)\n"
   " - Types :: ex:chases some owl:Thing\n"))

(defmacro elot-id-insert-test--with-buffer (scheme-spec headline-search-re &rest body)
  "Run BODY in a temp Org buffer seeded by `elot-id-insert-test--fixture'.
Point is moved to the first match of HEADLINE-SEARCH-RE before BODY runs."
  (declare (indent 2))
  `(with-temp-buffer
     (insert (elot-id-insert-test--fixture ,scheme-spec))
     (org-mode)
     (goto-char (point-min))
     (re-search-forward ,headline-search-re nil t)
     (beginning-of-line)
     (require 'elot-tangle nil 'noerror)
     ,@body))


;;; ---------------------------------------------------------------------------
;;; Section + scheme + prefix detection
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-section-classes ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (should (string= (elot-id-insert--section-name) "Classes"))))

(ert-deftest elot-id-insert-test-section-on-section-heading ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\* Classes"
    (should (string= (elot-id-insert--section-name) "Classes"))))

(ert-deftest elot-id-insert-test-section-deep-child ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\*\\* Dog"
    (should (string= (elot-id-insert--section-name) "Classes"))))

(ert-deftest elot-id-insert-test-section-individuals ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* spooky"
    (should (string= (elot-id-insert--section-name) "Individuals"))))

(ert-deftest elot-id-insert-test-scheme-spec-inherited ()
  (elot-id-insert-test--with-buffer "counter GO_0000000" "^\\*\\*\\* Animal"
    (should (string= (elot-id-insert--scheme-spec) "counter GO_0000000"))
    (should (string= (elot-id-insert--prefix) "ex"))))


;;; ---------------------------------------------------------------------------
;;; Sibling insert under each section
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-sibling-classes-counter ()
  (elot-id-insert-test--with-buffer "counter GO_0000000" "^\\*\\*\\* Animal"
    (let ((curies (elot-id-insert--do-insert nil 1 '("Plant"))))
      (should (= (length curies) 1))
      (should (string-match-p "\\`ex:GO_[0-9]\\{7\\}\\'" (car curies)))
      ;; Heading line shape: stars + space + LABEL + space + (curie)
      (beginning-of-line)
      (should (looking-at
               (concat "^\\*\\*\\* Plant ("
                       (regexp-quote (car curies))
                       ")\\s-*$"))))))

(ert-deftest elot-id-insert-test-sibling-objprops ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* chases"
    (let ((curies (elot-id-insert--do-insert nil 1 '("eats"))))
      (should (string-prefix-p "ex:" (car curies)))
      (beginning-of-line)
      (should (looking-at "^\\*\\*\\* eats (")))))

(ert-deftest elot-id-insert-test-sibling-datatypes ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* myType"
    (let ((curies (elot-id-insert--do-insert nil 1 '("otherType"))))
      (should (= (length curies) 1)))))

(ert-deftest elot-id-insert-test-sibling-individuals ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* spooky"
    (let ((curies (elot-id-insert--do-insert nil 1 '("creepy"))))
      (should (= (length curies) 1)))))


;;; ---------------------------------------------------------------------------
;;; Child insert + refusal under datatypes/individuals
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-child-under-class ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (let ((curies (elot-id-insert--do-insert t 1 '("Mammal"))))
      (should (= (length curies) 1))
      (beginning-of-line)
      (should (looking-at "^\\*\\*\\*\\* Mammal (")))))

(ert-deftest elot-id-insert-test-child-refused-datatype-resource ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* myType"
    (should-error (elot-id-insert--do-insert t 1 '("sub")) :type 'user-error)))

(ert-deftest elot-id-insert-test-child-refused-individual-resource ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* spooky"
    (should-error (elot-id-insert--do-insert t 1 '("sub")) :type 'user-error)))

(ert-deftest elot-id-insert-test-child-allowed-on-section-heading ()
  "Child insert directly on the level-2 section heading is permitted
\(it seeds the first resource), even for Datatypes / Individuals."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\* Datatypes"
    (let ((curies (elot-id-insert--do-insert t 1 '("newType"))))
      (should (= (length curies) 1)))))


;;; ---------------------------------------------------------------------------
;;; Numeric prefix N: batch of headings, all distinct CURIEs
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-batch-of-5-counter ()
  (elot-id-insert-test--with-buffer "counter GO_0000000" "^\\*\\*\\* Animal"
    (let ((curies (elot-id-insert--do-insert
                   nil 5 '("A" "B" "C" "D" "E"))))
      (should (= (length curies) 5))
      ;; All distinct and monotone.
      (should (equal curies
                     '("ex:GO_0000001" "ex:GO_0000002"
                       "ex:GO_0000003" "ex:GO_0000004"
                       "ex:GO_0000005"))))))

(ert-deftest elot-id-insert-test-batch-point-on-first ()
  "After an N=3 batch insert, point sits at end of the first heading line."
  (elot-id-insert-test--with-buffer "counter GO_0000000" "^\\*\\*\\* Animal"
    (let* ((curies (elot-id-insert--do-insert nil 3 '("A" "B" "C")))
           (line (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
      (should (string-match (concat "\\`\\*\\*\\* A ("
                                    (regexp-quote (car curies))
                                    ")")
                            line))
      ;; Point should be at end of line (after the closing paren).
      (should (= (point) (line-end-position))))))


;;; ---------------------------------------------------------------------------
;;; Description-list inheritance
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-desc-inherited-blanked ()
  "Sibling under Animal inherits annotation keys blanked.
OMN axiom keywords (`DisjointClasses', `SubClassOf', ...) are NOT
inherited per Step 9.3.F3 -- a blank OMN-axiom row tangles to
malformed OMN."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (require 'elot-tangle)
    (elot-id-insert--do-insert nil 1 '("Plant"))
    ;; Point is on the new heading line; collect the description list
    ;; that follows.
    (let* ((start (line-beginning-position))
           (end (save-excursion (org-end-of-subtree t t) (point)))
           (text (buffer-substring-no-properties start end)))
      (should (string-match-p
               "^ - \\[ \\] iof-av:naturalLanguageDefinition ::$" text))
      (should (string-match-p
               "^ - \\[ \\] iof-av:isPrimitive ::$" text))
      ;; OMN axiom keyword row is dropped from the inherited template.
      (should-not (string-match-p "DisjointClasses" text)))))

(ert-deftest elot-id-insert-test-desc-no-list-stays-bare ()
  "Sibling under a heading without a description list inserts a bare heading."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* myType"
    (elot-id-insert--do-insert nil 1 '("another"))
    (let* ((start (line-beginning-position))
           (end (save-excursion (org-end-of-subtree t t) (point)))
           (text (buffer-substring-no-properties start end)))
      (should-not (string-match-p "^ -" text)))))

(ert-deftest elot-id-insert-test-nested-rows-not-replicated ()
  "Nested annotation rows under top-level keys are NOT replicated.
The top-level OMN axiom keyword (`Domain') is also dropped per
Step 9.3.F3."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* chases"
    (require 'elot-tangle)
    ;; chases has only a top-level `skos:example' line, no nested
    ;; rows here -- but we add one inline before calling.
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* chases" nil t)
    (forward-line 1)
    ;; Insert a Domain axiom with nested annotation rows.
    (end-of-line)
    (insert "\n - Domain :: ex:dog"
            "\n   - rdfs:comment :: nested annotation row")
    ;; Move back to the chases heading and invoke.
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* chases" nil t)
    (beginning-of-line)
    (elot-id-insert--do-insert nil 1 '("eats"))
    ;; The new heading must NOT contain the nested rdfs:comment
    ;; nor the blank Domain row.
    (let* ((start (line-beginning-position))
           (end (save-excursion (org-end-of-subtree t t) (point)))
           (text (buffer-substring-no-properties start end)))
      (should-not (string-match-p "rdfs:comment" text))
      (should-not (string-match-p "Domain" text)))))


;;; ---------------------------------------------------------------------------
;;; Scheme-resolution errors
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-missing-scheme-errors ()
  "Without `:ELOT-id-scheme:' the command refuses."
  (with-temp-buffer
    (insert "* my-ont\n"
            ":PROPERTIES:\n"
            ":ELOT-context-type: ontology\n"
            ":ELOT-default-prefix: ex\n"
            ":END:\n"
            "** Classes\n"
            ":PROPERTIES:\n"
            ":resourcedefs: yes\n"
            ":END:\n"
            "*** Animal (ex:animal)\n")
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* Animal" nil t)
    (beginning-of-line)
    ;; The command now prompts via `elot-id-set-scheme' when the property
    ;; is missing; stub it to a no-op so the spec remains unset, then the
    ;; secondary guard signals user-error.
    (cl-letf (((symbol-function 'elot-id-set-scheme)
               (lambda () (interactive) nil)))
      (should-error (elot-id-insert--do-insert nil 1 '("X")) :type 'user-error))))

(ert-deftest elot-id-insert-test-not-in-section-errors ()
  "Invoked outside any recognised section, the command refuses."
  (with-temp-buffer
    (insert "* my-ont\n"
            ":PROPERTIES:\n"
            ":ELOT-context-type: ontology\n"
            ":ELOT-id-scheme: slug\n"
            ":ELOT-default-prefix: ex\n"
            ":END:\n"
            "** Queries\n"
            "*** select * { ?s ?p ?o }\n")
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* select" nil t)
    (beginning-of-line)
    (should-error (elot-id-insert--do-insert nil 1 '("X")) :type 'user-error)))


;;; ---------------------------------------------------------------------------
;;; Sibling semantics: appears AFTER the subtree
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-sibling-after-subtree ()
  "Sibling of Animal lands after Animal's whole subtree (after Dog),
not between Animal and Dog."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (elot-id-insert--do-insert nil 1 '("Plant"))
    ;; The buffer order must be: Animal, Dog, NEW.
    (goto-char (point-min))
    (let ((animal-pos (progn (re-search-forward "^\\*\\*\\* Animal") (point)))
          (dog-pos    (progn (re-search-forward "^\\*\\*\\*\\* Dog") (point)))
          (new-pos    (progn (re-search-forward "^\\*\\*\\* Plant (ex:") (point))))
      (should (< animal-pos dog-pos))
      (should (< dog-pos new-pos)))))


;;; ---------------------------------------------------------------------------
;;; Bad N argument
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-zero-defaults-to-one ()
  "N=0 from a non-prefix invocation defaults to 1."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (let ((curies (elot-id-insert--do-insert nil 0 '("Plant"))))
      (should (= (length curies) 1)))))

(ert-deftest elot-id-insert-test-labels-rendered-in-heading ()
  "Each label is rendered verbatim in the corresponding heading title."
  (elot-id-insert-test--with-buffer "counter GO_0000000" "^\\*\\*\\* Animal"
    (elot-id-insert--do-insert nil 3 '("Fish" "Bird" "Reptile"))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* Fish (ex:GO_0000001)" nil t))
    (should (re-search-forward "^\\*\\*\\* Bird (ex:GO_0000002)" nil t))
    (should (re-search-forward "^\\*\\*\\* Reptile (ex:GO_0000003)" nil t))))

(ert-deftest elot-id-insert-test-slug-derived-from-label ()
  "Under `slug', the local name is derived from the supplied label."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (let ((curies (elot-id-insert--do-insert nil 1 '("Hedgehog"))))
      (should (equal curies '("ex:hedgehog"))))))

(ert-deftest elot-id-insert-test-acme-slug-uses-label ()
  "Under `acme slug:t', the acme slug is derived from the supplied label."
  (elot-id-insert-test--with-buffer "acme slug:t" "^\\*\\*\\* Animal"
    (let* ((curies (elot-id-insert--do-insert nil 1 '("Hedgehog")))
           (curie (car curies)))
      ;; Shape: ex:C_<5lower><9alnum>
      (should (string-match-p
               "\\`ex:C_[a-z]\\{5\\}[0-9A-Z]\\{9\\}\\'"
               curie))
      ;; The slug component should begin with `hedge' (5-char label window).
      (should (string-match "\\`ex:C_\\([a-z]\\{5\\}\\)" curie))
      (should (string= (match-string 1 curie) "hedge")))))

(ert-deftest elot-id-insert-test-empty-label-rejected ()
  "An empty / blank label in the LABELS list is rejected."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (should-error (elot-id-insert--do-insert nil 2 '("OK" "   "))
                  :type 'user-error)))


;;; ---------------------------------------------------------------------------
;;; Tree builder
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-insert-test-tree-normalize-leaf ()
  (should (equal (elot-id-insert--normalize-tree-node "Dog")
                 '("Dog"))))

(ert-deftest elot-id-insert-test-tree-normalize-branch ()
  (should (equal (elot-id-insert--normalize-tree-node '("Dog" "Beagle" "Poodle"))
                 '("Dog" "Beagle" "Poodle"))))

(ert-deftest elot-id-insert-test-tree-normalize-rejects-junk ()
  (should-error (elot-id-insert--normalize-tree-node 42)
                :type 'user-error))

(ert-deftest elot-id-insert-test-tree-flat-siblings ()
  "A flat tree (only leaves) inserts N siblings, one per element."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (let ((curies (elot-insert-labels-tree
                   '("Hedgehog" "Otter" "Badger"))))
      (should (= (length curies) 3))
      (dolist (lbl '("Hedgehog" "Otter" "Badger"))
        (goto-char (point-min))
        (should (re-search-forward
                 (concat "^\\*\\*\\* " (regexp-quote lbl) " (")
                 nil t))))))

(ert-deftest elot-id-insert-test-tree-nested ()
  "Nested nodes generate children one outline level deeper."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (elot-insert-labels-tree
     '(("Bird" "Sparrow" "Eagle")
       ("Fish" "Trout")))
    ;; Top-level siblings at *** level.
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* Bird (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* Fish (" nil t))
    ;; Children at **** level under their parents.
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\* Sparrow (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\* Eagle (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\* Trout (" nil t))))

(ert-deftest elot-id-insert-test-tree-deep ()
  "Three levels of nesting produce three deeper outline levels."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (elot-insert-labels-tree
     '(("Mammal"
        ("Carnivore" "Lion" "Tiger")
        "Herbivore")))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* Mammal (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\* Carnivore (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\* Herbivore (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\*\\* Lion (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\*\\* Tiger (" nil t))))

(ert-deftest elot-id-insert-test-tree-as-child ()
  "AS=`child' makes the top-level nodes children of the heading at point."
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (elot-insert-labels-tree '("Beagle" "Poodle") 'child)
    ;; Children at **** under Animal.
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\* Beagle (" nil t))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\*\\* Poodle (" nil t))))

(ert-deftest elot-id-insert-test-tree-empty-rejected ()
  (elot-id-insert-test--with-buffer "slug" "^\\*\\*\\* Animal"
    (should-error (elot-insert-labels-tree '()) :type 'user-error)))


(provide 'elot-id-insert-test)
;;; elot-id-insert-test.el ends here
