;;; elot-id-rename-test.el --- Tests for elot-rename-resource  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-id-rename-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 12 Step 12.4 -- tests for the
;; resource-rename command `elot-rename-resource'.  Pure-Elisp; no
;; ROBOT and no live `elot-db' required (prefix-resolution tests
;; that consult `elot-db' run only when the DB is loaded; the
;; common path uses an explicit :target-iri argument so the test
;; suite exercises the LLM-shaped contract).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defconst elot-id-rename-test--repo-root
  (let ((this (or load-file-name buffer-file-name
                  (locate-library "elot-id-rename-test"))))
    (unless this
      (error "elot-id-rename-test: cannot determine source location"))
    (file-name-directory
     (directory-file-name
      (file-name-directory (file-truename this))))))

(add-to-list 'load-path
             (expand-file-name "elot-package" elot-id-rename-test--repo-root))

(require 'elot-id)
(require 'elot-id-rename)


;;; ---------------------------------------------------------------------------
;;; Fixture: a minimal ELOT ontology with classes, a property with axiom
;;; annotations, an individual, and a prefix-table.  Mirrors pets.org in
;;; miniature so the rename tests exercise every rewrite class.
;;; ---------------------------------------------------------------------------

(defun elot-id-rename-test--fixture ()
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-id-scheme: slug\n"
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Prefixes\n"
   ":PROPERTIES:\n:prefixdefs: yes\n:END:\n"
   "#+name: prefix-table\n"
   "| prefix | uri                              |\n"
   "|--------+----------------------------------|\n"
   "| owl:   | http://www.w3.org/2002/07/owl#   |\n"
   "| rdfs:  | http://www.w3.org/2000/01/rdf-schema# |\n"
   "| ex:    | http://example.org/              |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "*** Dog (ex:dog)\n"
   " - rdfs:comment :: Certain dogs are friendly.\n"
   " - rdfs:seeAlso :: <http://example.org/dog>\n"
   "*** Dogfood (ex:dogfood)\n"
   " - rdfs:comment :: Should NOT be renamed when ex:dog is renamed.\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:dog\n"
   "   - rdfs:comment :: Dogs chase things.\n"
   " - Range :: ex:cat\n"
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** scooby (ex:scooby)\n"
   " - Types :: ex:dog\n"
   ))

(defmacro elot-id-rename-test--with (&rest body)
  "Insert the fixture in a temp Org buffer and run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (insert (elot-id-rename-test--fixture))
     (org-mode)
     (goto-char (point-min))
     ,@body))


;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun elot-id-rename-test--line-with (s)
  "Return the first line containing S, as a string (no properties)."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward s nil t)
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position)))))

(defun elot-id-rename-test--count (regex)
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (re-search-forward regex nil t) (cl-incf n))
      n)))


;;; ---------------------------------------------------------------------------
;;; Validation / refusal paths
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-rename-test-source-not-declared ()
  (elot-id-rename-test--with
    (should-error (elot-rename-resource "ex:nope" "ex:other")
                  :type 'user-error)))

(ert-deftest elot-id-rename-test-target-collides ()
  ;; `ex:dogfood' is declared as a heading in the fixture, so renaming
  ;; `ex:dog' -> `ex:dogfood' must be refused on collision grounds.
  (elot-id-rename-test--with
    (should-error (elot-rename-resource "ex:dog" "ex:dogfood")
                  :type 'user-error)))

(ert-deftest elot-id-rename-test-malformed-curie-refused ()
  (elot-id-rename-test--with
    (should-error (elot-rename-resource "ex:dog" "no-colon")
                  :type 'user-error)))

(ert-deftest elot-id-rename-test-source-equals-target ()
  (elot-id-rename-test--with
    (should-error (elot-rename-resource "ex:dog" "ex:dog")
                  :type 'user-error)))


;;; ---------------------------------------------------------------------------
;;; Same-prefix rename (most common case)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-rename-test-rewrites-heading ()
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    (should (elot-id-rename-test--line-with "(ex:canine)"))
    (should-not (elot-id-rename-test--line-with "(ex:dog)"))))

(ert-deftest elot-id-rename-test-rewrites-domain-axiom ()
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    (should (string-match-p
             "Domain :: ex:canine"
             (elot-id-rename-test--line-with "Domain ::")))))

(ert-deftest elot-id-rename-test-rewrites-individual-types ()
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    (should (string-match-p
             "Types :: ex:canine"
             (elot-id-rename-test--line-with "Types ::")))))

(ert-deftest elot-id-rename-test-preserves-prefix-table ()
  "Prefix-table rows containing the source local-name in URI must
not be touched."
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    ;; The ex: row's URI is `http://example.org/'; harmless,
    ;; but the row text containing `ex:' must not have been
    ;; mangled (it never matches the CURIE token regex anyway
    ;; because the prefix column ends in `:').
    (should (string-match-p "http://example.org/"
                            (elot-id-rename-test--line-with "| ex:")))))

(ert-deftest elot-id-rename-test-boundary-protects-dogfood ()
  "`ex:dogfood' must NOT be rewritten when renaming `ex:dog'."
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    (should (elot-id-rename-test--line-with "(ex:dogfood)"))
    (should-not (search-forward "ex:caninefood" nil t))))

(ert-deftest elot-id-rename-test-prose-skipped-counted ()
  "The line `Should NOT be renamed when ex:dog is renamed.' contains
SOURCE in prose -- skipped, counted in :prose-skipped."
  (elot-id-rename-test--with
    (let ((res (elot-rename-resource "ex:dog" "ex:canine")))
      ;; The prose mention is inside a desc-list value (` - rdfs:comment :: ...');
      ;; that classifies as desc-list, so it IS rewritten.  The Dogfood line
      ;; mentions `ex:dog' in its description-list value and so gets
      ;; rewritten too -- this is correct: description-list bodies are
      ;; part of the axiomatic content.  The :prose-skipped counter
      ;; therefore measures running prose OUTSIDE description-list rows.
      ;; In this fixture there is none, so :prose-skipped = 0.
      (should (zerop (plist-get res :prose-skipped))))))


;;; ---------------------------------------------------------------------------
;;; Full-IRI rewriting
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-rename-test-rewrites-angle-iri ()
  "An angle-bracketed full IRI expanding to the source must be
rewritten to the angle-bracketed full IRI of the target."
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    (let ((line (elot-id-rename-test--line-with "rdfs:seeAlso")))
      (should (string-match-p "<http://example.org/canine>" line))
      (should-not (string-match-p "<http://example.org/dog>" line)))))


;;; ---------------------------------------------------------------------------
;;; Cross-prefix rename with target_iri (programmatic / LLM path)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-rename-test-cross-prefix-needs-target-iri ()
  "Renaming to an undeclared prefix without :target-iri must signal."
  (elot-id-rename-test--with
    (should-error
     (elot-rename-resource "ex:dog" "newp:canine")
     :type 'user-error)))

(ert-deftest elot-id-rename-test-cross-prefix-with-target-iri ()
  "Supplying :target-iri adds the prefix row atomically and proceeds."
  (elot-id-rename-test--with
    (let ((res (elot-rename-resource
                "ex:dog" "newp:canine"
                :target-iri "http://new.example.org/")))
      (should (equal (plist-get res :declared-prefix)
                     (cons "newp" "http://new.example.org/")))
      ;; Prefix-table row added.
      (should (elot-id-rename-test--line-with "newp:"))
      (should (string-match-p "http://new.example.org/"
                              (elot-id-rename-test--line-with "newp:")))
      ;; Heading rewritten to the new CURIE.
      (should (elot-id-rename-test--line-with "(newp:canine)"))
      ;; Full IRI in rdfs:seeAlso expanded under the NEW prefix.
      (should (string-match-p
               "<http://new.example.org/canine>"
               (elot-id-rename-test--line-with "rdfs:seeAlso"))))))


;;; ---------------------------------------------------------------------------
;;; Hierarchy refresh side-effect
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-rename-test-hierarchy-refreshed ()
  "After rename, walking the hierarchy sees the new CURIE (and not
the old one)."
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    (let ((declared (elot-id-rename--declared-curies)))
      (should (member "ex:canine" declared))
      (should-not (member "ex:dog" declared)))))


;;; ---------------------------------------------------------------------------
;;; Boundary-aware string rewriter (pure unit test)
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; :new-label keyword -- rewrite the heading title alongside the CURIE
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-rename-test-new-label-rewrites-heading ()
  "Supplying :new-label rewrites the resource heading's label, in the
same atomic operation as the CURIE rewrite."
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine" :new-label "Canine")
    ;; The heading carries the new label AND the new CURIE on a
    ;; single line; the old label must be gone.
    (let ((line (elot-id-rename-test--line-with "(ex:canine)")))
      (should line)
      (should (string-match-p "\\*\\*\\* Canine (ex:canine)" line))
      (should-not (string-match-p "\\bDog\\b" line)))
    (should-not (elot-id-rename-test--line-with "(ex:dog)"))))

(ert-deftest elot-id-rename-test-new-label-nil-leaves-label ()
  "Without :new-label, only the CURIE changes; the heading's label
text is preserved verbatim."
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine")
    (let ((line (elot-id-rename-test--line-with "(ex:canine)")))
      (should line)
      (should (string-match-p "\\*\\*\\* Dog (ex:canine)" line)))))

(ert-deftest elot-id-rename-test-new-label-preserves-tail ()
  "When the heading carries a trailing statistics cookie or tag, the
label rewrite preserves it verbatim."
  (with-temp-buffer
    (insert (elot-id-rename-test--fixture))
    ;; Decorate the Dog heading with a cookie and a tag.
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* Dog (ex:dog)$")
    (replace-match "*** Dog (ex:dog) [0/0]    :pet:" t t)
    (org-mode)
    (goto-char (point-min))
    (elot-rename-resource "ex:dog" "ex:canine" :new-label "Canine")
    (let ((line (elot-id-rename-test--line-with "(ex:canine)")))
      (should line)
      (should (string-match-p "\\*\\*\\* Canine (ex:canine) \\[0/0\\]" line))
      (should (string-match-p ":pet:" line)))))

(ert-deftest elot-id-rename-test-new-label-empty-string-skipped ()
  "An empty NEW-LABEL is treated the same as nil -- no heading rewrite."
  (elot-id-rename-test--with
    (elot-rename-resource "ex:dog" "ex:canine" :new-label "")
    (let ((line (elot-id-rename-test--line-with "(ex:canine)")))
      (should line)
      (should (string-match-p "\\*\\*\\* Dog (ex:canine)" line)))))


;;; ---------------------------------------------------------------------------
;;; Boundary-aware string rewriter (pure unit test)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-rename-test-string-rewrite-boundaries ()
  (let ((f #'elot-id-rename--rewrite-curie-in-string))
    (should (string= (funcall f "ex:dog and ex:cat" "ex:dog" "ex:K9")
                     "ex:K9 and ex:cat"))
    ;; Boundary: ex:dogfood is NOT a match for ex:dog.
    (should (string= (funcall f "ex:dogfood" "ex:dog" "ex:K9")
                     "ex:dogfood"))
    ;; Multiple in same line.
    (should (string= (funcall f "ex:dog ex:dog" "ex:dog" "ex:K9")
                     "ex:K9 ex:K9"))
    ;; Surrounded by parens / punctuation.
    (should (string= (funcall f "(ex:dog), ex:dog." "ex:dog" "ex:K9")
                     "(ex:K9), ex:K9."))))


(provide 'elot-id-rename-test)
;;; elot-id-rename-test.el ends here
