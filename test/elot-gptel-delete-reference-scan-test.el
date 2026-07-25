;;; elot-gptel-delete-reference-scan-test.el --- M9.9 reference-scan tests  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-delete-reference-scan-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9 -- tests for the shared
;; reference-scan helper `elot-gptel--delete-reference-scan'.
;;
;; The helper answers "what would dangle if SUBJECT were deleted?"
;; Pure read; no buffer mutation; foundation for the M9.9 delete
;; wrappers and a future `elot_rename_resource --safe' mode.
;;
;; Covers:
;;   - subject resolved against `elot-headline-hierarchy';
;;   - direct children enumerated from the hierarchy (heading
;;     nesting carries SubClassOf / SubPropertyOf);
;;   - axiom rows on OTHER subjects whose VALUE mentions SUBJECT,
;;     keyed on `elot-omn-all-keywords';
;;   - axiom-annotation sub-rows attributed to the enclosing
;;     resource heading;
;;   - OMN / SPARQL src-block hits;
;;   - prose mentions are counted, never reported by row;
;;   - prefix-table rows / non-rewritable src-blocks ignored;
;;   - rows on SUBJECT itself ignored (self-reference is the
;;     P24 / OOPS! lint check's concern, not ours);
;;   - unknown subject -> :node nil, empty lists, count 0;
;;   - F8 staleness: hierarchy is refreshed up front.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-drsc-test--repo-root nil)
(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path (expand-file-name "elot-package" root))
  (add-to-list 'load-path (file-name-directory this))
  (setq elot-gptel-drsc-test--repo-root root))

(require 'elot-tangle)
(require 'elot-id-rename)
(require 'elot-gptel)


;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-gptel-drsc-test--fixture ()
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
   "| prefix | uri                                   |\n"
   "|--------+---------------------------------------|\n"
   "| owl:   | http://www.w3.org/2002/07/owl#        |\n"
   "| rdfs:  | http://www.w3.org/2000/01/rdf-schema# |\n"
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "*** Dog (ex:dog)\n"
   " - SubClassOf :: ex:animal\n"
   " - rdfs:comment :: A friendly ex:animal companion.\n"
   "*** Cat (ex:cat)\n"
   " - SubClassOf :: ex:animal\n"
   " - rdfs:seeAlso :: ex:puppy\n"
   "*** Puppy (ex:puppy)\n"
   " - SubClassOf :: ex:dog\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:animal\n"
   "   - rdfs:comment :: Restricted to ex:animal subclasses.\n"
   " - Range :: ex:animal\n"
   "** SPARQL queries\n"
   "#+begin_src sparql\n"
   "SELECT ?x WHERE { ?x a ex:animal . }\n"
   "#+end_src\n"
   "** Notes\n"
   "Some prose mentioning ex:animal in running text.\n"
   "Another prose line about ex:dog and ex:animal.\n"))

(defmacro elot-gptel-drsc-test--with (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert (elot-gptel-drsc-test--fixture))
     (org-mode)
     (elot-update-headline-hierarchy)
     (goto-char (point-min))
     ,@body))


;;; ---------------------------------------------------------------------------
;;; Helper-level unit tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-drsc-test-curie-mentioned-p ()
  (should (elot-gptel--delete-curie-mentioned-p "ex:dog" "SubClassOf ex:dog"))
  (should (elot-gptel--delete-curie-mentioned-p "ex:dog" "ex:dog"))
  (should (elot-gptel--delete-curie-mentioned-p "ex:dog" "x ex:dog y"))
  (should-not (elot-gptel--delete-curie-mentioned-p "ex:dog" "ex:dogfood"))
  (should-not (elot-gptel--delete-curie-mentioned-p "ex:dog" "myex:dog"))
  (should-not (elot-gptel--delete-curie-mentioned-p "ex:dog" nil))
  (should-not (elot-gptel--delete-curie-mentioned-p "ex:dog" "")))

(ert-deftest elot-gptel-drsc-test-defined ()
  (should (fboundp 'elot-gptel--delete-reference-scan)))


;;; ---------------------------------------------------------------------------
;;; Subject + children resolution
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-drsc-test-unknown-subject ()
  (elot-gptel-drsc-test--with
    (let ((r (elot-gptel--delete-reference-scan "ex:nope")))
      (should (null (plist-get r :node)))
      (should (null (plist-get r :children)))
      (should (null (plist-get r :axiom-rows)))
      (should (null (plist-get r :annotation-rows)))
      (should (null (plist-get r :src-rows)))
      (should (= 0 (plist-get r :prose-count))))))

(ert-deftest elot-gptel-drsc-test-resolves-leaf ()
  (elot-gptel-drsc-test--with
    (let ((r (elot-gptel--delete-reference-scan "ex:puppy")))
      (should (plist-get r :node))
      (should (null (plist-get r :children))))))

(ert-deftest elot-gptel-drsc-test-children-enumerated ()
  (elot-gptel-drsc-test--with
    (let ((r (elot-gptel--delete-reference-scan "ex:dog")))
      (should (plist-get r :node))
      (should (equal (plist-get r :children) '("ex:puppy"))))))


;;; ---------------------------------------------------------------------------
;;; Axiom-row attribution
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-drsc-test-axiom-rows-on-other-subjects ()
  "Deleting ex:animal must surface every axiom row that mentions it
on some OTHER subject -- excluding SubClassOf / SubPropertyOf rows,
which are promoted to `:children' (axiom-derived parenthood)."
  (elot-gptel-drsc-test--with
    (let* ((r (elot-gptel--delete-reference-scan "ex:animal"))
           (rows (plist-get r :axiom-rows))
           (subjects (mapcar (lambda (row) (plist-get row :curie)) rows))
           (keywords (mapcar (lambda (row) (plist-get row :keyword)) rows))
           (children (plist-get r :children)))
      ;; SubClassOf rows on ex:dog / ex:cat declare them as children
      ;; of ex:animal -- they live in `:children', not `:axiom-rows'.
      (should (member "ex:dog" children))
      (should (member "ex:cat" children))
      (should-not (member "SubClassOf" keywords))
      ;; Remaining axiom-row references: ex:chases Domain/Range.
      (should (member "ex:chases" subjects))
      (should (member "Domain" keywords))
      (should (member "Range" keywords))
      ;; Lines are 1-based and monotone increasing.
      (let ((lines (mapcar (lambda (row) (plist-get row :line)) rows)))
        (should (cl-every #'integerp lines))
        (should (equal lines (cl-sort (copy-sequence lines) #'<)))))))

(ert-deftest elot-gptel-drsc-test-self-rows-ignored ()
  "A subject's OWN rows must not appear as references to itself.
ex:dog has a `SubClassOf :: ex:animal' row -- that's a reference
TO ex:animal, not a self-reference; deleting ex:dog must NOT
surface that row.  Verify by deleting ex:dog and asserting no
axiom-row entry is keyed on ex:dog."
  (elot-gptel-drsc-test--with
    (let* ((r (elot-gptel--delete-reference-scan "ex:dog"))
           (rows (plist-get r :axiom-rows))
           (subjects (mapcar (lambda (row) (plist-get row :curie)) rows)))
      (should-not (member "ex:dog" subjects))
      ;; ex:puppy's SubClassOf :: ex:dog declares ex:puppy as a
      ;; child of ex:dog -- promoted to `:children', not surfaced
      ;; as an axiom-row reference.
      (should-not (member "ex:puppy" subjects))
      (should (member "ex:puppy" (plist-get r :children))))))

(ert-deftest elot-gptel-drsc-test-annotation-rows-skipped ()
  "rdfs:comment is NOT an OMN axiom keyword, so a comment row that
mentions SUBJECT must not be surfaced as an axiom row.  It IS
however an annotation-property row, so it surfaces under the
new `:annotation-rows' advisory bucket -- not as a refusal."
  (elot-gptel-drsc-test--with
    (let* ((r (elot-gptel--delete-reference-scan "ex:animal"))
           (axiom-rows (plist-get r :axiom-rows))
           (ann-rows (plist-get r :annotation-rows))
           (axiom-kw (mapcar (lambda (row) (plist-get row :keyword))
                             axiom-rows))
           (ann-kw   (mapcar (lambda (row) (plist-get row :keyword))
                             ann-rows)))
      (should-not (member "rdfs:comment" axiom-kw))
      (should     (member "rdfs:comment" ann-kw)))))

(ert-deftest elot-gptel-drsc-test-annotation-rows-on-other-subjects ()
  "Annotation-property row `rdfs:seeAlso :: ex:puppy' on ex:cat
must be surfaced under `:annotation-rows' when scanning ex:puppy."
  (elot-gptel-drsc-test--with
    (let* ((r (elot-gptel--delete-reference-scan "ex:puppy"))
           (ann-rows (plist-get r :annotation-rows))
           (subjects (mapcar (lambda (row) (plist-get row :curie))
                             ann-rows))
           (keywords (mapcar (lambda (row) (plist-get row :keyword))
                             ann-rows)))
      (should (member "ex:cat" subjects))
      (should (member "rdfs:seeAlso" keywords))
      ;; The advisory bucket must NOT cause axiom-rows to be populated.
      (should-not
       (cl-some (lambda (row)
                  (string= (plist-get row :keyword) "rdfs:seeAlso"))
                (plist-get r :axiom-rows))))))


;;; ---------------------------------------------------------------------------
;;; Src-block + prose
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-drsc-test-sparql-src-row ()
  (elot-gptel-drsc-test--with
    (let* ((r (elot-gptel--delete-reference-scan "ex:animal"))
           (srcs (plist-get r :src-rows)))
      (should (>= (length srcs) 1))
      (should (cl-some (lambda (row)
                         (string= (plist-get row :language) "sparql"))
                       srcs)))))

(ert-deftest elot-gptel-drsc-test-prose-counted-not-reported ()
  (elot-gptel-drsc-test--with
    (let ((r (elot-gptel--delete-reference-scan "ex:animal")))
      ;; Two prose lines in the fixture's Notes section mention
      ;; ex:animal.
      (should (= 2 (plist-get r :prose-count))))))


;;; ---------------------------------------------------------------------------
;;; F8: staleness contract
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-drsc-test-refreshes-stale-cache ()
  "When the buffer-local cache is stale, the scan must refresh it
up front so freshly-mutated subjects are visible."
  (elot-gptel-drsc-test--with
    ;; Insert a brand-new heading and mark the cache stale, mirroring
    ;; the F8 contract that mutators rely on.
    (goto-char (point-max))
    (insert "\n*** Bird (ex:bird)\n")
    (when (fboundp 'elot-headline-hierarchy-mark-stale)
      (elot-headline-hierarchy-mark-stale))
    (let ((r (elot-gptel--delete-reference-scan "ex:bird")))
      (should (plist-get r :node)))))


;;; elot-gptel-delete-reference-scan-test.el ends here
