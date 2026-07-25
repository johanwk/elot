;;; elot-gptel-db-borrow-compose-test.el --- Tests for M9.7 composite borrow  -*- lexical-binding: t; -*-

;; Usage:  make -C test db-borrow-compose-test  (or)
;;         cd test && emacs --batch -l elot-gptel-db-borrow-compose-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.7: the composite
;; `elot_borrow_term' tool that fuses search + borrow in one
;; round-trip.  Pure-Elisp; no ROBOT required.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)
(require 'elot-gptel)

;;;; Harness ------------------------------------------------------------

(defvar elot-gptel-db-borrow-compose-test--tmpfile nil)

(defun elot-gptel-db-borrow-compose-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-borrow-compose-test--tmpfile
             (file-exists-p elot-gptel-db-borrow-compose-test--tmpfile))
    (ignore-errors
      (delete-file elot-gptel-db-borrow-compose-test--tmpfile)))
  (setq elot-gptel-db-borrow-compose-test--tmpfile
        (make-temp-file "elot-gptel-db-borrow-compose-test-"
                        nil ".sqlite"))
  (ignore-errors
    (delete-file elot-gptel-db-borrow-compose-test--tmpfile))
  (elot-db-init elot-gptel-db-borrow-compose-test--tmpfile))

(defun elot-gptel-db-borrow-compose-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-borrow-compose-test--tmpfile
             (file-exists-p elot-gptel-db-borrow-compose-test--tmpfile))
    (ignore-errors
      (delete-file elot-gptel-db-borrow-compose-test--tmpfile)))
  (setq elot-gptel-db-borrow-compose-test--tmpfile nil))

(defmacro elot-gptel-db-borrow-compose-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-gptel-db-borrow-compose-test--fresh-db) ,@body)
     (elot-gptel-db-borrow-compose-test--teardown)))

(defun elot-gptel-db-borrow-compose-test--seed-single ()
  "Seed one source with a unique-by-label class `Food'."
  (elot-db-update-source
   "pizza" nil "org"
   '(("pizza-ont" "Pizza ontology"
      ("rdf:type" "owl:Ontology"
       "dcterms:title" "Pizza Ontology"))
     ("pizza:Food" "Food"
      ("rdf:type" "owl:Class"
       "rdfs:label" ("Food" "en")
       "skos:definition" "An edible substance.")))))

(defun elot-gptel-db-borrow-compose-test--seed-multi ()
  "Seed two sources both carrying a class labelled `Food'."
  (elot-gptel-db-borrow-compose-test--seed-single)
  (elot-db-update-source
   "menu" nil "org"
   '(("menu-ont" "Menu ontology"
      ("rdf:type" "owl:Ontology"
       "dcterms:title" "Menu"))
     ("menu:Food" "Food"
      ("rdf:type" "owl:Class"
       "rdfs:label" ("Food" "en"))))))

(defun elot-gptel-db-borrow-compose-test--seed-cross-kind ()
  "Seed a class + an object property sharing label `eats'."
  (elot-db-update-source
   "cross" nil "org"
   '(("cross-ont" "Cross ontology"
      ("rdf:type" "owl:Ontology"
       "dcterms:title" "Cross"))
     ("cross:Eats" "eats"
      ("rdf:type" "owl:Class"
       "rdfs:label" ("eats" "en")))
     ("cross:eats" "eats"
      ("rdf:type" "owl:ObjectProperty"
       "rdfs:label" ("eats" "en"))))))

(defun elot-gptel-db-borrow-compose-test--seed-default-prefix ()
  "Seed a source where the matching id uses the default-prefix form."
  (elot-db-update-source
   "pizza-mini" nil "org"
   '(("pizza-mini-ont" "Pizza Mini"
      ("rdf:type" "owl:Ontology"
       "dcterms:title" "Pizza Mini"))
     (":Food" ":Food"
      ("rdf:type" "owl:Class"))))
  (elot-db-add-prefix
   "pizza-mini" nil ""
   "https://example.org/pizza-mini#"))

;;;; Zero-candidate fall-through ---------------------------------------

(ert-deftest test-elot-gptel-borrow-zero-candidates ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (elot-gptel-db-borrow-compose-test--seed-single)
    (let ((out (elot-gptel-tool-borrow-term "ZZZ-no-such-label")))
      (should (string-prefix-p "OK: no candidates" out))
      (should (string-match-p "elot_mint_identifier" out)))))

;;;; Single-candidate auto-borrow --------------------------------------

(ert-deftest test-elot-gptel-borrow-single-candidate ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (elot-gptel-db-borrow-compose-test--seed-single)
    (let ((out (elot-gptel-tool-borrow-term "Food")))
      (should (stringp out))
      ;; Provenance preface.
      (should (string-match-p "^OK: auto-borrow pizza:Food " out))
      (should (string-match-p "kind=owl:Class" out))
      (should (string-match-p "source=pizza" out))
      ;; Borrowed snippet body.
      (should (string-match-p "^\\* \"Food\"@en (pizza:Food)" out))
      (should (string-match-p "rdfs:isDefinedBy :: pizza-ont" out))
      (should (string-match-p "skos:definition :: \"An edible"
                              out)))))

;;;; Multi-candidate disambiguation prompt -----------------------------

(ert-deftest test-elot-gptel-borrow-multi-candidates ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (elot-gptel-db-borrow-compose-test--seed-multi)
    (let ((out (elot-gptel-tool-borrow-term "Food")))
      (should (stringp out))
      ;; Both candidate ids appear in the TSV.
      (should (string-match-p "pizza:Food" out))
      (should (string-match-p "menu:Food"  out))
      ;; Disambiguation block present.
      (should (string-match-p "^SELECT: 2 candidates match `Food'"
                              out))
      (should (string-match-p "source=NAME" out))
      ;; No auto-borrow happened -- no provenance preface or
      ;; ELOT heading snippet.
      (should-not (string-match-p "^OK: auto-borrow"   out))
      (should-not (string-match-p "rdfs:isDefinedBy"   out)))))

;;;; SOURCE narrows multi to single ------------------------------------

(ert-deftest test-elot-gptel-borrow-source-narrows-multi ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (elot-gptel-db-borrow-compose-test--seed-multi)
    (let ((out (elot-gptel-tool-borrow-term "Food" nil "menu")))
      (should (string-match-p "^OK: auto-borrow menu:Food " out))
      (should (string-match-p "^\\* \"Food\"@en (menu:Food)" out))
      (should-not (string-match-p "pizza:Food" out)))))

;;;; LANG narrows multi (degenerate -- both have en; assert no crash) --

(ert-deftest test-elot-gptel-borrow-lang-filter-applies ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (elot-gptel-db-borrow-compose-test--seed-multi)
    (let ((out (elot-gptel-tool-borrow-term "Food" "class" nil "en")))
      ;; Both candidates carry @en, so this stays multi.  The point
      ;; of the test is that the lang filter is wired through and
      ;; does not crash on a multi result.
      (should (or (string-match-p "^SELECT: 2 candidates" out)
                  (string-match-p "^OK: auto-borrow" out))))))

;;;; KIND narrows cross-kind ambiguity to single -----------------------

(ert-deftest test-elot-gptel-borrow-kind-narrows-cross-kind ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (elot-gptel-db-borrow-compose-test--seed-cross-kind)
    ;; Default kind=class -> auto-borrow the class.
    (let ((out (elot-gptel-tool-borrow-term "eats")))
      (should (string-match-p "^OK: auto-borrow cross:Eats " out)))
    ;; Explicit kind=object-property -> auto-borrow the property.
    (let ((out (elot-gptel-tool-borrow-term
                "eats" "object-property")))
      (should (string-match-p "^OK: auto-borrow cross:eats " out))
      (should (string-match-p "kind=owl:ObjectProperty" out)))))

;;;; Contingent NOTE inheritance (default-prefix source) ---------------

(ert-deftest test-elot-gptel-borrow-default-prefix-note-inherited ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (elot-gptel-db-borrow-compose-test--seed-default-prefix)
    (let ((out (elot-gptel-tool-borrow-term "Food")))
      (should (string-match-p "^OK: auto-borrow :Food " out))
      ;; The default-prefix NOTE from the snippet formatter flows
      ;; through unchanged.
      (should (string-match-p
               "NOTE: source uses default-prefix form `:Food'" out))
      ;; The label-missing NOTE also flows through (id == label here).
      (should (string-match-p
               "NOTE: source has no human-readable rdfs:label"
               out)))))

;;;; Empty / malformed label -------------------------------------------

(ert-deftest test-elot-gptel-borrow-empty-label ()
  (elot-gptel-db-borrow-compose-test--with-fresh-db
    (let ((out (elot-gptel-tool-borrow-term "")))
      (should (string-prefix-p "ERROR:" out)))
    (let ((out (elot-gptel-tool-borrow-term nil)))
      (should (string-prefix-p "ERROR:" out)))))

;;;; Tool-spec registration + dispatcher arity -------------------------

(ert-deftest test-elot-gptel-borrow-tool-spec-registered ()
  (let ((spec (assoc "elot_borrow_term" elot-gptel--tool-specs)))
    (should spec)
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 4 (length args)))
      (should (equal "label"  (plist-get (nth 0 args) :name)))
      (should (equal "kind"   (plist-get (nth 1 args) :name)))
      (should (equal "source" (plist-get (nth 2 args) :name)))
      (should (equal "lang"   (plist-get (nth 3 args) :name)))
      ;; Read-only -- no :confirm key.
      (should-not (plist-member (cdr spec) :confirm)))
    (should (eq 'elot-gptel-tool-borrow-term
                (plist-get (cdr spec) :function)))))

(ert-deftest test-elot-gptel-borrow-dispatcher-arity ()
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-borrow-term)))
    (should (functionp thunk))
    ;; The dispatcher accepts 1..4 args.  We only care whether an
    ;; arity error fires -- swallow any other return / error so the
    ;; assertion is about arity, not about DB contents.
    (should-not (condition-case _
                    (progn (funcall thunk "x") nil)
                  (wrong-number-of-arguments t)
                  (error nil)))
    (should-not (condition-case _
                    (progn (funcall thunk "x" "class" "src" "en") nil)
                  (wrong-number-of-arguments t)
                  (error nil)))))

;;;; Provenance-line formatter -----------------------------------------

(ert-deftest test-elot-gptel-borrow-provenance-line ()
  (let ((line (elot-gptel--borrow-provenance-line
               '("pizza:Food" "Food" "owl:Class" "pizza-ont"
                 "pizza" nil "label-substring"))))
    (should (equal
             "OK: auto-borrow pizza:Food (kind=owl:Class, \
source=pizza, via=label-substring)"
             line))))

(provide 'elot-gptel-db-borrow-compose-test)
;;; elot-gptel-db-borrow-compose-test.el ends here
