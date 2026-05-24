;;; elot-lint-oops-test.el --- Tests for OOPS!-derived org-lint checkers  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-lint-oops-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 8 (OOPS!-for-lint sliver):
;; Tests for the OOPS!-derived org-lint checkers shipped inline
;; with `elot-org-lint':
;;
;;   - elot/oops-missing-annotations   (OOPS! P08)
;;   - elot/oops-recursive-definition  (OOPS! P24)
;;   - elot/oops-duplicate-labels      (OOPS! P32)
;;
;; Drives `elot-gptel-tool-lint' against
;; test/fixtures/oops-lint-sliver.org and asserts both the
;; positive firings and the trickiest negatives (substring-but-not-
;; word-boundary match for P24, same label text but different
;; language tags for P32, several alternative annotation keys for
;; P08).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(defvar elot-lint-oops-test--repo-root nil)
(defvar elot-lint-oops-test--fixtures nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-lint-oops-test--repo-root repo-root
        elot-lint-oops-test--fixtures
        (expand-file-name "test/fixtures" repo-root)))

(require 'elot-gptel)

(defun elot-lint-oops-test--lint ()
  "Run elot_lint on the fixture; return the report string."
  (let* ((file (expand-file-name "oops-lint-sliver.org"
                                 elot-lint-oops-test--fixtures))
         (default-directory elot-lint-oops-test--repo-root))
    (elot-gptel-tool-lint file "all" nil)))

(defun elot-lint-oops-test--lines-matching (out checker)
  "Return only the lines of OUT mentioning CHECKER (a symbol or string)."
  (let ((name (if (symbolp checker) (symbol-name checker) checker))
        (lines (split-string out "\n" t)))
    (seq-filter (lambda (line) (string-match-p (regexp-quote name) line))
                lines)))

;; -- Registration ----------------------------------------------------------

(ert-deftest elot-lint-oops-test-checkers-registered ()
  "All three OOPS!-derived checkers are registered with org-lint."
  (require 'elot-lint)
  (require 'org-lint)
  (dolist (sym '(elot/oops-missing-annotations
                 elot/oops-recursive-definition
                 elot/oops-duplicate-labels))
    (should (seq-find (lambda (c) (eq (org-lint-checker-name c) sym))
                      org-lint--checkers))))

;; -- P08 Missing annotations ----------------------------------------------

(ert-deftest elot-lint-oops-test-p08-fires-on-undocumented ()
  "P08 warns about ex:undocumented (no label / comment / definition)."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-missing-annotations)))
    (should (seq-some (lambda (l) (string-match-p "ex:undocumented" l))
                      lines))))

(ert-deftest elot-lint-oops-test-p08-cites-oops ()
  "P08 messages embed the OOPS! P08 citation."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-missing-annotations)))
    (should (seq-some (lambda (l)
                        (and (string-match-p "OOPS! P08" l)
                             (string-match-p "oops.linkeddata.es" l)))
                      lines))))

(ert-deftest elot-lint-oops-test-p08-silent-on-documented ()
  "P08 does not fire on resources with rdfs:label / rdfs:comment /
skos:definition / dcterms:description."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-missing-annotations)))
    (dolist (curie '("ex:labelled" "ex:described" "ex:defined"
                     "ex:annotatedOp"))
      (dolist (line lines)
        (should-not (string-match-p (regexp-quote curie) line))))))

;; -- P24 Recursive definition ---------------------------------------------

(ert-deftest elot-lint-oops-test-p24-fires-on-subclass-self ()
  "P24 warns when SubClassOf names the subject's own CURIE."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-recursive-definition)))
    (should (seq-some (lambda (l)
                        (and (string-match-p "ex:recursive" l)
                             (string-match-p "SubClassOf" l)))
                      lines))))

(ert-deftest elot-lint-oops-test-p24-fires-on-equivalentto-self ()
  "P24 warns when EquivalentTo names the subject's own CURIE."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-recursive-definition)))
    (should (seq-some (lambda (l)
                        (and (string-match-p "ex:looper" l)
                             (string-match-p "EquivalentTo" l)))
                      lines))))

(ert-deftest elot-lint-oops-test-p24-respects-word-boundaries ()
  "P24 must NOT fire when the subject CURIE only appears as a
substring of a longer CURIE (ex:dogfood inside ex:dogfoodThing)."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-recursive-definition)))
    ;; The diagnostic message for a real hit names the subject CURIE
    ;; only when the hit is genuine; ex:dogfood must not appear in
    ;; any P24 line.
    (dolist (line lines)
      (should-not (and (string-match-p "ex:dogfood\\b" line)
                       (not (string-match-p "ex:dogfoodThing" line)))))
    ;; Stronger: no P24 line should mention ex:dogfood at all, since
    ;; the only candidate row is its (legitimate) SubClassOf
    ;; ex:dogfoodThing.
    (should-not (seq-some (lambda (l)
                            (string-match-p
                             "\\bex:dogfood\\b[^T]" l))
                          lines))))

(ert-deftest elot-lint-oops-test-p24-cites-oops ()
  "P24 messages embed the OOPS! P24 citation."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-recursive-definition)))
    (should (seq-some (lambda (l)
                        (and (string-match-p "OOPS! P24" l)
                             (string-match-p "oops.linkeddata.es" l)))
                      lines))))

;; -- P32 Duplicate labels --------------------------------------------------

(ert-deftest elot-lint-oops-test-p32-fires-on-same-lang ()
  "P32 warns when two resources share the same label text + language."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-duplicate-labels)))
    (should (seq-some (lambda (l)
                        (and (string-match-p "Twin" l)
                             (string-match-p "ex:twin1" l)
                             (string-match-p "ex:twin2" l)))
                      lines))))

(ert-deftest elot-lint-oops-test-p32-silent-on-different-lang ()
  "P32 must NOT fire when label texts coincide but language tags differ
(\"House\"@en vs \"House\"@de)."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-duplicate-labels)))
    (dolist (line lines)
      (should-not (string-match-p "ex:houseEn\\|ex:houseDe" line)))))

(ert-deftest elot-lint-oops-test-p32-cites-oops ()
  "P32 messages embed the OOPS! P32 citation."
  (let* ((out (elot-lint-oops-test--lint))
         (lines (elot-lint-oops-test--lines-matching
                 out 'elot/oops-duplicate-labels)))
    (should (seq-some (lambda (l)
                        (and (string-match-p "OOPS! P32" l)
                             (string-match-p "oops.linkeddata.es" l)))
                      lines))))

(provide 'elot-lint-oops-test)
;;; elot-lint-oops-test.el ends here
