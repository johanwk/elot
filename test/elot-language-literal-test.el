;;; elot-language-literal-test.el --- Tests for org-ingest literal splitting -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `elot-split-language-literal' and the slurp
;; transformer applied on the ELOT label DB boundary
;; (`elot-slurp-for-db').  Pure Elisp: no DB, no ROBOT, no file I/O.
;;
;; Motivation: an ELOT .org source spells a tagged literal as the
;; single string "denoter"@en-us, while the Turtle ingest path yields
;; the pair ("denoter" . "en-us").  The DB rows must agree -- the
;; information is the same.

;;; Code:

(require 'ert)
(require 'elot-tangle)

;;;; elot-split-language-literal

(ert-deftest test-elot-split-language-literal-tagged ()
  "A complete language-tagged literal splits into (LEX LANG)."
  (should (equal (elot-split-language-literal "\"denoter\"@en-us")
                 '("denoter" "en-us")))
  (should (equal (elot-split-language-literal "\"bil\"@nb")
                 '("bil" "nb"))))

(ert-deftest test-elot-split-language-literal-untagged ()
  "Plain and quoted-only strings are not split."
  (should-not (elot-split-language-literal "denoter"))
  (should-not (elot-split-language-literal "\"denoter\""))
  (should-not (elot-split-language-literal "")))

(ert-deftest test-elot-split-language-literal-datatype-passthrough ()
  "Datatype literals are deliberately left alone -- no column for them."
  (should-not (elot-split-language-literal "\"3.5\"^^xsd:decimal"))
  (should-not (elot-split-language-literal "\"true\"^^xsd:boolean")))

(ert-deftest test-elot-split-language-literal-embedded-quote ()
  "An escaped quote inside the lexical form does not terminate the match."
  (should (equal (elot-split-language-literal "\"a \\\"b\\\" c\"@en")
                 '("a \\\"b\\\" c" "en"))))

(ert-deftest test-elot-split-language-literal-non-string ()
  "Non-string values return nil rather than signalling."
  (should-not (elot-split-language-literal nil))
  (should-not (elot-split-language-literal 42))
  (should-not (elot-split-language-literal '("x"))))

(ert-deftest test-elot-split-language-literal-anchored ()
  "The regexp is anchored: trailing or leading junk defeats the match."
  (should-not (elot-split-language-literal "prefix \"denoter\"@en-us"))
  (should-not (elot-split-language-literal "\"denoter\"@en-us trailing")))

;;;; elot-slurp-for-db

(ert-deftest test-elot-slurp-for-db-splits-label-and-values ()
  "Row label and tagged plist values are split; others pass through."
  (let* ((slurp '(("iof-constr:Denoter"
                   "\"denoter\"@en-us"
                   ("rdfs:label" "\"denoter\"@en-us"
                    "iof-av:naturalLanguageDefinition"
                    "\"information content entity that denotes some entity\"@en-us"
                    "SubClassOf" "iof-constr:InformationContentEntity"))))
         (out (elot-slurp-for-db slurp))
         (row (car out)))
    (should (equal (nth 0 row) "iof-constr:Denoter"))
    ;; display label reduced to bare lexical form
    (should (equal (nth 1 row) "denoter"))
    (let ((plist (nth 2 row)))
      (should (equal (plist-get plist "rdfs:label" #'equal)
                     '("denoter" "en-us")))
      (should (equal (plist-get plist "iof-av:naturalLanguageDefinition" #'equal)
                     '("information content entity that denotes some entity"
                       "en-us")))
      ;; structural row untouched
      (should (equal (plist-get plist "SubClassOf" #'equal)
                     "iof-constr:InformationContentEntity")))))

(ert-deftest test-elot-slurp-for-db-untagged-unchanged ()
  "A slurp with no tagged literals is returned structurally identical."
  (let ((slurp '(("cars:car" "Car" ("rdfs:comment" "A road vehicle.")))))
    (should (equal (elot-slurp-for-db slurp) slurp))))

(ert-deftest test-elot-slurp-for-db-preserves-plist-order ()
  "Key order in the plist is preserved by the transformer."
  (let* ((slurp '(("ex:a" "A" ("k1" "\"v1\"@en" "k2" "v2" "k3" "\"v3\"@nb"))))
         (plist (nth 2 (car (elot-slurp-for-db slurp)))))
    (should (equal (cl-remove-if-not #'stringp
                                     (cl-loop for x on plist by #'cddr
                                              collect (car x)))
                   '("k1" "k2" "k3")))))

(ert-deftest test-elot-slurp-for-db-empty ()
  "An empty slurp yields an empty result."
  (should (equal (elot-slurp-for-db nil) nil)))

;;;; elot-source-parse-org (register / refresh boundary)

(ert-deftest test-elot-source-parse-org-splits-language-literals ()
  "The DB register/refresh path splits tagged literals like the TTL path.
Regression: `elot-label-refresh-source' goes through
`elot-source-parse-org', NOT `elot-slurp-to-vars', so the
transformer must be applied there too."
  (require 'elot-sources)
  (let ((file (make-temp-file "elot-lang-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* test-ont\n"
                    ":PROPERTIES:\n"
                    ":ID: test-ont\n"
                    ":ELOT-context-type: ontology\n"
                    ":ELOT-context-localname: test-ont\n"
                    ":ELOT-default-prefix: ex\n"
                    ":END:\n"
                    "** Prefixes\n"
                    ":PROPERTIES:\n"
                    ":prefixdefs: yes\n"
                    ":END:\n"
                    "#+name: prefix-table\n"
                    "| prefix | uri                          |\n"
                    "|--------+------------------------------|\n"
                    "| ex:    | http://example.org/resource/ |\n"
                    "** Classes\n"
                    ":PROPERTIES:\n"
                    ":ID: test-ont-class-hierarchy\n"
                    ":resourcedefs: yes\n"
                    ":END:\n"
                    "*** \"denoter\"@en-us (ex:denoter)\n"
                    " - rdfs:comment :: \"a comment\"@en\n"))
          (let* ((slurp (car (elot-source--entries-and-prefixes
                              (elot-source-parse-org file))))
                 (row (assoc "ex:denoter" slurp)))
            (should row)
            ;; display label reduced to the bare lexical form
            (should (equal (nth 1 row) "denoter"))
            (let ((plist (nth 2 row)))
              (should (equal (plist-get plist "rdfs:label" #'equal)
                             '("denoter" "en-us")))
              (should (equal (plist-get plist "rdfs:comment" #'equal)
                             '("a comment" "en"))))))
      (delete-file file))))

(provide 'elot-language-literal-test)
;;; elot-language-literal-test.el ends here
