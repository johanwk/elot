;;; elot-gptel-read-resource-test.el --- Tests for elot_read_resource  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-read-resource-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-V1-PLAN.org post-v1 M12 -- tests for the read-only
;; `elot_read_resource' inspection tool.  Pure Elisp; no ROBOT, no
;; DB.  Exercises subject resolution (CURIE + label), the outline
;; context (parent CURIE + child list), and the description-list
;; render with meta-annotation nesting preserved (the thing the
;; flattened slurp cannot show).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-read-resource-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-read-resource-test--repo-root repo-root))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Fixture: `ex:chases' carries nested meta-annotations on its rows.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-read-resource-test--fixture ()
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
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
   "| skos:  | http://www.w3.org/2004/02/skos/core#  |\n"
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   " - rdfs:comment :: A living organism\n"
   "**** Dog (ex:dog)\n"
   "**** Cat (ex:cat)\n"
   "**** <http://example.org/wild> (<http://example.org/wild>)\n"
   "*** Species (ex:species)\n"
   " - rdfs:comment :: A biological species (also a named individual)\n"
   "*** \"role\"@en (<http://example.org/role>)\n"
   " - rdfs:comment :: A realizable entity, lang-tagged label\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:animal\n"
   "   - rdfs:comment :: A domain remark\n"
   " - Range :: ex:animal\n"
   "*** chatId (ex:chatId)\n"
   " - Domain :: ex:animal\n"
   " - Characteristics :: InverseFunctional\n"
   "** Data properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-data-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chatId (ex:chatId)\n"
   " - Range :: rdfs:Literal\n"
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Species (ex:species)\n"
   " - Types :: owl:Thing\n"))

(defun elot-gptel-read-resource-test--write ()
  (let ((path (expand-file-name
               (format "elot-read-resource-fixture-%d.org" (random 1000000))
               elot-gptel-read-resource-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-read-resource-test--fixture)))
    path))

(defmacro elot-gptel-read-resource-test--with-fixture (path-var &rest body)
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-read-resource-test--write)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-read-resource-test--rel (path)
  (file-relative-name path elot-gptel-read-resource-test--repo-root))

(defun elot-gptel-read-resource-test--run (path subject &optional limit)
  (let ((default-directory elot-gptel-read-resource-test--repo-root))
    (elot-gptel-tool-read-resource
     (elot-gptel-read-resource-test--rel path) subject limit)))

;;; ---------------------------------------------------------------------------
;;; Resolution
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-read-resource-test-by-curie ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:dog")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "ex:dog" out))
      (should (string-match-p "kind Class" out)))))

(ert-deftest elot-gptel-read-resource-test-by-label ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "chases")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "ex:chases" out))
      (should (string-match-p "kind ObjectProperty" out)))))

(ert-deftest elot-gptel-read-resource-test-by-bare-iri ()
  "A bare full IRI resolves a resource whose `:uri' is stored in
angle-bracketed form (`<...>')."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run
                path "http://example.org/role")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "realizable entity, lang-tagged" out)))))

(ert-deftest elot-gptel-read-resource-test-by-lang-tagged-label ()
  "A plain label resolves a resource whose stored `:label' carries a
surrounding pair of quotes and an `@lang' tag (e.g. `\"role\"@en')."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "role")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "realizable entity, lang-tagged" out)))))

(ert-deftest elot-gptel-read-resource-test-unknown ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:nope")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "not found" out)))))

(ert-deftest elot-gptel-read-resource-test-empty-subject ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "non-empty" out)))))

;;; ---------------------------------------------------------------------------
;;; Outline context
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-read-resource-test-parent ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:dog")))
      (should (string-match-p "Parent: ex:animal" out)))))

(ert-deftest elot-gptel-read-resource-test-children ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:animal")))
      (should (string-match-p "Children (3)" out))
      (should (string-match-p "ex:dog" out))
      (should (string-match-p "ex:cat" out)))))

(ert-deftest elot-gptel-read-resource-test-child-missing-label-dash ()
  "A full-IRI child with no distinct rdfs:label renders as \"--\",
matching the LABEL-column fallback used by `elot_resources', rather
than echoing the IRI as its own label."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:animal")))
      (should (string-match-p "<http://example.org/wild> (--)" out)))))

(ert-deftest elot-gptel-read-resource-test-children-cap ()
  "LIMIT caps the child list and appends a `... N more' trailer."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:animal" 2)))
      ;; Header still reports the true total.
      (should (string-match-p "Children (3)" out))
      ;; Only two children shown, with a trailer for the remaining one.
      (should (string-match-p "\\.\\.\\. 1 more child" out)))))

(ert-deftest elot-gptel-read-resource-test-children-cap-via-thunk ()
  "LIMIT reaches the cap even when marshalled as a float/string.
Drives the tool through `elot-gptel--tool-thunk' -- the gptel
dispatch seam where `elot-gptel--as-limit' coerces a JSON number
\(often a float like 10.0, or a numeric string) to an integer.
Calling the tool function directly (as `--run' does) would bypass
that coercion, so this pins the wiring, not just the cap logic."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((default-directory elot-gptel-read-resource-test--repo-root)
          (rel (elot-gptel-read-resource-test--rel path))
          (thunk (elot-gptel--tool-thunk 'elot-gptel-tool-read-resource)))
      (dolist (lim '(2.0 "2"))
        ;; gptel-style call: (file subject &optional limit), with LIMIT
        ;; marshalled as a JSON number (float) or numeric string.
        (let ((out (funcall thunk rel "ex:animal" lim)))
          (should (string-match-p "Children (3)" out))
          (should (string-match-p "\\.\\.\\. 1 more child" out)))))))

(ert-deftest elot-gptel-read-resource-test-top-level-no-parent ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:animal")))
      (should (string-match-p "Parent: (none" out)))))

;;; ---------------------------------------------------------------------------
;;; Description-list render (meta-annotation nesting preserved)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-read-resource-test-rows ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:chases")))
      (should (string-match-p "== Description-list rows ==" out))
      (should (string-match-p "Domain :: ex:animal" out))
      (should (string-match-p "Range :: ex:animal" out))
      ;; The nested meta-annotation is preserved (indented deeper).
      (should (string-match-p "rdfs:comment :: A domain remark" out))
      ;; Housekeeping rows filtered out.
      (should-not (string-match-p "rdfs:label ::" out))
      (should-not (string-match-p "rdf:type ::" out)))))

(ert-deftest elot-gptel-read-resource-test-empty-rows ()
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:dog")))
      (should (string-match-p "no description-list rows" out)))))

;;; ---------------------------------------------------------------------------
;;; Multiple declarations (punning)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-read-resource-test-forbidden-pun-property-kinds ()
  "A name used as both an object and a data property surfaces every
declaration, flags PUNNED in the header, and warns that OWL 2 DL
forbids using a name for more than one kind of property."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:chatId")))
      (should (string-prefix-p "OK:" out))
      ;; Header announces the multiplicity up front.
      (should (string-match-p "declared 2x (PUNNED)" out))
      (should (string-match-p "ObjectProperty" out))
      (should (string-match-p "DataProperty" out))
      ;; DL warning fires for the forbidden property-kind pun.
      (should (string-match-p "WARNING: not permitted in OWL 2 DL" out))
      (should (string-match-p "one kind of property" out))
      ;; Both faces are rendered in their own blocks.
      (should (string-match-p "declaration 1/2" out))
      (should (string-match-p "declaration 2/2" out))
      (should (string-match-p "Characteristics :: InverseFunctional" out))
      (should (string-match-p "Range :: rdfs:Literal" out)))))

(ert-deftest elot-gptel-read-resource-test-permitted-pun-class-individual ()
  "A class doubling as a named individual is a permitted OWL 2 DL pun:
both declarations are surfaced with a PUNNED header but NO warning."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:species")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "declared 2x (PUNNED)" out))
      (should (string-match-p "Class" out))
      (should (string-match-p "Individual" out))
      ;; Permitted pun: no DL warning.
      (should-not (string-match-p "WARNING: not permitted in OWL 2 DL" out)))))

(ert-deftest elot-gptel-read-resource-test-single-no-punned-header ()
  "A singly-declared resource keeps the plain single-block output --
no PUNNED header, no declaration framing."
  (elot-gptel-read-resource-test--with-fixture path
    (let ((out (elot-gptel-read-resource-test--run path "ex:chases")))
      (should-not (string-match-p "PUNNED" out))
      (should-not (string-match-p "declaration 1/" out)))))

;;; ---------------------------------------------------------------------------
;;; Registration
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-read-resource-test-tool-spec-registered ()
  (let ((spec (assoc "elot_read_resource" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-read-resource))
    (should (null (plist-get (cdr spec) :confirm)))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 3 (length args)))
      (should (equal (plist-get (car args) :name) "file"))
      (should (equal (plist-get (cadr args) :name) "subject"))
      (should (equal (plist-get (caddr args) :name) "limit")))))

(provide 'elot-gptel-read-resource-test)
;;; elot-gptel-read-resource-test.el ends here
