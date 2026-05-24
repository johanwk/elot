;;; elot-gptel-insert-test.el --- Tests for the elot_insert_* gptel tools  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-insert-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3 -- tests for the
;; LLM-facing `elot_insert_sibling_resource',
;; `elot_insert_child_resource', and `elot_insert_resource_tree'
;; tool wrappers.  Pure-Elisp; lint and OMN-validate stages are
;; stubbed via `cl-letf' so no ROBOT is required.  Side-effects
;; flag is bound around each test so the user's global setting
;; is not perturbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-insert-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-insert-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-id)
(require 'elot-id-insert)

;;; ---------------------------------------------------------------------------
;;; Fixture: minimal ontology with :ELOT-id-scheme:, a class hierarchy,
;;; an object property, a datatype, and an individual.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-insert-test--fixture (&optional scheme)
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-id-scheme: " (or scheme "slug") "\n"
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
   "**** Dog (ex:dog)\n"
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
   "*** spooky (ex:spooky)\n"))

(defun elot-gptel-insert-test--write-fixture (filename &optional scheme)
  (let ((path (expand-file-name filename
                                elot-gptel-insert-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-insert-test--fixture scheme)))
    path))

(defmacro elot-gptel-insert-test--with-fixture (path-var &rest body)
  "Bind PATH-VAR to a fresh fixture file and run BODY with stubs."
  (declare (indent 1))
  `(let* ((,path-var (elot-gptel-insert-test--write-fixture
                      (format "elot-insert-fixture-%d.org"
                              (random 1000000)))))
     (unwind-protect
         (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                    (lambda (&rest _) "OK: no lint issues"))
                   ((symbol-function 'elot-gptel-tool-omn-validate)
                    (lambda (&rest _) "OK: 1 ontology parsed"))
                   ((symbol-function 'elot-robot-available-p)
                    (lambda (&rest _) nil)))
           ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-insert-test--rel (path)
  (file-relative-name path elot-gptel-insert-test--repo-root))

(defun elot-gptel-insert-test--read (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))


;;; ---------------------------------------------------------------------------
;;; Side-effect gate
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-sibling-side-effects-disabled ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" '("Beagle"))))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "side effects disabled" out))))))

(ert-deftest elot-gptel-insert-test-child-side-effects-disabled ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" '("Mammal"))))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "side effects disabled" out))))))

(ert-deftest elot-gptel-insert-test-tree-side-effects-disabled ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-resource-tree
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" '("Bird") "sibling")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "side effects disabled" out))))))


;;; ---------------------------------------------------------------------------
;;; Happy path: sibling insert
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-sibling-success ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" '("Beagle" "Poodle"))))
        (should (stringp out))
        (should (string-prefix-p "OK: inserted 2 siblings under ex:dog" out))
        (should (string-match-p "Minted CURIEs:" out))
        (should (string-match-p "ex:beagle" out))
        (should (string-match-p "ex:poodle" out))
        (should (string-match-p "== LINT ==" out)))
      ;; On disk both new headings appear at level 4 (sibling of Dog).
      (let ((bytes (elot-gptel-insert-test--read path)))
        (should (string-match-p "^\\*\\*\\*\\* Beagle (ex:beagle)" bytes))
        (should (string-match-p "^\\*\\*\\*\\* Poodle (ex:poodle)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Happy path: child insert
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-child-success ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" '("Mammal"))))
        (should (stringp out))
        (should (string-prefix-p "OK: inserted 1 child under ex:animal" out))
        (should (string-match-p "ex:mammal" out)))
      (let ((bytes (elot-gptel-insert-test--read path)))
        ;; Mammal lands as a level-4 child of Animal (level 3).
        (should (string-match-p "^\\*\\*\\*\\* Mammal (ex:mammal)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Refusal: child under a Datatypes resource (level 3+)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-child-refused-under-datatype ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:myType" '("sub"))))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))))))

(ert-deftest elot-gptel-insert-test-child-refused-under-individual ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:spooky" '("sub"))))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))))))


;;; ---------------------------------------------------------------------------
;;; Tree insert: level-ordered CURIE list and structure
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-tree-success-level-order ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-resource-tree
                  (elot-gptel-insert-test--rel path)
                  "ex:animal"
                  '(("Mammal" "Mouse" "Whale") "Bird")
                  "child")))
        (should (stringp out))
        (should (string-prefix-p
                 "OK: inserted 4 headings under ex:animal (as child)"
                 out))
        ;; `elot-insert-labels-tree' batch-mints each level before
        ;; descending, so the CURIE list is level-ordered, not strict
        ;; pre-order DFS: top-level siblings (Mammal, Bird) first, then
        ;; Mammal's children (Mouse, Whale).
        (let ((pos-mammal (string-match "ex:mammal" out))
              (pos-bird   (string-match "ex:bird"   out))
              (pos-mouse  (string-match "ex:mouse"  out))
              (pos-whale  (string-match "ex:whale"  out)))
          (should (and pos-mammal pos-bird pos-mouse pos-whale))
          (should (< pos-mammal pos-bird))
          (should (< pos-bird pos-mouse))
          (should (< pos-mouse pos-whale))))
      (let ((bytes (elot-gptel-insert-test--read path)))
        ;; Top-level nodes at level 4 (child of Animal at level 3).
        (should (string-match-p "^\\*\\*\\*\\* Mammal (ex:mammal)" bytes))
        (should (string-match-p "^\\*\\*\\*\\* Bird (ex:bird)" bytes))
        ;; Nested species at level 5 under Mammal.
        (should (string-match-p "^\\*\\*\\*\\*\\* Mouse (ex:mouse)" bytes))
        (should (string-match-p "^\\*\\*\\*\\*\\* Whale (ex:whale)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Argument validation
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-empty-labels-rejected ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" '())))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "non-empty" out))))))

(ert-deftest elot-gptel-insert-test-non-string-label-rejected ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" '("OK" 42))))
        (should (string-prefix-p "ERROR:" out))))))

(ert-deftest elot-gptel-insert-test-blank-label-rejected ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" '("OK" "   "))))
        (should (string-prefix-p "ERROR:" out))))))

;;; ---------------------------------------------------------------------------
;;; F1 regression: JSON-array (= Elisp vector) labels must be accepted by
;;; the sibling / child wrappers.  gptel decodes JSON arrays as vectors
;;; in some configurations; the old `(listp labels)' guard refused every
;;; such call.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-sibling-accepts-vector-labels ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" ["Beagle" "Poodle"])))
        (should (stringp out))
        (should (string-prefix-p "OK: inserted 2 siblings under ex:dog" out))
        (should (string-match-p "ex:beagle" out))
        (should (string-match-p "ex:poodle" out)))
      (let ((bytes (elot-gptel-insert-test--read path)))
        (should (string-match-p "^\\*\\*\\*\\* Beagle (ex:beagle)" bytes))
        (should (string-match-p "^\\*\\*\\*\\* Poodle (ex:poodle)" bytes))))))

(ert-deftest elot-gptel-insert-test-child-accepts-vector-labels ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" ["Mammal"])))
        (should (stringp out))
        (should (string-prefix-p "OK: inserted 1 child under ex:animal" out))
        (should (string-match-p "ex:mammal" out))))))

(ert-deftest elot-gptel-insert-test-empty-vector-labels-rejected ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" [])))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "non-empty" out))))))

(ert-deftest elot-gptel-insert-test-bad-as-rejected ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-resource-tree
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" '("Bird") "nephew")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "child\\|sibling" out))))))

(ert-deftest elot-gptel-insert-test-malformed-tree-node-rejected ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-resource-tree
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" '(42) "sibling")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "malformed" out))))))

(ert-deftest elot-gptel-insert-test-empty-tree-rejected ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-resource-tree
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" '() "sibling")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "non-empty" out))))))


;;; ---------------------------------------------------------------------------
;;; Anchor resolution: bad CURIE / ambiguous / missing
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-anchor-missing ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:nope" '("X"))))
        (should (string-prefix-p "ERROR:" out))))))

(ert-deftest elot-gptel-insert-test-anchor-by-label ()
  "A label-shaped anchor resolves when it is unambiguous."
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "Dog" '("Beagle"))))
        (should (string-prefix-p "OK: inserted 1 sibling under Dog" out))))))


;;; ---------------------------------------------------------------------------
;;; F6 regression -- anchor resolution via :ID:, :CUSTOM_ID: and plain title.
;;; The pre-F6 resolver could only match headings of the shape
;;; `STARS LABEL (curie)' -- it could not address ELOT section-root
;;; headings (which carry no CURIE parenthetical), so the wrappers
;;; could not seed the first resource of an empty resource section.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-anchor-by-section-id ()
  "Resolve anchor by the section heading's :ID: property value."
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "my-ont-class-hierarchy" '("Reptile"))))
        (should (string-prefix-p
                 "OK: inserted 1 child under my-ont-class-hierarchy"
                 out))
        (should (string-match-p "ex:reptile" out)))
      (let ((bytes (elot-gptel-insert-test--read path)))
        ;; New heading lands at level 3 -- first child of the level-2
        ;; Classes section root.
        (should (string-match-p "^\\*\\*\\* Reptile (ex:reptile)" bytes))))))

(ert-deftest elot-gptel-insert-test-anchor-by-plain-title ()
  "Resolve anchor by plain heading title (no CURIE parenthetical)."
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "Classes" '("Reptile"))))
        (should (string-prefix-p
                 "OK: inserted 1 child under Classes"
                 out))
        (should (string-match-p "ex:reptile" out)))
      (let ((bytes (elot-gptel-insert-test--read path)))
        (should (string-match-p "^\\*\\*\\* Reptile (ex:reptile)" bytes))))))

(ert-deftest elot-gptel-insert-test-anchor-plain-title-ambiguous ()
  "Two headings with the same plain title trigger an ERROR."
  (let* ((path (expand-file-name
                (format "elot-insert-dup-%d.org" (random 1000000))
                elot-gptel-insert-test--repo-root)))
    (with-temp-file path
      (insert "* my-ont\n"
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
              "*** Foo\n"
              "*** Foo\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                   (lambda (&rest _) "OK: no lint issues"))
                  ((symbol-function 'elot-gptel-tool-omn-validate)
                   (lambda (&rest _) "OK: 1 ontology parsed"))
                  ((symbol-function 'elot-robot-available-p)
                   (lambda (&rest _) nil)))
          (let ((elot-gptel-allow-side-effects t)
                (default-directory elot-gptel-insert-test--repo-root))
            (let ((out (elot-gptel-tool-insert-sibling-resource
                        (elot-gptel-insert-test--rel path)
                        "Foo" '("Bar"))))
              (should (string-prefix-p "ERROR:" out))
              (should (string-match-p "ambiguous" out)))))
      (when (file-exists-p path) (delete-file path))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b)
                   (string= (buffer-file-name b) path))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))))))


;;; ---------------------------------------------------------------------------
;;; Missing :ELOT-id-scheme: -> error hint
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-missing-scheme ()
  (let* ((path (expand-file-name
                (format "elot-insert-noscheme-%d.org" (random 1000000))
                elot-gptel-insert-test--repo-root)))
    (with-temp-file path
      (insert "* my-ont\n"
              ":PROPERTIES:\n"
              ":ID: my-ont\n"
              ":ELOT-context-type: ontology\n"
              ":ELOT-context-localname: my-ont\n"
              ":ELOT-default-prefix: ex\n"
              ":END:\n"
              "** Classes\n"
              ":PROPERTIES:\n:resourcedefs: yes\n:END:\n"
              "*** Animal (ex:animal)\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                   (lambda (&rest _) "OK: no lint issues"))
                  ((symbol-function 'elot-gptel-tool-omn-validate)
                   (lambda (&rest _) "OK"))
                  ((symbol-function 'elot-robot-available-p)
                   (lambda (&rest _) nil))
                  ;; Block the interactive picker so the secondary guard fires.
                  ((symbol-function 'elot-id-set-scheme)
                   (lambda () (interactive) nil)))
          (let ((elot-gptel-allow-side-effects t)
                (default-directory elot-gptel-insert-test--repo-root))
            (let ((out (elot-gptel-tool-insert-sibling-resource
                        (file-relative-name
                         path elot-gptel-insert-test--repo-root)
                        "ex:animal" '("Plant"))))
              (should (string-prefix-p "ERROR:" out)))))
      (when (file-exists-p path) (delete-file path))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b)
                   (string= (buffer-file-name b) path))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))))))


;;; ---------------------------------------------------------------------------
;;; Revalidation failure -> rollback
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-revalidation-rolls-back ()
  (elot-gptel-insert-test--with-fixture path
    (let* ((elot-gptel-allow-side-effects t)
           (default-directory elot-gptel-insert-test--repo-root)
           (before (elot-gptel-insert-test--read path)))
      (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                 (lambda (&rest _)
                   "1:1  [elot/fake/low]  synthetic error\nSummary: 1 error, 0 warnings")))
        (let ((out (elot-gptel-tool-insert-sibling-resource
                    (elot-gptel-insert-test--rel path)
                    "ex:dog" '("Beagle"))))
          (should (string-prefix-p
                   "ERROR: revalidation failed -- changes rolled back"
                   out))
          (should (string-match-p "synthetic error" out))))
      (let ((after (elot-gptel-insert-test--read path)))
        (should (string= before after))))))


;;; ---------------------------------------------------------------------------
;;; F13 regression: minted CURIEs are paired with their input labels
;;; in the response, so follow-up renames are unambiguous.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-sibling-pairs-label ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-sibling-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:dog" '("Beagle" "Poodle"))))
        (should (stringp out))
        ;; Each minted CURIE line carries `-- "Label"' after the CURIE.
        (should (string-match-p "- ex:beagle -- \"Beagle\"" out))
        (should (string-match-p "- ex:poodle -- \"Poodle\"" out))))))

(ert-deftest elot-gptel-insert-test-child-pairs-label ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-child-resource
                  (elot-gptel-insert-test--rel path)
                  "ex:animal" '("Mammal"))))
        (should (string-match-p "- ex:mammal -- \"Mammal\"" out))))))

(ert-deftest elot-gptel-insert-test-tree-pairs-labels ()
  (elot-gptel-insert-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-insert-test--repo-root))
      (let ((out (elot-gptel-tool-insert-resource-tree
                  (elot-gptel-insert-test--rel path)
                  "ex:animal"
                  '(("Mammal" "Mouse" "Whale") "Bird")
                  "child")))
        (should (string-match-p "- ex:mammal -- \"Mammal\"" out))
        (should (string-match-p "- ex:bird -- \"Bird\""     out))
        (should (string-match-p "- ex:mouse -- \"Mouse\""   out))
        (should (string-match-p "- ex:whale -- \"Whale\""   out))
        ;; Pairs are still in level order (top-level first).
        (let ((pos-mammal (string-match "- ex:mammal -- \"Mammal\"" out))
              (pos-bird   (string-match "- ex:bird -- \"Bird\""     out))
              (pos-mouse  (string-match "- ex:mouse -- \"Mouse\""   out))
              (pos-whale  (string-match "- ex:whale -- \"Whale\""   out)))
          (should (and pos-mammal pos-bird pos-mouse pos-whale))
          (should (< pos-mammal pos-bird))
          (should (< pos-bird pos-mouse))
          (should (< pos-mouse pos-whale)))))))

(ert-deftest elot-gptel-insert-test-format-curies-pair-form ()
  ;; Direct unit test on the formatter.
  (should
   (equal
    (elot-gptel--insert-format-curies '(("ex:foo" . "Foo") ("ex:bar" . "Bar")))
    "Minted CURIEs:\n  - ex:foo -- \"Foo\"\n  - ex:bar -- \"Bar\"")))

(ert-deftest elot-gptel-insert-test-format-curies-bare-curies ()
  ;; Backward-compatibility: bare-string entries still render.
  (should
   (equal
    (elot-gptel--insert-format-curies '("ex:foo" "ex:bar"))
    "Minted CURIEs:\n  - ex:foo\n  - ex:bar")))

(ert-deftest elot-gptel-insert-test-format-curies-nil-label ()
  ;; A nil / empty label cdr falls back to the bare CURIE form.
  (should
   (equal
    (elot-gptel--insert-format-curies '(("ex:foo" . nil) ("ex:bar" . "")))
    "Minted CURIEs:\n  - ex:foo\n  - ex:bar")))


;;; ---------------------------------------------------------------------------
;;; Tool-spec + dispatcher arity
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-insert-test-spec-sibling-registered ()
  (let ((spec (assoc "elot_insert_sibling_resource"
                     elot-gptel--tool-specs)))
    (should spec)
    (let* ((plist (cdr spec))
           (args (plist-get plist :args)))
      (should (eq (plist-get plist :function)
                  'elot-gptel-tool-insert-sibling-resource))
      (should (eq (plist-get plist :confirm) t))
      (should (= 3 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "anchor"))
      (should (equal (plist-get (nth 2 args) :name) "labels")))))

(ert-deftest elot-gptel-insert-test-spec-child-registered ()
  (let ((spec (assoc "elot_insert_child_resource"
                     elot-gptel--tool-specs)))
    (should spec)
    (let* ((plist (cdr spec))
           (args (plist-get plist :args)))
      (should (eq (plist-get plist :function)
                  'elot-gptel-tool-insert-child-resource))
      (should (eq (plist-get plist :confirm) t))
      (should (= 3 (length args))))))

(ert-deftest elot-gptel-insert-test-spec-tree-registered ()
  (let ((spec (assoc "elot_insert_resource_tree"
                     elot-gptel--tool-specs)))
    (should spec)
    (let* ((plist (cdr spec))
           (args (plist-get plist :args)))
      (should (eq (plist-get plist :function)
                  'elot-gptel-tool-insert-resource-tree))
      (should (eq (plist-get plist :confirm) t))
      (should (= 4 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "anchor"))
      (should (equal (plist-get (nth 2 args) :name) "tree"))
      (should (equal (plist-get (nth 3 args) :name) "as")))))

(ert-deftest elot-gptel-insert-test-dispatcher-sibling-arity ()
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-insert-sibling-resource)))
    (should (functionp thunk))
    (cl-letf (((symbol-function 'elot-gptel-tool-insert-sibling-resource)
               (lambda (&rest args) (format "got %d args" (length args)))))
      (should (string= "got 3 args"
                       (funcall thunk "f" "a" '("L")))))))

(ert-deftest elot-gptel-insert-test-dispatcher-child-arity ()
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-insert-child-resource)))
    (should (functionp thunk))
    (cl-letf (((symbol-function 'elot-gptel-tool-insert-child-resource)
               (lambda (&rest args) (format "got %d args" (length args)))))
      (should (string= "got 3 args"
                       (funcall thunk "f" "a" '("L")))))))

(ert-deftest elot-gptel-insert-test-dispatcher-tree-arity ()
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-insert-resource-tree)))
    (should (functionp thunk))
    (cl-letf (((symbol-function 'elot-gptel-tool-insert-resource-tree)
               (lambda (&rest args) (format "got %d args" (length args)))))
      (should (string= "got 4 args"
                       (funcall thunk "f" "a" '("L") "sibling"))))))


(provide 'elot-gptel-insert-test)
;;; elot-gptel-insert-test.el ends here
