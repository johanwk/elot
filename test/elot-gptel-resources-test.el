;;; elot-gptel-resources-test.el --- Tests for elot_resources  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-resources-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-V1-PLAN.org post-v1 M12 -- tests for the read-only
;; `elot_resources' inspection tool.  Pure Elisp; no ROBOT, no DB.
;; Exercises the row render, KIND derivation, definition-picking,
;; and the kind / match / prefix / limit filters (which compose).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-resources-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-resources-test--repo-root repo-root))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-gptel-resources-test--fixture ()
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
   "| iof-av: | https://example.org/iof-av/          |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   " - iof-av:naturalLanguageDefinition :: A living organism\n"
   "**** Dog (ex:dog)\n"
   " - rdfs:comment :: A domesticated carnivore\n"
   "**** Cat (ex:C_028TVPNEW)\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:dog\n"
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** scooby (ex:scooby)\n"
   " - Types :: ex:dog\n"))

(defun elot-gptel-resources-test--write ()
  (let ((path (expand-file-name
               (format "elot-resources-fixture-%d.org" (random 1000000))
               elot-gptel-resources-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-resources-test--fixture)))
    path))

(defmacro elot-gptel-resources-test--with-fixture (path-var &rest body)
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-resources-test--write)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-resources-test--rel (path)
  (file-relative-name path elot-gptel-resources-test--repo-root))

(defun elot-gptel-resources-test--run (path &rest args)
  (let ((default-directory elot-gptel-resources-test--repo-root))
    (apply #'elot-gptel-tool-resources
           (elot-gptel-resources-test--rel path) args)))

;;; ---------------------------------------------------------------------------
;;; Basic listing
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-resources-test-lists-all ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path)))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "CURIE" out))
      (should (string-match-p "KIND" out))
      (should (string-match-p "ex:animal" out))
      (should (string-match-p "ex:chases" out))
      (should (string-match-p "ex:scooby" out))
      ;; KIND column uses the pretty names.
      (should (string-match-p "Class" out))
      (should (string-match-p "ObjectProperty" out))
      (should (string-match-p "Individual" out)))))

(ert-deftest elot-gptel-resources-test-definition-picking ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path)))
      ;; iof-av definition preferred for ex:animal.
      (should (string-match-p "A living organism" out))
      ;; rdfs:comment used as fallback for ex:dog.
      (should (string-match-p "A domesticated carnivore" out))
      ;; No definition -> em-dash placeholder.
      (should (string-match-p "ex:C_028TVPNEW +Class +Cat +--" out)))))

;;; ---------------------------------------------------------------------------
;;; Filters
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-resources-test-kind-filter ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path "ObjectProperty")))
      (should (string-match-p "ex:chases" out))
      (should-not (string-match-p "ex:animal" out))
      (should-not (string-match-p "ex:scooby" out)))))

(ert-deftest elot-gptel-resources-test-kind-all ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path "all")))
      (should (string-match-p "ex:animal" out))
      (should (string-match-p "ex:chases" out)))))

(ert-deftest elot-gptel-resources-test-kind-unknown ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path "Bogus")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown kind" out)))))

(ert-deftest elot-gptel-resources-test-match-regexp-curie ()
  (elot-gptel-resources-test--with-fixture path
    ;; Case-insensitive regexp against CURIE finds a minted id.
    (let ((out (elot-gptel-resources-test--run path nil "^ex:c_")))
      (should (string-match-p "ex:C_028TVPNEW" out))
      (should-not (string-match-p "ex:animal" out)))))

(ert-deftest elot-gptel-resources-test-match-regexp-label ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path nil "dog")))
      (should (string-match-p "ex:dog" out))
      (should-not (string-match-p "ex:chases" out)))))

(ert-deftest elot-gptel-resources-test-prefix-filter ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path nil nil "ex")))
      (should (string-match-p "ex:animal" out))
      ;; iof-av / owl annotation-property vocab excluded.
      (should-not (string-match-p "iof-av:" out)))))

(ert-deftest elot-gptel-resources-test-filters-compose ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path "Class" "dog" "ex")))
      (should (string-match-p "ex:dog" out))
      (should-not (string-match-p "ex:chases" out))
      (should-not (string-match-p "ex:animal" out)))))

(ert-deftest elot-gptel-resources-test-limit-truncates ()
  (elot-gptel-resources-test--with-fixture path
    (let ((out (elot-gptel-resources-test--run path nil nil nil 2)))
      (should (string-match-p "more resource(s) omitted" out)))))

;;; ---------------------------------------------------------------------------
;;; Registration
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-resources-test-tool-spec-registered ()
  (let ((spec (assoc "elot_resources" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-resources))
    (should (null (plist-get (cdr spec) :confirm)))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 5 (length args)))
      (should (equal (plist-get (car args) :name) "file")))))

(provide 'elot-gptel-resources-test)
;;; elot-gptel-resources-test.el ends here
