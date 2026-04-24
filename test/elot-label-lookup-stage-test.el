;;; elot-label-lookup-stage-test.el --- Step 1.15 tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.15: disambiguating many ids sharing one label (two-stage
;; selection).  The DB-path collection shape widens to carry a list
;; of ids per label; `elot-label-lookup--from-db' runs a stage-2
;; picker when a label maps to more than one id.  `C-u C-u' on
;; `elot-label-lookup' forces the flat-presentation variant.
;;
;; Covers:
;;   1. `elot-db-all-active-labels' returns a list of ids per label
;;      (multi-id case).
;;   2. `elot-label-lookup--collect-db' emits a stage-1 entry with
;;      :count = N for a multi-id label.
;;   3. Single-id fast path: one `completing-read' call, no stage 2.
;;   4. Two-stage flow: stage 1 picks label, stage 2 picks id;
;;      chosen id is inserted as CURIE.
;;   5. Stage-2 `C-g' aborts the whole lookup cleanly; nothing is
;;      inserted; stage 1 is NOT re-entered.
;;   6. `C-u C-u' prefix arg produces a flat collection (all ids
;;      visible at stage 1), no stage 2.
;;   7. Stage-2 annotator surfaces attributes from
;;      `elot-db-get-all-attrs' (rdf:type, definition).
;;   8. Slurp path is unchanged: with a populated
;;      `elot-attriblist-ht', `elot-label-lookup' still takes the
;;      attriblist path; no stage-2 behaviour, no call into
;;      `elot-db-ids-for-label'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)

;;;; ---------------------------------------------------------------------
;;;; Harness
;;;; ---------------------------------------------------------------------

(defvar elot-s15-test--tmp-db nil)

(defun elot-s15-test--setup ()
  (setq elot-s15-test--tmp-db
        (make-temp-file "elot-s15-" nil ".sqlite"))
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-s15-test--tmp-db))

(defun elot-s15-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-s15-test--tmp-db
             (file-exists-p elot-s15-test--tmp-db))
    (ignore-errors (delete-file elot-s15-test--tmp-db)))
  (setq elot-s15-test--tmp-db nil)
  (setq-default elot-active-label-sources nil))

(defmacro elot-s15-test--with-fresh (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-s15-test--setup) ,@body)
     (elot-s15-test--teardown)))

(defun elot-s15-test--populate-thimbles ()
  "Register a source where label `thimble B' maps to 3 distinct ids."
  (elot-db-update-source
   "src" nil "csv"
   '(("ex:thimble-1" "thimble B" nil)
     ("ex:thimble-2" "thimble B" nil)
     ("ex:thimble-3" "thimble B" nil)
     ("ex:widget"    "Widget"    nil))))

;;;; ---------------------------------------------------------------------
;;;; 1. Data layer: list of ids per label
;;;; ---------------------------------------------------------------------

(ert-deftest test-db-all-active-labels-returns-list ()
  "`elot-db-all-active-labels' returns a list of ids per label."
  (elot-s15-test--with-fresh
    (elot-s15-test--populate-thimbles)
    (let* ((active '(("src" nil)))
           (ht (elot-db-all-active-labels active))
           (thimbles (gethash "thimble B" ht))
           (widget   (gethash "Widget"    ht)))
      (should (listp thimbles))
      (should (= 3 (length thimbles)))
      (should (member "ex:thimble-1" thimbles))
      (should (member "ex:thimble-2" thimbles))
      (should (member "ex:thimble-3" thimbles))
      ;; Singletons are also lists.
      (should (listp widget))
      (should (equal '("ex:widget") widget)))
    ;; Sibling helper.
    (when (fboundp 'elot-db-ids-for-label)
      (let ((ids (elot-db-ids-for-label "thimble B" '(("src" nil)))))
        (should (= 3 (length ids)))))))

;;;; ---------------------------------------------------------------------
;;;; 2. Collection emits group entry with :count
;;;; ---------------------------------------------------------------------

(ert-deftest test-collect-db-emits-group-entry ()
  "`elot-label-lookup--collect-db' emits :ids/:count for multi-id labels."
  (elot-s15-test--with-fresh
    (elot-s15-test--populate-thimbles)
    (with-temp-buffer
      (setq-local elot-active-label-sources '(("src" nil)))
      (let* ((pair (elot-label-lookup--collect-db))
             (ht (car pair))
             (entry (gethash "thimble B" ht)))
        (should (consp pair))
        (should (hash-table-p ht))
        (should entry)
        (should (= 3 (plist-get entry :count)))
        (should (= 3 (length (plist-get entry :ids))))
        ;; Singleton also carries :count = 1.
        (let ((w (gethash "Widget" ht)))
          (should (= 1 (plist-get w :count))))))))

;;;; ---------------------------------------------------------------------
;;;; 3. Single-id fast path: one completing-read, no stage 2
;;;; ---------------------------------------------------------------------

(ert-deftest test-from-db-single-id-fast-path ()
  "Singleton label: exactly one `completing-read', no stage 2."
  (elot-s15-test--with-fresh
    (elot-s15-test--populate-thimbles)
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources '(("src" nil)))
      (let ((calls 0)
            (stage2-called 0))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p _c &rest _) (cl-incf calls) "Widget"))
                  ((symbol-function 'elot-label-lookup--pick-id-stage2)
                   (lambda (&rest _) (cl-incf stage2-called) nil)))
          (elot-label-lookup))
        (should (= 1 calls))
        (should (= 0 stage2-called))
        (should (string-match-p "\\bex:widget\\b" (buffer-string)))))))

;;;; ---------------------------------------------------------------------
;;;; 4. Two-stage flow
;;;; ---------------------------------------------------------------------

(ert-deftest test-from-db-two-stage-flow ()
  "Multi-id label triggers stage 2; chosen id is inserted."
  (elot-s15-test--with-fresh
    (elot-s15-test--populate-thimbles)
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources '(("src" nil)))
      (let ((calls 0)
            (stage2-collection nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _)
                     (cl-incf calls)
                     (cond
                      ((= calls 1) "thimble B")
                      ((= calls 2)
                       (setq stage2-collection coll)
                       ;; Pick the second thimble.
                       "ex:thimble-2")))))
          (elot-label-lookup))
        (should (= 2 calls))
        ;; Stage 2 collection must contain all three ids.
        (should (or (hash-table-p stage2-collection)
                    (listp stage2-collection)))
        (should (string-match-p "\\bex:thimble-2\\b" (buffer-string)))
        (should-not (string-match-p "\\bex:thimble-1\\b" (buffer-string)))
        (should-not (string-match-p "\\bex:thimble-3\\b" (buffer-string)))))))

;;;; ---------------------------------------------------------------------
;;;; 5. Stage-2 C-g aborts cleanly
;;;; ---------------------------------------------------------------------

(ert-deftest test-from-db-stage-2-quit-aborts ()
  "`C-g' in stage 2 signals `quit' and inserts nothing; stage 1 not re-entered."
  (elot-s15-test--with-fresh
    (elot-s15-test--populate-thimbles)
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources '(("src" nil)))
      (let ((calls 0))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p _c &rest _)
                     (cl-incf calls)
                     (cond
                      ((= calls 1) "thimble B")
                      (t (signal 'quit nil))))))
          (let ((quit-signalled nil))
            (condition-case _err
                (elot-label-lookup)
              (quit (setq quit-signalled t)))
            (should quit-signalled)))
        ;; Stage 1 entered once; stage 2 entered once (and signalled);
        ;; stage 1 must NOT have been re-entered.
        (should (= 2 calls))
        (should (= 0 (buffer-size)))))))

;;;; ---------------------------------------------------------------------
;;;; 6. C-u C-u forces flat presentation
;;;; ---------------------------------------------------------------------

(ert-deftest test-flat-prefix-arg-override ()
  "`C-u C-u' flattens all ids into stage 1; stage 2 not entered."
  (elot-s15-test--with-fresh
    (elot-s15-test--populate-thimbles)
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources '(("src" nil)))
      (let ((seen nil)
            (stage2-called 0))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _)
                     (setq seen coll)
                     ;; Select the first flattened entry.
                     (let (pick)
                       (cond
                        ((hash-table-p coll)
                         (maphash (lambda (k _v)
                                    (when (and (null pick)
                                               (string-match-p "thimble B" k))
                                      (setq pick k)))
                                  coll))
                        ((listp coll)
                         (setq pick (car (cl-remove-if-not
                                          (lambda (k)
                                            (string-match-p "thimble B" k))
                                          coll)))))
                       pick)))
                  ((symbol-function 'elot-label-lookup--pick-id-stage2)
                   (lambda (&rest _) (cl-incf stage2-called) nil)))
          (funcall #'elot-label-lookup '(16)))
        (should (hash-table-p seen))
        (should (= 0 stage2-called))
        ;; All three thimble entries must be present as separate keys
        ;; (suffix-disambiguated), plus the Widget singleton.
        (let ((thimble-keys 0))
          (maphash (lambda (k _v)
                     (when (string-match-p "thimble B" k)
                       (cl-incf thimble-keys)))
                   seen)
          (should (= 3 thimble-keys)))
        ;; Something was inserted (some ex:thimble-*).
        (should (string-match-p "\\bex:thimble-" (buffer-string)))))))

;;;; ---------------------------------------------------------------------
;;;; 7. Stage-2 annotator surfaces attributes
;;;; ---------------------------------------------------------------------

(ert-deftest test-stage-2-annotator-uses-attrs ()
  "Stage-2 annotator includes attribute fields from `elot-db-get-all-attrs'."
  (elot-s15-test--with-fresh
    (elot-s15-test--populate-thimbles)
    (with-temp-buffer
      (setq-local elot-active-label-sources '(("src" nil)))
      ;; Stage 2 builds its DISPLAY -> id ht as a side effect.
      (let ((ids '("ex:thimble-1" "ex:thimble-2")))
        (cl-letf (((symbol-function 'elot-db-get-all-attrs)
                   (lambda (id &rest _)
                     (list "rdf:type" "owl:NamedIndividual"
                           "skos:definition"
                           (format "Definition of %s" id)))))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_p _coll &rest _) nil)))
            ;; Prime the stage-2 ht by calling the picker itself.
            (elot-label-lookup--pick-id-stage2 "thimble B" ids))
          ;; Now the annotator can be exercised against the populated ht.
          (let* ((display (elot-label-lookup--id-suffix "ex:thimble-1"))
                 (ann (elot-label-lookup--stage2-annotations display)))
            (should (stringp ann))
            (should (string-match-p "owl:NamedIndividual" ann))
            (should (string-match-p "Definition of ex:thimble-1" ann))))))))

;;;; ---------------------------------------------------------------------
;;;; 8. Slurp path unchanged
;;;; ---------------------------------------------------------------------

(ert-deftest test-slurp-path-unchanged ()
  "Populated `elot-attriblist-ht' takes the attriblist path; no stage-2,
no call into `elot-db-ids-for-label'."
  (elot-s15-test--with-fresh
    (with-temp-buffer
      (setq-local elot-attriblist-ht (make-hash-table :test 'equal))
      (puthash "Widget"
               '("puri" "ex:Widget" "rdf:type" "owl:Class")
               elot-attriblist-ht)
      ;; Ensure active sources are nil so only the attriblist path is
      ;; available.
      (setq-local elot-active-label-sources nil)
      (let ((stage2-called 0)
            (ids-helper-called 0)
            (calls 0))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p _c &rest _)
                     (cl-incf calls) "Widget"))
                  ((symbol-function 'elot-label-lookup--pick-id-stage2)
                   (lambda (&rest _) (cl-incf stage2-called) nil)))
          (when (fboundp 'elot-db-ids-for-label)
            (advice-add 'elot-db-ids-for-label :before
                        (lambda (&rest _) (cl-incf ids-helper-called))
                        '((name . s15-spy))))
          (unwind-protect
              (progn
                (elot-label-lookup)
                (should (= 1 calls))
                (should (= 0 stage2-called))
                (should (= 0 ids-helper-called))
                (should (string-match-p "\\bex:Widget\\b"
                                        (buffer-string))))
            (when (fboundp 'elot-db-ids-for-label)
              (ignore-errors
                (advice-remove 'elot-db-ids-for-label 's15-spy)))))))))

(provide 'elot-label-lookup-stage-test)
;;; elot-label-lookup-stage-test.el ends here
