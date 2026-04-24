;;; elot-db-crud-test.el --- ERT tests for elot-db Step 1.2  -*- lexical-binding: t; -*-

;; Usage:  make -C test crud-test  (or)
;;         cd test && emacs --batch -l elot-db-crud-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Step 1.2 tests: write/read primitives built on top of the schema
;; from Steps 1.1 / 1.1.1.  Exercises `elot-db-update-source',
;; `elot-db-source-exists-p', `elot-db-list-sources',
;; `elot-db-remove-source', `elot-db-source-needs-update-p',
;; `elot-db-get-label', `elot-db-get-attr',
;; `elot-db-get-all-attrs', and an integration check that
;; `elot-db-get-label-any' still works against data written via
;; `elot-db-update-source'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

;;;; Harness

(defvar elot-db-test--tmpfile nil)

(defun elot-db-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-test--tmpfile
             (file-exists-p elot-db-test--tmpfile))
    (ignore-errors (delete-file elot-db-test--tmpfile)))
  (setq elot-db-test--tmpfile
        (make-temp-file "elot-db-crud-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-db-test--tmpfile))
  (elot-db-init elot-db-test--tmpfile))

(defun elot-db-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-test--tmpfile
             (file-exists-p elot-db-test--tmpfile))
    (ignore-errors (delete-file elot-db-test--tmpfile)))
  (setq elot-db-test--tmpfile nil))

(defmacro elot-db-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-db-test--fresh-db) ,@body)
     (elot-db-test--teardown)))

;;;; Subtask A: write path

(ert-deftest test-elot-db-update-source-roundtrip ()
  "Insert two entities with attributes, read back identical via the
low-level read API; entity count, attribute count, and label match."
  (elot-db-test--with-fresh-db
    (let ((data
           '(("e1" "Entity One"
              ("rdf:type" "owl:Class" "skos:definition" "the one"))
             ("e2" "Entity Two"
              ("rdf:type" "owl:Class")))))
      (should (= 2 (elot-db-update-source "s1" nil "org" data 123.0)))
      (should (elot-db-source-exists-p "s1"))
      (should (= 2 (caar (sqlite-select
                          elot-db
                          "SELECT COUNT(*) FROM entities WHERE source = 's1'"))))
      (should (= 3 (caar (sqlite-select
                          elot-db
                          "SELECT COUNT(*) FROM attributes WHERE source = 's1'"))))
      (should (equal "Entity One"
                     (caar (sqlite-select
                            elot-db
                            "SELECT label FROM entities WHERE id = 'e1'"))))
      (should (equal 123.0
                     (caar (sqlite-select
                            elot-db
                            "SELECT last_modified FROM sources WHERE source = 's1'")))))))

(ert-deftest test-elot-db-update-source-replaces ()
  "Re-inserting under the same (source, data_source) wipes the old
rows and replaces them with the new set."
  (elot-db-test--with-fresh-db
    (elot-db-update-source
     "s1" nil "org"
     '(("e1" "Old One" ("rdf:type" "owl:Class"))
       ("e2" "Old Two" ("rdf:type" "owl:Class"))))
    (elot-db-update-source
     "s1" nil "org"
     '(("e3" "New Three" ("rdf:type" "owl:Thing"))))
    (should (equal '(("e3"))
                   (sqlite-select
                    elot-db
                    "SELECT id FROM entities WHERE source = 's1' ORDER BY id")))
    (should (= 1 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM attributes WHERE source = 's1'"))))))

(ert-deftest test-elot-db-update-source-kind-keyword ()
  "A :kind keyword in the plist populates entities.kind without
leaking into attributes."
  (elot-db-test--with-fresh-db
    (elot-db-update-source
     "s1" nil "org"
     '(("http://example.org/x" "X" (:kind "uri" "rdf:type" "owl:Class"))
       ("ex:y" "Y" (:kind "curie"))))
    (should (equal "uri"
                   (caar (sqlite-select
                          elot-db
                          "SELECT kind FROM entities WHERE id = 'http://example.org/x'"))))
    (should (equal "curie"
                   (caar (sqlite-select
                          elot-db
                          "SELECT kind FROM entities WHERE id = 'ex:y'"))))
    (should (= 1 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM attributes WHERE source = 's1'"))))
    ;; No attribute row whose prop looks like a keyword.
    (should (null (sqlite-select
                   elot-db
                   "SELECT prop FROM attributes WHERE prop LIKE ':%'")))))

(ert-deftest test-elot-db-update-source-data-source-pair ()
  "(SOURCE, DATA-SOURCE) pairs are independent: same SOURCE with two
different DATA-SOURCE values yields two independent sources rows."
  (elot-db-test--with-fresh-db
    (elot-db-update-source "q.rq" "a.ttl" "rq"
                           '(("e1" "A" ("p" "v"))) 1.0)
    (elot-db-update-source "q.rq" "b.ttl" "rq"
                           '(("e2" "B" ("p" "v"))) 2.0)
    (should (elot-db-source-exists-p "q.rq" "a.ttl"))
    (should (elot-db-source-exists-p "q.rq" "b.ttl"))
    (should (= 2 (length (elot-db-list-sources))))))

(ert-deftest test-elot-db-list-sources ()
  "list-sources returns one row per registered source."
  (elot-db-test--with-fresh-db
    (elot-db-update-source "s1" nil "org" '(("e1" "E1" nil)))
    (elot-db-update-source "s2" nil "csv" '(("e2" "E2" nil)))
    (let ((rows (elot-db-list-sources)))
      (should (= 2 (length rows)))
      (should (cl-find "s1" rows :key #'car :test #'equal))
      (should (cl-find "s2" rows :key #'car :test #'equal)))))

(ert-deftest test-elot-db-remove-source ()
  "remove-source deletes the sources row and cascades."
  (elot-db-test--with-fresh-db
    (elot-db-update-source
     "s1" nil "org"
     '(("e1" "E1" ("rdf:type" "owl:Class"))))
    (elot-db-add-prefix "s1" nil "ex" "http://example.org/")
    (should (elot-db-remove-source "s1"))
    (should-not (elot-db-source-exists-p "s1"))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM entities"))))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM attributes"))))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM prefixes"))))
    ;; Removing a non-existent source is a no-op returning nil.
    (should-not (elot-db-remove-source "does-not-exist"))))

(ert-deftest test-elot-db-source-needs-update-p ()
  "needs-update-p is true for unknown sources, true when file mtime
is newer than stored, false when stored mtime is newer or equal."
  (elot-db-test--with-fresh-db
    (let ((tmp (make-temp-file "elot-db-mtime-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "hello\n"))
            ;; Unknown source.
            (should (elot-db-source-needs-update-p tmp))
            ;; After registering with a mtime in the far future,
            ;; needs-update-p is false.
            (elot-db-update-source
             tmp nil "csv"
             '(("e1" "E1" nil))
             (+ (float-time) 1e9))
            (should-not (elot-db-source-needs-update-p tmp))
            ;; Register with a mtime in the distant past -> stale.
            (elot-db-update-source
             tmp nil "csv" '(("e1" "E1" nil)) 0.0)
            (should (elot-db-source-needs-update-p tmp)))
        (ignore-errors (delete-file tmp))))))

;;;; Subtask B: priority-aware read path

(ert-deftest test-elot-db-get-label-empty-active-sources ()
  "Empty / nil active-sources yields nil (not an error)."
  (elot-db-test--with-fresh-db
    (elot-db-update-source "s1" nil "csv" '(("e1" "E1" nil)))
    (should (null (elot-db-get-label "e1" nil)))
    (should (null (elot-db-get-label "e1" '())))))

(ert-deftest test-elot-db-get-label-priority ()
  "Same id in two sources: the earlier entry in active-sources wins."
  (elot-db-test--with-fresh-db
    (elot-db-update-source "A" nil "csv" '(("e1" "Label from A" nil)))
    (elot-db-update-source "B" nil "csv" '(("e1" "Label from B" nil)))
    (should (equal "Label from A"
                   (elot-db-get-label "e1" '(("A" nil) ("B" nil)))))
    (should (equal "Label from B"
                   (elot-db-get-label "e1" '(("B" nil) ("A" nil)))))
    ;; A source that isn't active is never consulted.
    (should (null (elot-db-get-label "e1" '(("C" nil)))))))

(ert-deftest test-elot-db-get-attr-priority ()
  "Attribute lookup respects active-sources priority order."
  (elot-db-test--with-fresh-db
    (elot-db-update-source "A" nil "csv"
                           '(("e1" "E1" ("prop" "from A"))))
    (elot-db-update-source "B" nil "csv"
                           '(("e1" "E1" ("prop" "from B"))))
    (should (equal "from A"
                   (elot-db-get-attr "e1" "prop" '(("A" nil) ("B" nil)))))
    (should (equal "from B"
                   (elot-db-get-attr "e1" "prop" '(("B" nil) ("A" nil)))))
    (should (null (elot-db-get-attr "e1" "no-such-prop"
                                    '(("A" nil) ("B" nil)))))))

(ert-deftest test-elot-db-get-all-attrs ()
  "get-all-attrs returns a plist from the first active source that
has any attribute rows for ID."
  (elot-db-test--with-fresh-db
    (elot-db-update-source
     "A" nil "csv"
     '(("e1" "E1" ("p1" "v1" "p2" "v2"))))
    (let ((pl (elot-db-get-all-attrs "e1" '(("A" nil)))))
      (should (equal "v1" (plist-get pl "p1" #'equal)))
      (should (equal "v2" (plist-get pl "p2" #'equal)))
      ;; Step 1.14: plist now carries :source-origin alongside the
      ;; string-keyed prop/value pairs, so total length is 4 + 2.
      (should (= 6 (length pl)))
      (should (equal (cons "A" "") (plist-get pl :source-origin))))
    (should (null (elot-db-get-all-attrs "no-such" '(("A" nil)))))))

(ert-deftest test-elot-db-get-label-respects-buffer-local ()
  "When ACTIVE-SOURCES is omitted, `elot-active-label-sources' is used."
  (elot-db-test--with-fresh-db
    (elot-db-update-source "A" nil "csv" '(("e1" "From A" nil)))
    (let ((elot-active-label-sources '(("A" nil))))
      (should (equal "From A" (elot-db-get-label "e1"))))))

;;;; Integration: get-label-any still works after update-source

(ert-deftest test-elot-db-get-label-any-after-update-source ()
  "End-to-end: populate the DB via `elot-db-update-source' (including
prefix rows) and confirm that all three passes of
`elot-db-get-label-any' still work."
  (elot-db-test--with-fresh-db
    ;; Source A: CURIE-stored entity, no prefix row.
    (elot-db-update-source
     "A" nil "csv"
     '(("ex:hello" "Greeting" (:kind "curie"))))
    ;; Source B: URI-stored entity + prefix binding.
    (elot-db-update-source
     "B" nil "csv"
     '(("http://example.org/world" "World" (:kind "uri"))))
    (elot-db-add-prefix "B" nil "ex" "http://example.org/")
    (let ((active '(("A" nil) ("B" nil))))
      ;; Pass 1: literal id.
      (should (equal "Greeting"
                     (elot-db-get-label-any "ex:hello" active)))
      ;; Pass 2: CURIE -> URI via source B's prefix.
      (should (equal "World"
                     (elot-db-get-label-any "ex:world" active)))
      ;; Pass 3: URI -> CURIE (no row stored as URI for hello, but
      ;; contraction yields ex:hello which matches literally in A).
      (should (equal "Greeting"
                     (elot-db-get-label-any "http://example.org/hello"
                                            active))))))

(provide 'elot-db-crud-test)

;;; elot-db-crud-test.el ends here
