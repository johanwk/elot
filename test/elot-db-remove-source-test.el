;;; elot-db-remove-source-test.el --- Tests for elot-db-remove-source  -*- lexical-binding: t; -*-

;; Usage:  make -C test remove-source-test  (or)
;;         cd test && emacs --batch -l ./test-helper.el \
;;              -l elot-db-remove-source-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Briefing test-helper-rollout-and-source-removal.org, Task 2.6.
;; Covers `elot-db-remove-source' (exact + LIKE-pattern modes) and
;; the companion `elot-db-list-sources-like' preview helper.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

;;;; Harness -- a fresh per-test temp sqlite, regardless of what the
;;;; test-helper has set elot-db-file to.

(defvar elot-rs-test--tmpfile nil)

(defun elot-rs-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-rs-test--tmpfile
             (file-exists-p elot-rs-test--tmpfile))
    (ignore-errors (delete-file elot-rs-test--tmpfile)))
  (setq elot-rs-test--tmpfile
        (make-temp-file "elot-db-remove-source-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-rs-test--tmpfile))
  (elot-db-init elot-rs-test--tmpfile))

(defun elot-rs-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-rs-test--tmpfile
             (file-exists-p elot-rs-test--tmpfile))
    (ignore-errors (delete-file elot-rs-test--tmpfile)))
  (setq elot-rs-test--tmpfile nil))

(defmacro elot-rs-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-rs-test--fresh-db) ,@body)
     (elot-rs-test--teardown)))

(defun elot-rs-test--seed-fixtures ()
  "Seed a handful of representative sources with entities + prefix rows."
  (elot-db-update-source
   "/tmp/elot-edit-axioms-empties-001.org" nil "org"
   '(("e1" "E1" ("rdf:type" "owl:Class"))))
  (elot-db-add-prefix "/tmp/elot-edit-axioms-empties-001.org" nil
                      "ex" "http://example.org/")
  (elot-db-update-source
   "/tmp/elot-edit-axioms-empties-002.org" nil "org"
   '(("e2" "E2" ("rdf:type" "owl:Class"))))
  (elot-db-update-source
   "/home/u/keep-me.org" nil "org"
   '(("k1" "Keep" ("rdf:type" "owl:Class"))))
  (elot-db-add-prefix "/home/u/keep-me.org" nil
                      "ex" "http://example.org/"))

;;;; Exact-match mode

(ert-deftest test-rs-exact-removes-and-cascades ()
  "Exact-match delete removes the row, cascades to entities /
attributes / prefixes, leaves siblings untouched."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (should (= 1 (elot-db-remove-source
                  "/tmp/elot-edit-axioms-empties-001.org")))
    (should-not (elot-db-source-exists-p
                 "/tmp/elot-edit-axioms-empties-001.org"))
    ;; The other two sources survive.
    (should (elot-db-source-exists-p
             "/tmp/elot-edit-axioms-empties-002.org"))
    (should (elot-db-source-exists-p "/home/u/keep-me.org"))
    ;; Cascade: no entity / attribute / prefix row for the removed source.
    (should (= 0 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM entities
                          WHERE source = ?"
                        (list "/tmp/elot-edit-axioms-empties-001.org")))))
    (should (= 0 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM attributes
                          WHERE source = ?"
                        (list "/tmp/elot-edit-axioms-empties-001.org")))))
    (should (= 0 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM prefixes
                          WHERE source = ?"
                        (list "/tmp/elot-edit-axioms-empties-001.org")))))
    ;; Cascade did NOT touch the surviving source's prefix row.
    (should (= 1 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM prefixes
                          WHERE source = ?"
                        (list "/home/u/keep-me.org")))))))

(ert-deftest test-rs-exact-unknown-source-returns-0 ()
  "Removing an unknown source returns 0 (idempotent, no error)."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (should (= 0 (elot-db-remove-source "/no/such/source.org")))
    ;; Nothing changed.
    (should (= 3 (length (elot-db-list-sources))))))

(ert-deftest test-rs-exact-data-source-disambiguation ()
  "Two sources sharing SOURCE but with different DATA-SOURCE are
removable independently."
  (elot-rs-test--with-fresh-db
    (elot-db-update-source "q.rq" "a.ttl" "rq"
                           '(("e1" "A" ("p" "v"))) 1.0)
    (elot-db-update-source "q.rq" "b.ttl" "rq"
                           '(("e2" "B" ("p" "v"))) 2.0)
    (should (= 1 (elot-db-remove-source "q.rq" "a.ttl")))
    (should-not (elot-db-source-exists-p "q.rq" "a.ttl"))
    (should (elot-db-source-exists-p "q.rq" "b.ttl"))))

;;;; LIKE-pattern mode

(ert-deftest test-rs-like-matches-multiple ()
  "Pattern mode removes every matching source and cascades for each."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (should (= 2 (elot-db-remove-source
                  "%elot-edit-axioms-empties-%.org" nil t)))
    ;; Both fixture rows are gone; the unrelated source survives.
    (should-not (elot-db-source-exists-p
                 "/tmp/elot-edit-axioms-empties-001.org"))
    (should-not (elot-db-source-exists-p
                 "/tmp/elot-edit-axioms-empties-002.org"))
    (should (elot-db-source-exists-p "/home/u/keep-me.org"))
    ;; Cascade: the surviving source's child rows are intact.
    (should (= 1 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM entities"))))
    (should (= 1 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM prefixes"))))))

(ert-deftest test-rs-like-no-match-returns-0 ()
  "Pattern mode with no matches returns 0 and leaves the DB alone."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (should (= 0 (elot-db-remove-source "%no-such-pattern%" nil t)))
    (should (= 3 (length (elot-db-list-sources))))))

(ert-deftest test-rs-like-data-source-default-is-any ()
  "In LIKE mode, nil DATA-SOURCE defaults to `%' (match any)."
  (elot-rs-test--with-fresh-db
    (elot-db-update-source "q.rq" "a.ttl" "rq"
                           '(("e1" "A" ("p" "v"))) 1.0)
    (elot-db-update-source "q.rq" "b.ttl" "rq"
                           '(("e2" "B" ("p" "v"))) 2.0)
    (elot-db-update-source "q.rq" "c.ttl" "rq"
                           '(("e3" "C" ("p" "v"))) 3.0)
    (should (= 3 (elot-db-remove-source "q.rq" nil t)))
    (should (= 0 (length (elot-db-list-sources))))))

(ert-deftest test-rs-like-data-source-pattern ()
  "DATA-SOURCE is honoured as its own LIKE pattern."
  (elot-rs-test--with-fresh-db
    (elot-db-update-source "q.rq" "a.ttl" "rq"
                           '(("e1" "A" ("p" "v"))) 1.0)
    (elot-db-update-source "q.rq" "b.ttl" "rq"
                           '(("e2" "B" ("p" "v"))) 2.0)
    (elot-db-update-source "q.rq" "c.csv" "rq"
                           '(("e3" "C" ("p" "v"))) 3.0)
    (should (= 2 (elot-db-remove-source "q.rq" "%.ttl" t)))
    (should-not (elot-db-source-exists-p "q.rq" "a.ttl"))
    (should-not (elot-db-source-exists-p "q.rq" "b.ttl"))
    (should (elot-db-source-exists-p "q.rq" "c.csv"))))

;;;; ALLOW-ALL guard

(ert-deftest test-rs-like-refuses-percent-without-allow-all ()
  "SOURCE=\"%\" in LIKE mode is refused unless ALLOW-ALL is non-nil."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (should-error (elot-db-remove-source "%" nil t))
    (should-error (elot-db-remove-source "%" nil t nil))
    ;; Nothing was deleted.
    (should (= 3 (length (elot-db-list-sources))))))

(ert-deftest test-rs-like-refuses-empty-without-allow-all ()
  "SOURCE=\"\" in LIKE mode is refused unless ALLOW-ALL is non-nil."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (should-error (elot-db-remove-source "" nil t))
    (should (= 3 (length (elot-db-list-sources))))))

(ert-deftest test-rs-like-allow-all-nukes-everything ()
  "With ALLOW-ALL, SOURCE=\"%\" removes every source."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (let ((n (elot-db-remove-source "%" nil t t)))
      (should (= 3 n)))
    (should (= 0 (length (elot-db-list-sources))))
    ;; Cascade fired for every source.
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM entities"))))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM attributes"))))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM prefixes"))))))

;;;; Preview helper

(ert-deftest test-rs-list-sources-like-preview ()
  "`elot-db-list-sources-like' returns the rows a matching
remove-source call would target, in the same shape as
`elot-db-list-sources'."
  (elot-rs-test--with-fresh-db
    (elot-rs-test--seed-fixtures)
    (let ((rows (elot-db-list-sources-like
                 "%elot-edit-axioms-empties-%.org")))
      (should (= 2 (length rows)))
      (should (cl-every (lambda (r) (= 5 (length r))) rows))
      (should (cl-find "/tmp/elot-edit-axioms-empties-001.org"
                       rows :key #'car :test #'equal))
      (should (cl-find "/tmp/elot-edit-axioms-empties-002.org"
                       rows :key #'car :test #'equal))
      (should-not (cl-find "/home/u/keep-me.org"
                           rows :key #'car :test #'equal)))
    ;; Preview is read-only: nothing was removed.
    (should (= 3 (length (elot-db-list-sources))))))

(ert-deftest test-rs-list-sources-like-default-data-source ()
  "`elot-db-list-sources-like' DATA-SOURCE defaults to `%'."
  (elot-rs-test--with-fresh-db
    (elot-db-update-source "q.rq" "a.ttl" "rq"
                           '(("e1" "A" ("p" "v"))) 1.0)
    (elot-db-update-source "q.rq" "b.ttl" "rq"
                           '(("e2" "B" ("p" "v"))) 2.0)
    (should (= 2 (length (elot-db-list-sources-like "q.rq"))))
    (should (= 1 (length (elot-db-list-sources-like "q.rq" "a.ttl"))))))

(provide 'elot-db-remove-source-test)

;;; elot-db-remove-source-test.el ends here
