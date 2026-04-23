;;; elot-ttl-prefix-test.el --- Tests for Step 1.7.3  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.7.3: TTL prefix harvest + dual-form (IRI / CURIE) matcher
;; for `elot-global-label-display-mode'.
;;
;; Exercises, in order of increasing scope:
;;   A. The pure harvester `elot-source--harvest-prefixes'.
;;   B. End-to-end register-TTL writes prefix rows via
;;      `elot-db-add-prefix' (ROBOT-gated).
;;   C. `elot-db-all-active-ids' with include-curies=t returns both
;;      IRI and CURIE forms for ingested sources.
;;   D. `elot-global-label-display-mode' decorates both forms in a
;;      buffer, and the safety-net cap falls back cleanly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)

(defconst elot-ttlpx-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun elot-ttlpx-test--fx (name)
  (expand-file-name (concat "fixtures/" name) elot-ttlpx-test--dir))

(defvar elot-ttlpx-test--tmp-db nil)
(defvar elot-ttlpx-test--tmp-files nil)

(defun elot-ttlpx-test--setup ()
  (setq elot-ttlpx-test--tmp-db
        (make-temp-file "elot-ttlpx-" nil ".sqlite"))
  (setq elot-ttlpx-test--tmp-files nil)
  (when (and (boundp 'elot-db) elot-db)
    (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-ttlpx-test--tmp-db))

(defun elot-ttlpx-test--teardown ()
  (when (and (boundp 'elot-db) elot-db)
    (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-ttlpx-test--tmp-db
             (file-exists-p elot-ttlpx-test--tmp-db))
    (ignore-errors (delete-file elot-ttlpx-test--tmp-db)))
  (dolist (f elot-ttlpx-test--tmp-files)
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f))))
  (setq-default elot-active-label-sources nil))

(defun elot-ttlpx-test--copy-fixture (name)
  (let* ((src (elot-ttlpx-test--fx name))
         (ext (file-name-extension name))
         (tmp (make-temp-file "elot-ttlpx-" nil (concat "." ext))))
    (copy-file src tmp t)
    (push tmp elot-ttlpx-test--tmp-files)
    tmp))

(defmacro elot-ttlpx-test--with-fresh (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (elot-ttlpx-test--setup) ,@body)
     (elot-ttlpx-test--teardown)))

(defun elot-ttlpx-test--robot-or-skip ()
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available")))

;;; ---------------------------------------------------------------------------
;;; A. Pure harvester
;;; ---------------------------------------------------------------------------

(ert-deftest test-ttl-prefix-harvest-basic ()
  "Harvester returns an alist of (PREFIX . EXPANSION) pairs."
  (let* ((alist (elot-source--harvest-prefixes
                 (elot-ttlpx-test--fx "prefixes.ttl"))))
    (should (equal "http://www.w3.org/2000/01/rdf-schema#"
                   (cdr (assoc "rdfs" alist))))
    (should (equal "http://www.w3.org/2004/02/skos/core#"
                   (cdr (assoc "skos" alist))))
    ;; Empty prefix supported.
    (should (equal "http://example.org/default/"
                   (cdr (assoc "" alist))))
    ;; SPARQL-style PREFIX line supported.
    (should (equal "http://example.org/ex/"
                   (cdr (assoc "ex" alist))))))

(ert-deftest test-ttl-prefix-harvest-first-wins ()
  "When the same prefix is declared twice, the first occurrence wins."
  (let ((alist (elot-source--harvest-prefixes
                (elot-ttlpx-test--fx "prefixes.ttl"))))
    (should (equal "http://www.w3.org/2000/01/rdf-schema#"
                   (cdr (assoc "rdfs" alist))))))

(ert-deftest test-ttl-prefix-harvest-smallttl ()
  "The regression fixture yields ellsea + rdl + rdfs + rdf."
  (let* ((alist (elot-source--harvest-prefixes
                 (elot-ttlpx-test--fx "smallttl.ttl"))))
    (should (equal "http://data.dnv.com/ontology/ellsea/asset/"
                   (cdr (assoc "ellsea" alist))))
    (should (equal "http://data.dnv.com/ontology/aps/rdl/"
                   (cdr (assoc "rdl" alist))))
    (should (assoc "rdfs" alist))
    (should (assoc "rdf"  alist))))

(ert-deftest test-ttl-prefix-harvest-missing-file ()
  "Harvesting a non-existent file returns nil, does not signal."
  (should (null (elot-source--harvest-prefixes
                 "/nonexistent/does-not-exist.ttl"))))

;;; ---------------------------------------------------------------------------
;;; B. Dispatcher contract: (:entries ... :prefixes ...)
;;; ---------------------------------------------------------------------------

(ert-deftest test-entries-and-prefixes-normaliser ()
  "`elot-source--entries-and-prefixes' accepts both shapes."
  ;; Bare list (back-compat).
  (let ((pair (elot-source--entries-and-prefixes
               '(("a" "A" nil) ("b" "B" nil)))))
    (should (= 2 (length (car pair))))
    (should (null (cdr pair))))
  ;; Extended plist.
  (let ((pair (elot-source--entries-and-prefixes
               (list :entries '(("a" "A" nil))
                     :prefixes '(("ex" . "http://example.org/"))))))
    (should (= 1 (length (car pair))))
    (should (equal (cdr pair) '(("ex" . "http://example.org/"))))))

;;; ---------------------------------------------------------------------------
;;; C. End-to-end TTL registration writes prefix rows (ROBOT-gated)
;;; ---------------------------------------------------------------------------

(ert-deftest test-register-ttl-adds-prefix-rows ()
  "Registering a TTL source writes its @prefix declarations."
  (elot-ttlpx-test--robot-or-skip)
  (elot-ttlpx-test--with-fresh
    (let* ((ttl (elot-ttlpx-test--copy-fixture "smallttl.ttl"))
           (_   (elot-label-register-source ttl))
           (rows (elot-db-list-prefixes ttl)))
      (should rows)
      ;; Find ellsea: among the rows.
      (let ((ellsea (cl-find-if
                     (lambda (r) (equal (nth 2 r) "ellsea"))
                     rows)))
        (should ellsea)
        (should (equal (nth 3 ellsea)
                       "http://data.dnv.com/ontology/ellsea/asset/"))))))

(ert-deftest test-smallttl-regression ()
  "After registering smallttl.ttl, CURIE-form expansion resolves IRIs."
  (elot-ttlpx-test--robot-or-skip)
  (elot-ttlpx-test--with-fresh
    (let ((ttl (elot-ttlpx-test--copy-fixture "smallttl.ttl")))
      (elot-label-register-source ttl)
      (setq-default elot-active-label-sources `((,ttl)))
      ;; CURIE expansion works for the source's own prefix.
      (should (equal
               "http://data.dnv.com/ontology/ellsea/asset/id51929cf8-5b83-483d-937a-eeaffbc2a907"
               (elot-db-expand-curie
                "ellsea:id51929cf8-5b83-483d-937a-eeaffbc2a907"
                `((,ttl)))))
      ;; Labels are reachable via the CURIE (pass 2 of get-label-any).
      (should (equal "thimble B"
                     (elot-db-get-label-any
                      "ellsea:id51929cf8-5b83-483d-937a-eeaffbc2a907"
                      `((,ttl))))))))

;;; ---------------------------------------------------------------------------
;;; D. all-active-ids with include-curies
;;; ---------------------------------------------------------------------------

(ert-deftest test-all-active-ids-include-curies ()
  "When include-curies is non-nil, CURIE forms appear in the id list."
  (elot-ttlpx-test--with-fresh
    (let ((ttl (elot-ttlpx-test--copy-fixture "smallttl.ttl")))
      ;; Simulate a TTL ingest without ROBOT: insert one IRI entity and the
      ;; ellsea prefix row directly.  This keeps the test fast and offline.
      (elot-db-update-source
       ttl nil "ttl"
       (list (list
              "http://data.dnv.com/ontology/ellsea/asset/id51929cf8-5b83-483d-937a-eeaffbc2a907"
              "thimble B" nil)))
      (elot-db-add-prefix ttl nil "ellsea"
                          "http://data.dnv.com/ontology/ellsea/asset/")
      (setq-default elot-active-label-sources `((,ttl)))
      (let ((ids-plain  (elot-db-all-active-ids nil nil))
            (ids-curies (elot-db-all-active-ids nil t)))
        (should (= 1 (length ids-plain)))
        (should (member
                 "http://data.dnv.com/ontology/ellsea/asset/id51929cf8-5b83-483d-937a-eeaffbc2a907"
                 ids-curies))
        (should (member
                 "ellsea:id51929cf8-5b83-483d-937a-eeaffbc2a907"
                 ids-curies))
        (should (> (length ids-curies) (length ids-plain)))))))

;;; ---------------------------------------------------------------------------
;;; E. Global mode matches both IRI and CURIE form
;;; ---------------------------------------------------------------------------

(ert-deftest test-global-mode-matches-curie-form-in-ttl-buffer ()
  "Global mode decorates CURIE-form tokens even though ids are full IRIs."
  (elot-ttlpx-test--with-fresh
    (let ((ttl (elot-ttlpx-test--copy-fixture "smallttl.ttl")))
      (elot-db-update-source
       ttl nil "ttl"
       (list (list
              "http://data.dnv.com/ontology/ellsea/asset/id51929cf8-5b83-483d-937a-eeaffbc2a907"
              "thimble B" nil)))
      (elot-db-add-prefix ttl nil "ellsea"
                          "http://data.dnv.com/ontology/ellsea/asset/")
      (with-temp-buffer
        (insert "ref ellsea:id51929cf8-5b83-483d-937a-eeaffbc2a907 more.\n")
        (setq-local elot-active-label-sources `((,ttl)))
        (elot-global-label-display-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (let* ((pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "ellsea:")
                      (match-beginning 0)))
               (label (get-text-property pos 'display)))
          (should (equal label "thimble B")))
        (elot-global-label-display-mode -1)))))

(ert-deftest test-global-mode-matches-iri-form-in-same-buffer ()
  "Global mode decorates full-IRI tokens in the same setup."
  (elot-ttlpx-test--with-fresh
    (let ((ttl (elot-ttlpx-test--copy-fixture "smallttl.ttl")))
      (elot-db-update-source
       ttl nil "ttl"
       (list (list
              "http://data.dnv.com/ontology/ellsea/asset/id51929cf8-5b83-483d-937a-eeaffbc2a907"
              "thimble B" nil)))
      (elot-db-add-prefix ttl nil "ellsea"
                          "http://data.dnv.com/ontology/ellsea/asset/")
      (with-temp-buffer
        (insert "full http://data.dnv.com/ontology/ellsea/asset/id51929cf8-5b83-483d-937a-eeaffbc2a907 end.\n")
        (setq-local elot-active-label-sources `((,ttl)))
        (elot-global-label-display-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (let* ((pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "http://data.dnv.com/")
                      (match-beginning 0)))
               (label (get-text-property pos 'display)))
          (should (equal label "thimble B")))
        (elot-global-label-display-mode -1)))))

;;; ---------------------------------------------------------------------------
;;; F. Safety-net cap
;;; ---------------------------------------------------------------------------

(ert-deftest test-global-mode-safety-net-cap ()
  "Exceeding `elot-global-label-display-max-ids' installs no matcher."
  (elot-ttlpx-test--with-fresh
    (let ((src "fake-src"))
      ;; 4 IRIs.
      (elot-db-update-source
       src nil "ttl"
       (list (list "http://example.org/a" "A" nil)
             (list "http://example.org/b" "B" nil)
             (list "http://example.org/c" "C" nil)
             (list "http://example.org/d" "D" nil)))
      (with-temp-buffer
        (insert "mention http://example.org/a here.\n")
        (setq-local elot-active-label-sources `((,src)))
        (let ((elot-global-label-display-max-ids 2))
          (elot-global-label-display-mode 1)
          (should (null elot-global--fontify-regexp))
          (should (null elot-global--keywords))
          ;; Mode itself is still on, so toggle UX is consistent.
          (should elot-global-label-display-mode)
          (elot-global-label-display-mode -1))))))

(provide 'elot-ttl-prefix-test)

;;; elot-ttl-prefix-test.el ends here
