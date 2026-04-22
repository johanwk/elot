;;; elot-sources-test.el --- ERT tests for elot-sources.el  -*- lexical-binding: t; -*-

;; Step 1.3 tests: source parsers + dispatcher.  Uses fixtures in
;; test/fixtures/ and never invokes ROBOT or the network.  Live-
;; execution tests for TTL / RQ are gated by ELOT_TEST_NETWORK=1 and
;; are not included here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-sources)

(defconst elot-sources-test--dir
  (file-name-directory
   (or load-file-name buffer-file-name
       (locate-library "elot-sources-test"))))

(defconst elot-sources-test--fixtures
  (expand-file-name "fixtures/" elot-sources-test--dir))

(defun elot-sources-test--fx (name)
  (expand-file-name name elot-sources-test--fixtures))

(defun elot-sources-test--find (id entries)
  "Return the (id label plist) triple for ID in ENTRIES, or nil."
  (cl-find id entries :key #'car :test #'equal))

;;;; -------------------------------------------------------------------
;;;; CSV
;;;; -------------------------------------------------------------------

(ert-deftest test-parse-csv-basic ()
  "3-row fixture -> 3 slurp entries; plist carries extra columns."
  (let* ((entries (elot-source-parse-csv
                   (elot-sources-test--fx "labels.csv"))))
    (should (= 3 (length entries)))
    (let ((w (elot-sources-test--find "ex:Widget" entries)))
      (should w)
      (should (equal "Widget" (nth 1 w)))
      (should (equal "A generic widget used for testing."
                     (plist-get (nth 2 w) "definition" #'equal)))
      (should (equal "Class" (plist-get (nth 2 w) "kind" #'equal))))
    ;; RFC 4180 quoted comma.
    (let ((g (elot-sources-test--find "ex:Gadget" entries)))
      (should g)
      (should (equal "A gadget, subclass of widget."
                     (plist-get (nth 2 g) "definition" #'equal))))))

;;;; -------------------------------------------------------------------
;;;; TSV (UC3 opaque ids)
;;;; -------------------------------------------------------------------

(ert-deftest test-parse-tsv-with-attributes ()
  "TSV with opaque EMP-NNN ids; extra columns become plist entries."
  (let* ((entries (elot-source-parse-tsv
                   (elot-sources-test--fx "labels.tsv"))))
    (should (= 3 (length entries)))
    (let ((alice (elot-sources-test--find "EMP-001" entries)))
      (should alice)
      (should (equal "Alice Anderson" (nth 1 alice)))
      (should (equal "employee" (plist-get (nth 2 alice) "kind" #'equal)))
      (should (equal "Senior engineer in the platform team."
                     (plist-get (nth 2 alice) "definition" #'equal))))))

;;;; -------------------------------------------------------------------
;;;; JSON
;;;; -------------------------------------------------------------------

(ert-deftest test-parse-json-flat-dict ()
  "Flat {id: label} dict produces entries with empty plist."
  (let ((entries (elot-source-parse-json
                  (elot-sources-test--fx "labels-flat.json"))))
    (should (= 3 (length entries)))
    (let ((w (elot-sources-test--find "ex:Widget" entries)))
      (should w)
      (should (equal "Widget" (nth 1 w)))
      (should (null (nth 2 w))))
    (should (elot-sources-test--find "ex:connectsTo" entries))))

(ert-deftest test-parse-json-nested-dict ()
  "Nested {id: {label, ...}} dict: label promoted, others to plist."
  (let* ((entries (elot-source-parse-json
                   (elot-sources-test--fx "labels-nested.json")))
         (w (elot-sources-test--find "ex:Widget" entries)))
    (should (= 3 (length entries)))
    (should w)
    (should (equal "Widget (nested)" (nth 1 w)))
    (should (equal "A generic widget used for testing."
                   (plist-get (nth 2 w) "definition" #'equal)))
    (should (equal "Class" (plist-get (nth 2 w) "kind" #'equal)))
    ;; 'label' key must NOT leak into the plist.
    (should-not (plist-get (nth 2 w) "label" #'equal))))

;;;; -------------------------------------------------------------------
;;;; Org
;;;; -------------------------------------------------------------------

(ert-deftest test-parse-org-minimal ()
  "Tiny hand-written ELOT fixture parses; Widget / Gadget / connectsTo present."
  ;; elot-build-slurp requires elot-tangle; skip if not loadable in batch.
  (unless (require 'elot-tangle nil t)
    (ert-skip "elot-tangle not available"))
  (let* ((entries (elot-source-parse-org
                   (elot-sources-test--fx "minimal-ontology.org")))
         (ids     (mapcar #'car entries)))
    (should (>= (length entries) 3))
    (should (cl-some (lambda (id) (string-match-p "Widget\\'" id)) ids))
    (should (cl-some (lambda (id) (string-match-p "Gadget\\'" id)) ids))
    (should (cl-some (lambda (id) (string-match-p "connectsTo\\'" id)) ids))
    ;; Every entry should have a non-empty label string.
    (dolist (e entries)
      (should (stringp (nth 1 e)))
      (should (> (length (nth 1 e)) 0)))))

;;;; -------------------------------------------------------------------
;;;; Dispatcher
;;;; -------------------------------------------------------------------

(ert-deftest test-parse-dispatcher ()
  "Dispatcher selects the correct parser by extension."
  ;; CSV
  (let ((entries (elot-source-parse
                  (elot-sources-test--fx "labels.csv"))))
    (should (= 3 (length entries)))
    (should (elot-sources-test--find "ex:Widget" entries)))
  ;; TSV
  (let ((entries (elot-source-parse
                  (elot-sources-test--fx "labels.tsv"))))
    (should (elot-sources-test--find "EMP-002" entries)))
  ;; JSON (flat)
  (let ((entries (elot-source-parse
                  (elot-sources-test--fx "labels-flat.json"))))
    (should (elot-sources-test--find "ex:Gadget" entries))))

(ert-deftest test-parse-dispatcher-unknown-extension ()
  "Unknown extension errors rather than silently no-op'ing."
  (let ((tmp (make-temp-file "elot-sources-test-" nil ".xyz")))
    (unwind-protect
        (should-error (elot-source-parse tmp) :type 'user-error)
      (delete-file tmp))))

(ert-deftest test-parse-dispatcher-user-extensible ()
  "Adding an extension to the defcustom routes through the dispatcher."
  (let* ((tmp (make-temp-file "elot-sources-test-" nil ".lbl"))
         (called nil)
         (fn (lambda (file)
               (setq called file)
               (list (list "X-1" "Ex One" nil))))
         (elot-source-supported-extensions
          (cons (cons "lbl" fn) elot-source-supported-extensions)))
    (unwind-protect
        (let ((entries (elot-source-parse tmp)))
          (should (equal called tmp))
          (should (equal entries '(("X-1" "Ex One" nil)))))
      (delete-file tmp))))

;;;; -------------------------------------------------------------------
;;;; .rq cache
;;;; -------------------------------------------------------------------

(ert-deftest test-parse-rq-cache-path-deterministic ()
  "Cache filename is deterministic for a given (query, data-source) pair."
  (let* ((tmproot (make-temp-file "elot-rq-root-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-source--project-root)
                   (lambda (_) tmproot)))
          (let* ((q (elot-sources-test--fx "rq/wd-labels.rq"))
                 (d (elot-sources-test--fx "rq/data.ttl"))
                 (p1 (elot-source-rq-cache-path q d))
                 (p2 (elot-source-rq-cache-path q d))
                 (p3 (elot-source-rq-cache-path
                      q "https://query.wikidata.org/sparql")))
            (should (equal p1 p2))
            (should (not (equal p1 p3)))
            (should (string-suffix-p ".csv" p1))
            (should (string-match-p "/\\.elot-cache/wd-labels\\.[0-9a-f]+\\.csv\\'" p1))))
      (ignore-errors (delete-directory tmproot t)))))

(ert-deftest test-parse-rq-from-cache ()
  "With a fresh pre-baked cache, the parser reads CSV and never invokes ROBOT."
  (let* ((tmproot (make-temp-file "elot-rq-root-" t))
         (q (elot-sources-test--fx "rq/wd-labels.rq"))
         (d (elot-sources-test--fx "rq/data.ttl"))
         (orig (and (fboundp 'elot-robot-execute-query)
                    (symbol-function 'elot-robot-execute-query))))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-source--project-root)
                   (lambda (_) tmproot))
                  ((symbol-function 'elot-robot-execute-query)
                   (lambda (&rest _)
                     (error "ROBOT must not be invoked when cache is fresh"))))
          (let* ((cache (elot-source-rq-cache-path q d))
                 (dir   (file-name-directory cache)))
            (unless (file-directory-p dir) (make-directory dir t))
            (with-temp-file cache
              (insert "id,label\n")
              (insert "ex:Widget,Widget\n")
              (insert "ex:Gadget,Gadget\n"))
            ;; Ensure the cache is newer than both inputs.
            (set-file-times cache (seconds-to-time (+ (float-time) 60)))
            (let ((entries (elot-source-parse-rq q d)))
              (should (= 2 (length entries)))
              (should (elot-sources-test--find "ex:Widget" entries))
              (should (elot-sources-test--find "ex:Gadget" entries)))))
      (ignore-errors (delete-directory tmproot t))
      (when orig (fset 'elot-robot-execute-query orig)))))

(ert-deftest test-parse-rq-empty-result-preserves-cache ()
  "Stale cache + executor returning an empty CSV: old cache is preserved."
  (let* ((tmproot (make-temp-file "elot-rq-root-" t))
         (q (elot-sources-test--fx "rq/wd-labels.rq"))
         (d (elot-sources-test--fx "rq/data.ttl")))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-source--project-root)
                   (lambda (_) tmproot))
                  ((symbol-function 'elot-source--rq-execute)
                   (lambda (_q _d out)
                     (with-temp-file out (insert "id,label\n")))))
          (let* ((cache (elot-source-rq-cache-path q d))
                 (dir   (file-name-directory cache)))
            (unless (file-directory-p dir) (make-directory dir t))
            ;; Write an OLD cache (mtime far in the past).
            (with-temp-file cache
              (insert "id,label\nex:Widget,Widget\n"))
            (set-file-times cache
                            (seconds-to-time (- (float-time) (* 3600 24))))
            (let ((entries (elot-source-parse-rq q d)))
              (should (= 1 (length entries)))
              (should (elot-sources-test--find "ex:Widget" entries)))
            (with-temp-buffer
              (insert-file-contents cache)
              (should (string-match-p "ex:Widget" (buffer-string))))))
      (ignore-errors (delete-directory tmproot t)))))

;;;; -------------------------------------------------------------------
;;;; TTL (live remote -- Tier 2, gated by ELOT_TEST_NETWORK)
;;;; -------------------------------------------------------------------

(ert-deftest test-parse-ttl-gated ()
  "Live TTL parsing via ROBOT is gated by ELOT_TEST_NETWORK=1."
  (unless (getenv "ELOT_TEST_NETWORK")
    (ert-skip "ELOT_TEST_NETWORK not set; skipping live ROBOT test"))
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available"))
  (let ((entries (elot-source-parse-ttl
                  (elot-sources-test--fx "rq/data.ttl"))))
    (should (> (length entries) 0))))

;;;; -------------------------------------------------------------------
;;;; Step 1.3.1 -- Tier 1: ROBOT-local tests (no network)
;;;;
;;;; Gated on (elot-robot-available-p) only.  ROBOT is invoked against
;;;; a local TTL fixture; no SPARQL endpoint is contacted.
;;;; -------------------------------------------------------------------

(defun elot-sources-test--robot-or-skip ()
  "Skip the current test unless ROBOT is available."
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available (set elot-robot-jar-path or install robot)")))

(ert-deftest test-parse-ttl-with-robot-local ()
  "ROBOT queries the local TTL fixture; multi-column CSV -> plist round-trip."
  (elot-sources-test--robot-or-skip)
  (let* ((elot-source-ttl-label-query
          "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?id ?label ?definition WHERE {
  ?id rdfs:label ?label .
  OPTIONAL { ?id skos:definition ?definition . }
}")
         (entries (elot-source-parse-ttl
                   (elot-sources-test--fx "rq/data.ttl")))
         ;; ROBOT emits full IRIs in the CSV.
         (widget (elot-sources-test--find
                  "http://example.org/ex/Widget" entries))
         (gadget (elot-sources-test--find
                  "http://example.org/ex/Gadget" entries)))
    (should (>= (length entries) 3))
    (should widget)
    (should (equal "Widget" (nth 1 widget)))
    (should (equal "A generic widget used for testing."
                   (plist-get (nth 2 widget) "definition" #'equal)))
    (should gadget)
    (should (equal "Gadget" (nth 1 gadget)))))

(ert-deftest test-parse-rq-with-robot-fresh-cache ()
  "End-to-end: no cache -> ROBOT runs -> cache written at deterministic path."
  (elot-sources-test--robot-or-skip)
  (let* ((tmproot (make-temp-file "elot-rq-root-" t))
         (q (elot-sources-test--fx "rq/local-labels.rq"))
         (d (elot-sources-test--fx "rq/data.ttl")))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-source--project-root)
                   (lambda (_) tmproot)))
          (let ((cache (elot-source-rq-cache-path q d)))
            (should-not (file-exists-p cache))
            (let* ((entries (elot-source-parse-rq q d))
                   (widget (elot-sources-test--find
                            "http://example.org/ex/Widget" entries)))
              (should (>= (length entries) 3))
              (should widget)
              (should (equal "Widget" (nth 1 widget)))
              (should (equal "A generic widget used for testing."
                             (plist-get (nth 2 widget) "definition" #'equal)))
              (should (file-exists-p cache))
              (should (string-match-p
                       "/\\.elot-cache/local-labels\\.[0-9a-f]+\\.csv\\'"
                       cache)))))
      (ignore-errors (delete-directory tmproot t)))))

(ert-deftest test-parse-rq-stale-cache-reingests ()
  "Cache older than the data source triggers re-execution; cache mtime advances."
  (elot-sources-test--robot-or-skip)
  (let* ((tmproot (make-temp-file "elot-rq-root-" t))
         (q (elot-sources-test--fx "rq/local-labels.rq"))
         (d (elot-sources-test--fx "rq/data.ttl")))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-source--project-root)
                   (lambda (_) tmproot)))
          (let* ((cache (elot-source-rq-cache-path q d))
                 (dir   (file-name-directory cache)))
            (unless (file-directory-p dir) (make-directory dir t))
            ;; Pre-seed an OLD cache with stale content.
            (with-temp-file cache
              (insert "id,label\nhttp://example.org/ex/Stale,Stale\n"))
            (set-file-times cache
                            (seconds-to-time (- (float-time) (* 3600 24))))
            (let ((old-mtime
                   (float-time
                    (file-attribute-modification-time
                     (file-attributes cache)))))
              (let* ((entries (elot-source-parse-rq q d))
                     (widget (elot-sources-test--find
                              "http://example.org/ex/Widget" entries))
                     (new-mtime
                      (float-time
                       (file-attribute-modification-time
                        (file-attributes cache)))))
                ;; Cache was refreshed.
                (should (> new-mtime old-mtime))
                ;; Stale entry is gone; real data is in.
                (should-not (elot-sources-test--find
                             "http://example.org/ex/Stale" entries))
                (should widget)
                (should (equal "Widget" (nth 1 widget)))))))
      (ignore-errors (delete-directory tmproot t)))))

(provide 'elot-sources-test)

;;; elot-sources-test.el ends here
