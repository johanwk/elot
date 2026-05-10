;;; elot-sparql-exec-helpers-test.el --- Tests for Step 1.4 helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.4:
;; Pure-helper tests for the failure-mode helpers extracted from
;; `elot--custom-org-babel-execute-sparql':
;;
;;   - `elot--sparql-resolve-format'  (format normalisation)
;;   - `elot--sparql-classify-url'    (:url validation)
;;   - `elot--sparql-result-empty-p'  (empty-result detection)
;;
;; These tests do not invoke ROBOT and do not stand up an HTTP
;; endpoint.  An end-to-end test that actually runs ROBOT lives
;; elsewhere and is skipped on machines without ROBOT.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot)

(defmacro elot-spx-helpers-test--capture-warnings (&rest body)
  "Run BODY capturing any `display-warning' calls.
Return a list of (TYPE MESSAGE LEVEL) triples in call order."
  (declare (indent 0))
  `(let ((elot-spx-helpers-test--warnings nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (type message &optional level &rest _)
                  (push (list type message level)
                        elot-spx-helpers-test--warnings))))
       ,@body)
     (nreverse elot-spx-helpers-test--warnings)))


;;;; elot--sparql-resolve-format

(ert-deftest elot-sparql-resolve-format/nil-defaults-to-csv ()
  (let ((warnings (elot-spx-helpers-test--capture-warnings
                    (should (eq 'csv (elot--sparql-resolve-format nil))))))
    (should (null warnings))))

(ert-deftest elot-sparql-resolve-format/empty-string-defaults-to-csv ()
  (let ((warnings (elot-spx-helpers-test--capture-warnings
                    (should (eq 'csv (elot--sparql-resolve-format ""))))))
    (should (null warnings))))

(ert-deftest elot-sparql-resolve-format/ttl-and-turtle ()
  (should (eq 'ttl (elot--sparql-resolve-format "ttl")))
  (should (eq 'ttl (elot--sparql-resolve-format "turtle")))
  (should (eq 'ttl (elot--sparql-resolve-format "text/turtle"))))

(ert-deftest elot-sparql-resolve-format/csv-variants ()
  (should (eq 'csv (elot--sparql-resolve-format "csv")))
  (should (eq 'csv (elot--sparql-resolve-format "text/csv"))))

(ert-deftest elot-sparql-resolve-format/unknown-warns-and-defaults ()
  (let* ((result nil)
         (warnings (elot-spx-helpers-test--capture-warnings
                     (setq result (elot--sparql-resolve-format "json")))))
    (should (eq 'csv result))
    (should (= 1 (length warnings)))
    (should (eq 'elot-sparql (nth 0 (car warnings))))))


;;;; elot--sparql-classify-url

(ert-deftest elot-sparql-classify-url/missing-url-errors ()
  (should-error (elot--sparql-classify-url nil)    :type 'user-error)
  (should-error (elot--sparql-classify-url "")     :type 'user-error))

(ert-deftest elot-sparql-classify-url/non-string-errors ()
  (should-error (elot--sparql-classify-url 'foo)   :type 'user-error)
  (should-error (elot--sparql-classify-url 42)     :type 'user-error))

(ert-deftest elot-sparql-classify-url/http-and-https ()
  (should (eq 'endpoint
              (elot--sparql-classify-url "http://example.org/sparql")))
  (should (eq 'endpoint
              (elot--sparql-classify-url "https://example.org/sparql"))))

(ert-deftest elot-sparql-classify-url/local-file-must-exist ()
  (let ((tmp (make-temp-file "elot-classify-url-" nil ".omn")))
    (unwind-protect
        (should (eq 'local-file (elot--sparql-classify-url tmp)))
      (ignore-errors (delete-file tmp)))))

(ert-deftest elot-sparql-classify-url/missing-local-file-errors ()
  (let* ((missing (expand-file-name
                   (format "definitely-not-here-%d.omn"
                           (random 1000000))
                   temporary-file-directory)))
    (should-not (file-exists-p missing))
    (let ((err (should-error (elot--sparql-classify-url missing)
                             :type 'user-error)))
      ;; Resolved absolute path must appear in the message.
      (should (string-match-p (regexp-quote missing)
                              (error-message-string err))))))


;;;; elot--sparql-result-empty-p

(ert-deftest elot-sparql-result-empty-p/totally-empty ()
  (with-temp-buffer
    (should (elot--sparql-result-empty-p 'csv))
    (should (elot--sparql-result-empty-p 'ttl))))

(ert-deftest elot-sparql-result-empty-p/whitespace-only ()
  (with-temp-buffer
    (insert "  \n\t\n")
    (should (elot--sparql-result-empty-p 'csv))
    (should (elot--sparql-result-empty-p 'ttl))))

(ert-deftest elot-sparql-result-empty-p/csv-header-only ()
  (with-temp-buffer
    (insert "s,p,o\n")
    (should (elot--sparql-result-empty-p 'csv))
    ;; In ttl mode, the same content is non-empty.
    (should-not (elot--sparql-result-empty-p 'ttl))))

(ert-deftest elot-sparql-result-empty-p/csv-with-rows ()
  (with-temp-buffer
    (insert "s,p,o\nhttp://a,http://b,http://c\n")
    (should-not (elot--sparql-result-empty-p 'csv))))

(ert-deftest elot-sparql-result-empty-p/ttl-with-content ()
  (with-temp-buffer
    (insert "@prefix ex: <http://example.org/> .\nex:a ex:b ex:c .\n")
    (should-not (elot--sparql-result-empty-p 'ttl))))


(provide 'elot-sparql-exec-helpers-test)
;;; elot-sparql-exec-helpers-test.el ends here
