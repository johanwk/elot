;;; elot-id-test.el --- Tests for elot-id (Milestone 10)  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-id-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 10 Step 10.5 -- pure tests for the
;; identifier-minting framework plus integration tests for the
;; LLM-facing tools `elot_mint_identifier' and `elot_verify_identifier'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst elot-id-test--repo-root
  (let ((this-file (or load-file-name buffer-file-name
                       (locate-library "elot-id-test"))))
    (unless this-file
      (error "elot-id-test: cannot determine source file location"))
    (file-name-directory
     (directory-file-name
      (file-name-directory (file-truename this-file)))))
  "Absolute path to the ELOT repo root, captured at load time.")

(add-to-list 'load-path (expand-file-name "elot-package" elot-id-test--repo-root))

(require 'elot-id)
(require 'elot-gptel)


;;; ---------------------------------------------------------------------------
;;; Framework: registry and resolution
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-builtin-schemes-registered ()
  "All four built-in schemes are registered after `(require 'elot-id)'."
  (dolist (name '(uuid slug counter acme))
    (let ((scheme (elot-id-scheme-by-name name)))
      (should (elot-id-scheme-p scheme))
      (should (eq (elot-id-scheme-name scheme) name)))))

(ert-deftest elot-id-test-register-replaces ()
  "Re-registering a scheme with the same name replaces the prior entry."
  (let ((original (elot-id-scheme-by-name 'uuid)))
    (unwind-protect
        (progn
          (elot-id-register-scheme
           (make-elot-id-scheme
            :name 'uuid
            :description "stub"
            :mint-fn (lambda (_l _c) "ex:stub")
            :verify-fn (lambda (_c _x) t)
            :prefix "ex"))
          (should (string= (elot-id-mint 'uuid "x") "ex:stub")))
      (elot-id-register-scheme original))))

(ert-deftest elot-id-test-scheme-by-name-string-or-symbol ()
  "Scheme lookup accepts both strings and symbols."
  (should (eq (elot-id-scheme-name (elot-id-scheme-by-name "acme"))
              'acme))
  (should (eq (elot-id-scheme-name (elot-id-scheme-by-name 'acme))
              'acme)))

(ert-deftest elot-id-test-scheme-by-name-unknown ()
  "Unknown scheme name signals `user-error'."
  (should-error (elot-id-scheme-by-name 'no-such-scheme)
                :type 'user-error))

(ert-deftest elot-id-test-mint-empty-label-refused ()
  (should-error (elot-id-mint 'uuid "") :type 'user-error)
  (should-error (elot-id-mint 'uuid nil) :type 'user-error))


;;; ---------------------------------------------------------------------------
;;; uuid scheme
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-uuid-shape ()
  "Mint produces an `ex:UUID' CURIE; verify accepts it and rejects garbage."
  (let ((curie (elot-id-mint 'uuid "anything")))
    (should (string-match-p
             "\\`ex:[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
             curie))
    (should (elot-id-verify 'uuid curie)))
  (should-not (elot-id-verify 'uuid "ex:not-a-uuid"))
  (should-not (elot-id-verify 'uuid "ex:ABCDEF12-3456-7890-ABCD-1234567890AB")))

(ert-deftest elot-id-test-uuid-prefix-override ()
  "CONTEXT `:prefix' overrides the scheme's default prefix."
  (let ((curie (elot-id-mint 'uuid "x" '(:prefix "FOO"))))
    (should (string-prefix-p "FOO:" curie))))


;;; ---------------------------------------------------------------------------
;;; slug scheme
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-slug-basic ()
  (should (string= (elot-id-mint 'slug "Dog") "ex:dog"))
  (should (string= (elot-id-mint 'slug "Hot Pink Snake") "ex:hot-pink-snake"))
  (should (elot-id-verify 'slug "ex:dog"))
  (should (elot-id-verify 'slug "ex:hot-pink-snake"))
  (should-not (elot-id-verify 'slug "ex:Dog"))         ; uppercase
  (should-not (elot-id-verify 'slug "ex:dog-")))       ; trailing hyphen

(ert-deftest elot-id-test-slug-collision ()
  "Collision suffix is added when the slug already exists."
  (should (string= (elot-id-mint 'slug "Dog"
                                 '(:existing-iris ("ex:dog")))
                   "ex:dog-2"))
  (should (string= (elot-id-mint 'slug "Dog"
                                 '(:existing-iris ("ex:dog" "ex:dog-2")))
                   "ex:dog-3")))

(ert-deftest elot-id-test-slug-empty-label-fallback ()
  "All-non-ASCII labels fall back to `x'."
  ;; Use escapes so the source stays ASCII.
  (let* ((label (concat (string #x00e9) (string #x00f8))) ; e-acute o-stroke
         (curie (elot-id-mint 'slug label)))
    (should (string= curie "ex:x"))))


;;; ---------------------------------------------------------------------------
;;; counter scheme
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-counter-from-zero ()
  (should (string= (elot-id-mint 'counter "anything") "ex:000001")))

(ert-deftest elot-id-test-counter-increments ()
  (should (string= (elot-id-mint 'counter "anything"
                                 '(:existing-iris ("ex:000007" "ex:000003")))
                   "ex:000008")))

(ert-deftest elot-id-test-counter-verify ()
  (should (elot-id-verify 'counter "ex:000001"))
  (should (elot-id-verify 'counter "ex:1234567"))   ; >= pad-width is OK
  (should-not (elot-id-verify 'counter "ex:abc"))
  (should-not (elot-id-verify 'counter "ex:42")))   ; under pad-width

(ert-deftest elot-id-test-counter-pad-width-override ()
  "Per-scheme `:pad-width' metadata overrides the global default."
  (let ((original (elot-id-scheme-by-name 'counter)))
    (unwind-protect
        (progn
          (elot-id-register-scheme
           (make-elot-id-scheme
            :name 'counter
            :description "narrow"
            :mint-fn #'elot-id--counter-mint
            :verify-fn #'elot-id--counter-verify
            :prefix "FOO"
            :metadata '(:pad-width 3)))
          (should (string= (elot-id-mint 'counter "x") "FOO:001")))
      (elot-id-register-scheme original))))


;;; ---------------------------------------------------------------------------
;;; acme scheme
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-acme-shape-and-roundtrip ()
  "Default (slugless) mint has the 11-char form; verify accepts it."
  (let* ((curie (elot-id-mint 'acme "Dog" '(:kind class)))
         (local (cdr (elot-id--curie-split curie))))
    (should (string-prefix-p "ex:" curie))
    (should (string-match-p
             "\\`C_[0-9A-HJKMNP-TV-Z]\\{9\\}\\'"
             local))
    (should (elot-id-verify 'acme curie))))

(ert-deftest elot-id-test-acme-slug-enabled ()
  "With :slug enabled in scheme-params the 16-char slug form is produced."
  (let* ((curie (elot-id-mint 'acme "Dog"
                              '(:kind class :scheme-params (:slug "t"))))
         (local (cdr (elot-id--curie-split curie))))
    (should (string-match-p
             "\\`C_[a-z]\\{5\\}[0-9A-HJKMNP-TV-Z]\\{9\\}\\'"
             local))
    (should (elot-id-verify 'acme curie))))

(ert-deftest elot-id-test-acme-type-letter-mapping ()
  "Each registered KIND produces its CARD-X TYPE letter."
  (dolist (case '((class . "C") (annotation-property . "A")
                  (object-property . "R") (data-property . "D")
                  (individual . "X") (datatype . "T")))
    (let* ((curie (elot-id-mint 'acme "x"
                                (list :kind (car case)
                                      :scheme-params '(:slug "t"))))
           (local (cdr (elot-id--curie-split curie))))
      (should (string-prefix-p (cdr case) local)))))

(ert-deftest elot-id-test-acme-checksum-rejects-mutation ()
  "Mutating any non-underscore core character flips the checksum."
  (let* ((curie (elot-id-mint 'acme "Dog"
                              '(:kind class :scheme-params (:slug "t"))))
         (local (cdr (elot-id--curie-split curie)))
         (i 5)            ; first random char (post slug+day)
         (orig (aref local i))
         (alt  (if (= orig ?A) ?B ?A))
         (mutated (concat (substring local 0 i)
                          (string alt)
                          (substring local (1+ i)))))
    (should-not (elot-acme-verify mutated))
    (should-not (elot-id-verify 'acme (concat "ex:" mutated)))))

(ert-deftest elot-id-test-acme-collision-retry ()
  "Adapter retries when the freshly minted CURIE is in `:existing-iris'."
  (let* ((attempts 0)
         (orig (symbol-function 'elot-id--curie-collides-p)))
    (unwind-protect
        (progn
          ;; First call is a collision; subsequent calls succeed.
          (fset 'elot-id--curie-collides-p
                (lambda (_s _c _ctx)
                  (cl-incf attempts)
                  (= attempts 1)))
          (let ((minted (elot-id-mint 'acme "dogxx"
                                      (list :kind 'class
                                            :existing-iris '("ex:fake")))))
            (should (string-prefix-p "ex:" minted))
            (should (>= attempts 2))))
      (fset 'elot-id--curie-collides-p orig))))

(ert-deftest elot-id-test-acme-collision-budget-exhausted ()
  "Persistent collisions exhaust the retry budget with a `user-error'."
  (let ((orig (symbol-function 'elot-id--curie-collides-p)))
    (unwind-protect
        (progn
          (fset 'elot-id--curie-collides-p (lambda (&rest _) t))
          (should-error (elot-id-mint 'acme "x" '(:kind class))
                        :type 'user-error))
      (fset 'elot-id--curie-collides-p orig))))


;;; ---------------------------------------------------------------------------
;;; Spec parsing
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-parse-spec-bare-name ()
  (should (equal (elot-id-parse-spec "counter") '(counter . nil)))
  (should (equal (elot-id-parse-spec 'counter)  '(counter . nil))))

(ert-deftest elot-id-test-parse-spec-positional ()
  (let ((parsed (elot-id-parse-spec "counter GO_0000000")))
    (should (eq (car parsed) 'counter))
    (should (equal (plist-get (cdr parsed) :positional)
                   '("GO_0000000")))))

(ert-deftest elot-id-test-parse-spec-keyword-tokens ()
  (let ((parsed (elot-id-parse-spec "acme slug:t")))
    (should (eq (car parsed) 'acme))
    (should (string= (plist-get (cdr parsed) :slug) "t"))))

(ert-deftest elot-id-test-parse-spec-mixed ()
  (let ((parsed (elot-id-parse-spec "counter GO_0000000 sep:_")))
    (should (eq (car parsed) 'counter))
    (should (equal (plist-get (cdr parsed) :positional) '("GO_0000000")))
    (should (string= (plist-get (cdr parsed) :sep) "_"))))


;;; ---------------------------------------------------------------------------
;;; Counter template
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-counter-template-obo ()
  "Template GO_0000000 produces ex:GO_0000001 / ex:GO_0000002."
  (let* ((params '(:positional ("GO_0000000")))
         (c1 (elot-id-mint 'counter "x"
                           (list :scheme-params params)))
         (c2 (elot-id-mint 'counter "x"
                           (list :scheme-params params
                                 :existing-iris (list c1)))))
    (should (string= c1 "ex:GO_0000001"))
    (should (string= c2 "ex:GO_0000002"))
    (should (elot-id-verify 'counter c1
                            (list :scheme-params params)))
    (should-not (elot-id-verify 'counter "ex:GO_42"
                                (list :scheme-params params)))
    (should-not (elot-id-verify 'counter "ex:0000001"
                                (list :scheme-params params)))))

(ert-deftest elot-id-test-counter-template-no-sep ()
  "Template CHEBI00000 produces ex:CHEBI00001 (no separator)."
  (let* ((params '(:positional ("CHEBI00000")))
         (c (elot-id-mint 'counter "x" (list :scheme-params params))))
    (should (string= c "ex:CHEBI00001"))
    (should (elot-id-verify 'counter c (list :scheme-params params)))))

(ert-deftest elot-id-test-counter-template-via-keyword ()
  "Template may be passed under the explicit :template keyword."
  (let* ((params '(:template "GO_0000000"))
         (c (elot-id-mint 'counter "x" (list :scheme-params params))))
    (should (string= c "ex:GO_0000001"))))

(ert-deftest elot-id-test-counter-template-bad ()
  (should-error
   (elot-id--counter-parse-template "GO_NODIGITS")
   :type 'user-error))


;;; ---------------------------------------------------------------------------
;;; Acme slug toggle
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-acme-default-is-slugless ()
  (let* ((curie (elot-id-mint 'acme "Dog" '(:kind class)))
         (local (cdr (elot-id--curie-split curie))))
    (should (= (length local) 11))
    (should (elot-id-verify 'acme curie))))

(ert-deftest elot-id-test-acme-slug-on-via-string ()
  (let* ((curie (elot-id-mint 'acme "Dog"
                              '(:kind class
                                :scheme-params (:slug "t"))))
         (local (cdr (elot-id--curie-split curie))))
    (should (= (length local) 16))
    (should (elot-id-verify 'acme curie))))

(ert-deftest elot-id-test-acme-slug-explicit-off ()
  (let* ((curie (elot-id-mint 'acme "Dog"
                              '(:kind class
                                :scheme-params (:slug "false"))))
         (local (cdr (elot-id--curie-split curie))))
    (should (= (length local) 11))))


;;; ---------------------------------------------------------------------------
;;; Batch minting
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-test-batch-counter-monotone ()
  "counter batch produces N successive numbers in one call."
  (let ((batch (elot-id-mint-batch
                'counter 3
                '(:scheme-params (:positional ("GO_0000000"))))))
    (should (equal batch
                   '("ex:GO_0000001" "ex:GO_0000002" "ex:GO_0000003")))))

(ert-deftest elot-id-test-batch-counter-seeded-from-existing ()
  "Batch seeds from CONTEXT :existing-iris."
  (let ((batch (elot-id-mint-batch
                'counter 2
                '(:scheme-params (:positional ("GO_0000000"))
                  :existing-iris ("ex:GO_0000005")))))
    (should (equal batch '("ex:GO_0000006" "ex:GO_0000007")))))

(ert-deftest elot-id-test-batch-slug-disambiguates-within-batch ()
  "Two slug mints for the same label disambiguate against each other."
  (let ((batch (elot-id-mint-batch 'slug 3 nil
                                   :labels '("Dog" "Dog" "Dog"))))
    (should (equal batch '("ex:dog" "ex:dog-2" "ex:dog-3")))))

(ert-deftest elot-id-test-batch-uuid-unique ()
  "uuid batch returns N distinct identifiers."
  (let ((batch (elot-id-mint-batch 'uuid 5)))
    (should (= (length batch) 5))
    (should (= (length (delete-dups (copy-sequence batch))) 5))
    (dolist (c batch)
      (should (elot-id-verify 'uuid c)))))

(ert-deftest elot-id-test-batch-acme-unique-within-batch ()
  "acme batch returns N distinct identifiers, all verifying."
  (let ((batch (elot-id-mint-batch 'acme 4 '(:kind class))))
    (should (= (length batch) 4))
    (should (= (length (delete-dups (copy-sequence batch))) 4))
    (dolist (c batch)
      (should (elot-id-verify 'acme c)))))

(ert-deftest elot-id-test-batch-labels-length-mismatch ()
  (should-error (elot-id-mint-batch 'slug 3 nil :labels '("a" "b"))
                :type 'user-error))

(ert-deftest elot-id-test-batch-bad-size ()
  (should-error (elot-id-mint-batch 'uuid 0) :type 'user-error)
  (should-error (elot-id-mint-batch 'uuid -1) :type 'user-error))

(ert-deftest elot-id-test-batch-placeholder-labels-by-kind ()
  "When LABELS is omitted, placeholders are synthesised from :kind."
  (let ((batch (elot-id-mint-batch 'slug 2 '(:kind class))))
    (should (equal batch '("ex:new-class-1" "ex:new-class-2")))))

(ert-deftest elot-id-test-batch-mint-batch-fn-override ()
  "A scheme's `mint-batch-fn' slot is honoured when set."
  (let ((original (elot-id-scheme-by-name 'uuid)))
    (unwind-protect
        (progn
          (elot-id-register-scheme
           (make-elot-id-scheme
            :name 'uuid
            :description "stub"
            :mint-fn (lambda (_l _c) "ex:single")
            :verify-fn (lambda (_c _x) t)
            :mint-batch-fn (lambda (n _ctx _labels)
                             (cl-loop for i from 1 to n
                                      collect (format "ex:batch-%d" i)))
            :prefix "ex"))
          (should (equal (elot-id-mint-batch 'uuid 3)
                         '("ex:batch-1" "ex:batch-2" "ex:batch-3"))))
      (elot-id-register-scheme original))))

(ert-deftest elot-id-test-batch-mint-batch-fn-bad-count ()
  "Framework validates the override's result length."
  (let ((original (elot-id-scheme-by-name 'uuid)))
    (unwind-protect
        (progn
          (elot-id-register-scheme
           (make-elot-id-scheme
            :name 'uuid
            :description "bad"
            :mint-fn (lambda (_l _c) "ex:x")
            :verify-fn (lambda (_c _x) t)
            :mint-batch-fn (lambda (_n _ctx _labels) '("ex:only-one"))
            :prefix "ex"))
          (should-error (elot-id-mint-batch 'uuid 3) :type 'user-error))
      (elot-id-register-scheme original))))


;;; ---------------------------------------------------------------------------
;;; LLM-facing tools: elot_mint_identifier / elot_verify_identifier
;;; ---------------------------------------------------------------------------

(defun elot-id-test--with-pets-file (fn)
  "Call FN with the workspace-relative path to a pets-like fixture."
  (let ((default-directory elot-id-test--repo-root))
    (funcall fn "examples/pets.org")))

(ert-deftest elot-id-test-tool-mint-each-scheme ()
  "`elot_mint_identifier' round-trips for all four built-in schemes."
  (elot-id-test--with-pets-file
   (lambda (file)
     (dolist (scheme '("uuid" "slug" "counter" "acme"))
       (let ((out (elot-gptel-tool-mint-identifier
                   file "Snake" scheme "class")))
         (should (string-prefix-p "OK: minted " out))
         (should (string-match-p (format "scheme=%s" scheme) out))
         ;; Extract the CURIE and verify under the same scheme.
         (should (string-match "OK: minted \\([^ ]+\\)" out))
         (let ((curie (match-string 1 out)))
           (should (string-match-p "\\`ex:" curie))
           (let ((vout (elot-gptel-tool-verify-identifier
                        file curie scheme)))
             (should (string-prefix-p "OK:" vout)))))))))

(ert-deftest elot-id-test-tool-mint-default-scheme ()
  "Omitting the scheme uses `elot-id-default-scheme'."
  (elot-id-test--with-pets-file
   (lambda (file)
     (let ((out (elot-gptel-tool-mint-identifier file "Snake")))
       (should (string-prefix-p "OK: minted " out))
       (should (string-match-p
                (format "scheme=%s" elot-id-default-scheme)
                out))))))

(ert-deftest elot-id-test-tool-mint-empty-label ()
  (elot-id-test--with-pets-file
   (lambda (file)
     (let ((out (elot-gptel-tool-mint-identifier file "" "uuid")))
       (should (string-prefix-p "ERROR:" out))))))

(ert-deftest elot-id-test-tool-mint-unknown-scheme ()
  (elot-id-test--with-pets-file
   (lambda (file)
     (let ((out (elot-gptel-tool-mint-identifier
                 file "Snake" "no-such-scheme")))
       (should (string-prefix-p "ERROR:" out))))))

(ert-deftest elot-id-test-tool-mint-unknown-kind ()
  (elot-id-test--with-pets-file
   (lambda (file)
     (let ((out (elot-gptel-tool-mint-identifier
                 file "Snake" "acme" "bogus-kind")))
       (should (string-prefix-p "ERROR:" out))))))

(ert-deftest elot-id-test-tool-verify-fail ()
  (elot-id-test--with-pets-file
   (lambda (file)
     (let ((out (elot-gptel-tool-verify-identifier
                 file "ex:not-a-uuid" "uuid")))
       (should (string-prefix-p "FAIL:" out))))))

(ert-deftest elot-id-test-tool-verify-empty-curie ()
  (elot-id-test--with-pets-file
   (lambda (file)
     (let ((out (elot-gptel-tool-verify-identifier file "" "uuid")))
       (should (string-prefix-p "ERROR:" out))))))

(ert-deftest elot-id-test-tool-specs-registered ()
  "Both M10 tools appear in `elot-gptel--tool-specs'."
  (dolist (name '("elot_mint_identifier" "elot_verify_identifier"))
    (should (assoc name elot-gptel--tool-specs))))

(ert-deftest elot-id-test-tool-mint-pets-context ()
  "The pets.org fixture seeds the context with prefix `ex' and
collected existing identifiers, so the slug scheme avoids them."
  (elot-id-test--with-pets-file
   (lambda (file)
     (let* ((out1 (elot-gptel-tool-mint-identifier
                   file "dog" "slug" "class"))
            (curie1 (and (string-match "OK: minted \\([^ ]+\\)" out1)
                         (match-string 1 out1))))
       (should curie1)
       ;; pets.org declares ex:dog already, so the mint should
       ;; produce a disambiguated form, not ex:dog itself.
       (should-not (string= curie1 "ex:dog"))))))

(ert-deftest elot-id-test-tool-mint-counter-spec ()
  "Tool accepts a `counter GO_0000000' spec string."
  (elot-id-test--with-pets-file
   (lambda (file)
     (let* ((out (elot-gptel-tool-mint-identifier
                  file "Snake" "counter GO_0000000" "class"))
            (curie (and (string-match "OK: minted \\([^ ]+\\)" out)
                        (match-string 1 out))))
       (should curie)
       (should (string-match-p "\\`ex:GO_[0-9]\\{7\\}\\'" curie))
       (let ((vout (elot-gptel-tool-verify-identifier
                    file curie "counter GO_0000000")))
         (should (string-prefix-p "OK:" vout)))))))

(ert-deftest elot-id-test-tool-mint-acme-with-slug ()
  "`acme slug:t' produces the 16-char slug form via the tool."
  (elot-id-test--with-pets-file
   (lambda (file)
     (let* ((out (elot-gptel-tool-mint-identifier
                  file "Snake" "acme slug:t" "class"))
            (curie (and (string-match "OK: minted \\([^ ]+\\)" out)
                        (match-string 1 out))))
       (should curie)
       (should (string-match-p
                "\\`ex:C_[a-z]\\{5\\}[0-9A-HJKMNP-TV-Z]\\{9\\}\\'"
                curie))))))

(ert-deftest elot-id-test-tool-mint-acme-default-slugless ()
  "Bare `acme' spec produces the 11-char slugless form."
  (elot-id-test--with-pets-file
   (lambda (file)
     (let* ((out (elot-gptel-tool-mint-identifier
                  file "Snake" "acme" "class"))
            (curie (and (string-match "OK: minted \\([^ ]+\\)" out)
                        (match-string 1 out))))
       (should curie)
       (should (string-match-p
                "\\`ex:C_[0-9A-HJKMNP-TV-Z]\\{9\\}\\'"
                curie))))))

(provide 'elot-id-test)
;;; elot-id-test.el ends here
