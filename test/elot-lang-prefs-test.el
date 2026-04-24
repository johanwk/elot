;;; elot-lang-prefs-test.el --- Step 1.16.1 tests: language-picker helper -*- lexical-binding: t; -*-

;; Unit tests for the pure language-picker helper introduced in
;; Step 1.16.1 -- `elot-db--select-by-language' and
;; `elot-db--effective-language-prefs', together with the
;; `elot-preferred-languages' defcustom and its safe-local-variable
;; predicate.  No DB fixture is needed; these are pure Elisp tests.

;;; Code:

(require 'ert)
(require 'elot-db)

;; Test 1: untagged wins by default.
(ert-deftest test-lang-select-untagged-wins-by-default ()
  "With no user prefs, an untagged row beats language-tagged rows."
  (let ((elot-preferred-languages nil))
    (should (equal (elot-db--select-by-language
                    '(("hello" . "en") ("hallo" . "de") ("plain" . nil)))
                   '("plain" . nil)))
    ;; Empty string lang counts as untagged.
    (should (equal (elot-db--select-by-language
                    '(("hello" . "en") ("plain" . "")))
                   '("plain" . "")))))

;; Test 2: @en is the default second choice.
(ert-deftest test-lang-select-en-is-default-second ()
  "Without user prefs and without an untagged row, @en wins."
  (let ((elot-preferred-languages nil))
    (should (equal (elot-db--select-by-language
                    '(("hallo" . "de") ("bonjour" . "fr") ("hello" . "en")))
                   '("hello" . "en")))
    ;; Case-insensitive match.
    (should (equal (elot-db--select-by-language
                    '(("hallo" . "de") ("hello" . "EN")))
                   '("hello" . "EN")))))

;; Test 3: alphabetical fallback when neither untagged nor @en is present.
(ert-deftest test-lang-select-alphabetical-fallback ()
  "When neither untagged nor @en is available, the alphabetically first tag wins."
  (let ((elot-preferred-languages nil))
    (should (equal (elot-db--select-by-language
                    '(("annyeong" . "ko") ("hallo" . "de") ("bonjour" . "fr")))
                   '("hallo" . "de")))))

;; Test 4: user-supplied PREFS argument overrides the defcustom.
(ert-deftest test-lang-select-user-prefs-win ()
  "An explicit PREFS list beats the default policy."
  (let ((elot-preferred-languages nil)
        (rows '(("hallo" . "de") ("hello" . "en") ("plain" . nil)
                ("annyeong" . "ko"))))
    (should (equal (elot-db--select-by-language rows '("ko"))
                   '("annyeong" . "ko")))
    (should (equal (elot-db--select-by-language rows '("de" "en"))
                   '("hallo" . "de")))
    ;; When PREFS is nil, `elot-preferred-languages' is consulted.
    (let ((elot-preferred-languages '("ko")))
      (should (equal (elot-db--select-by-language rows)
                     '("annyeong" . "ko"))))))

;; Test 5: :untagged sentinel at a specific position.
(ert-deftest test-lang-select-untagged-sentinel ()
  "`:untagged' in the prefs list pins untagged literals at that rank."
  (let ((rows '(("hallo" . "de") ("hello" . "en") ("plain" . nil))))
    ;; Korean first, untagged second -- fallback to untagged.
    (should (equal (elot-db--select-by-language rows '("ko" :untagged))
                   '("plain" . nil)))
    ;; Without :untagged, the helper appends it implicitly; untagged
    ;; still loses to an earlier explicit match.
    (should (equal (elot-db--select-by-language rows '("en"))
                   '("hello" . "en")))
    ;; User pins untagged after English explicitly.
    (should (equal (elot-db--select-by-language rows '("en" :untagged "de"))
                   '("hello" . "en")))
    ;; User pins untagged last: en wins before reaching untagged.
    (should (equal (elot-db--select-by-language
                    '(("plain" . nil) ("hello" . "en"))
                    '("en" :untagged))
                   '("hello" . "en")))))

;; Bonus: effective-prefs normalisation.
(ert-deftest test-lang-effective-prefs-normalisation ()
  "`elot-db--effective-language-prefs' applies the documented defaults."
  (let ((elot-preferred-languages nil))
    (should (equal (elot-db--effective-language-prefs) '(:untagged "en"))))
  (let ((elot-preferred-languages '("ko")))
    (should (equal (elot-db--effective-language-prefs) '("ko" :untagged))))
  (let ((elot-preferred-languages '(:untagged "en")))
    (should (equal (elot-db--effective-language-prefs) '(:untagged "en"))))
  ;; Explicit PREFS argument wins over the defcustom.
  (let ((elot-preferred-languages '("de")))
    (should (equal (elot-db--effective-language-prefs '("fr"))
                   '("fr" :untagged)))))

;; Bonus: singleton / empty input.
(ert-deftest test-lang-select-singleton-and-empty ()
  "Empty input returns nil; singleton returns its element verbatim."
  (should (null (elot-db--select-by-language nil)))
  (should (equal (elot-db--select-by-language '(("only" . "xx")))
                 '("only" . "xx")))
  (should (equal (elot-db--select-by-language '(("only" . nil)))
                 '("only" . nil))))

;; Bonus: safe-local-variable predicate.
(ert-deftest test-lang-preferred-languages-safe-local ()
  "The `safe-local-variable' predicate accepts well-formed values
and rejects malformed ones."
  (let ((pred (get 'elot-preferred-languages 'safe-local-variable)))
    (should (functionp pred))
    (should (funcall pred nil))
    (should (funcall pred '("en")))
    (should (funcall pred '("ko" :untagged "en")))
    (should (funcall pred '(:untagged)))
    (should-not (funcall pred '(42)))
    (should-not (funcall pred 'not-a-list))
    (should-not (funcall pred '("en" bogus)))))

(provide 'elot-lang-prefs-test)
;;; elot-lang-prefs-test.el ends here
