;;; peggy-to-peg.el --- Convert Peggy grammar to peg.el S-expressions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A converter that reads a subset of Peggy/PEG.js grammar files and
;; emits an equivalent `peg-define-grammar' form for Emacs' peg.el.
;;
;; Supported Peggy constructs (the subset used by owl-manchester.peggy):
;;   - Named rules:        RuleName = Expression
;;   - Ordered choice:     A / B / C
;;   - Sequence:           A B C
;;   - Zero-or-more:       E*
;;   - One-or-more:        E+
;;   - Optional:           E?
;;   - Negative lookahead: !E
;;   - Grouping:           ( ... )
;;   - String literals:    "foo" and 'foo'
;;   - Character classes:  [a-zA-Z0-9_-]  [^>]  [ \t\n\r]
;;   - Rule references:    RuleName
;;   - Comments:           // ...
;;   - Display names:      RuleName "display name"  (ignored)
;;
;; Usage:
;;   (require 'peggy-to-peg)
;;   (peggy-to-peg-convert-file "syntax/owl-manchester.peggy"
;;                              "syntax/elot-owl-grammar.el")
;;
;; Or from the command line:
;;   emacs --batch -l syntax/peggy-to-peg.el \
;;     --eval '(peggy-to-peg-convert-file "syntax/owl-manchester.peggy" \
;;                                         "syntax/elot-owl-grammar.el")'

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Tokenizer
;; ---------------------------------------------------------------------------

(cl-defstruct (peggy--tok (:constructor peggy--tok-make))
  type   ; symbol: string, class, ident, slash, star, plus, qmark,
         ;         lparen, rparen, lbrace, rbrace, eq, bang, eof
  value) ; string payload

(defvar peggy--input nil "Input string being tokenized.")
(defvar peggy--pos   0   "Current position in `peggy--input'.")

(defun peggy--peek-char ()
  "Return current char or nil at end."
  (and (< peggy--pos (length peggy--input))
       (aref peggy--input peggy--pos)))

(defun peggy--advance ()
  "Advance position by 1 and return the consumed char."
  (prog1 (peggy--peek-char)
    (cl-incf peggy--pos)))

(defun peggy--skip-ws-and-comments ()
  "Skip whitespace and // comments."
  (let ((moved t))
    (while moved
      (setq moved nil)
      ;; whitespace
      (while (and (peggy--peek-char)
                  (memq (peggy--peek-char) '(?\s ?\t ?\n ?\r)))
        (peggy--advance)
        (setq moved t))
      ;; line comment
      (when (and (< (1+ peggy--pos) (length peggy--input))
                 (= (aref peggy--input peggy--pos) ?/)
                 (= (aref peggy--input (1+ peggy--pos)) ?/))
        (while (and (peggy--peek-char)
                    (/= (peggy--peek-char) ?\n))
          (peggy--advance))
        (setq moved t)))))

(defun peggy--read-string-with-quote (quote-char)
  "Read a string literal delimited by QUOTE-CHAR (pos on opening quote)."
  (peggy--advance) ; skip opening quote
  (let ((start peggy--pos))
    (while (and (peggy--peek-char)
                (/= (peggy--peek-char) quote-char))
      (when (= (peggy--peek-char) ?\\)
        (peggy--advance)) ; skip escaped char
      (peggy--advance))
    (let ((val (substring peggy--input start peggy--pos)))
      (peggy--advance) ; skip closing quote
      (peggy--tok-make :type 'string :value val))))

(defun peggy--read-char-class ()
  "Read a character class [...] (pos on opening bracket)."
  (peggy--advance) ; skip [
  (let ((start peggy--pos))
    ;; handle ] as first char or ^] as first two chars
    (when (and (peggy--peek-char) (= (peggy--peek-char) ?^))
      (peggy--advance))
    (when (and (peggy--peek-char) (= (peggy--peek-char) ?\]))
      (peggy--advance))
    (while (and (peggy--peek-char)
                (/= (peggy--peek-char) ?\]))
      (peggy--advance))
    (let ((val (substring peggy--input start peggy--pos)))
      (peggy--advance) ; skip ]
      (peggy--tok-make :type 'class :value val))))

(defun peggy--read-ident ()
  "Read an identifier [a-zA-Z_][a-zA-Z0-9_]*."
  (let ((start peggy--pos))
    (while (and (peggy--peek-char)
                (let ((c (peggy--peek-char)))
                  (or (<= ?a c ?z) (<= ?A c ?Z) (<= ?0 c ?9) (= c ?_))))
      (peggy--advance))
    (substring peggy--input start peggy--pos)))

(defun peggy--next-token ()
  "Return the next token."
  (peggy--skip-ws-and-comments)
  (let ((c (peggy--peek-char)))
    (cond
     ((null c)  (peggy--tok-make :type 'eof :value nil))
     ((= c ?\") (peggy--read-string-with-quote ?\"))
     ((= c ?')  (peggy--read-string-with-quote ?'))
     ((= c ?\[) (peggy--read-char-class))
     ((= c ?/)  (peggy--advance) (peggy--tok-make :type 'slash :value "/"))
     ((= c ?*)  (peggy--advance) (peggy--tok-make :type 'star  :value "*"))
     ((= c ?+)  (peggy--advance) (peggy--tok-make :type 'plus  :value "+"))
     ((= c ??)  (peggy--advance) (peggy--tok-make :type 'qmark :value "?"))
     ((= c ?\() (peggy--advance) (peggy--tok-make :type 'lparen :value "("))
     ((= c ?\)) (peggy--advance) (peggy--tok-make :type 'rparen :value ")"))
     ((= c ?{)  (peggy--advance) (peggy--tok-make :type 'lbrace :value "{"))
     ((= c ?})  (peggy--advance) (peggy--tok-make :type 'rbrace :value "}"))
     ((= c ?=)  (peggy--advance) (peggy--tok-make :type 'eq :value "="))
     ((= c ?!)  (peggy--advance) (peggy--tok-make :type 'bang :value "!"))
     ((or (<= ?a c ?z) (<= ?A c ?Z) (= c ?_))
      (let ((id (peggy--read-ident)))
        (peggy--tok-make :type 'ident :value id)))
     (t (error "peggy--next-token: unexpected char '%c' at pos %d" c peggy--pos)))))

;; Token stream with one-token lookahead
(defvar peggy--current-token nil)

(defun peggy--init-tokenizer (input)
  "Initialize the tokenizer with INPUT string."
  (setq peggy--input input
        peggy--pos 0
        peggy--current-token (peggy--next-token)))

(defun peggy--cur ()
  "Return current token."
  peggy--current-token)

(defun peggy--eat (type)
  "Consume token of TYPE and return it; error otherwise."
  (let ((tok peggy--current-token))
    (unless (eq (peggy--tok-type tok) type)
      (error "Expected token %s but got %s (%s) at pos %d"
             type (peggy--tok-type tok) (peggy--tok-value tok) peggy--pos))
    (setq peggy--current-token (peggy--next-token))
    tok))

(defun peggy--at (type)
  "Return non-nil if current token is TYPE."
  (eq (peggy--tok-type peggy--current-token) type))

;; ---------------------------------------------------------------------------
;; Parser – Peggy grammar → intermediate AST
;; ---------------------------------------------------------------------------
;;
;; AST node types (as lists):
;;   (rule NAME expr)
;;   (choice EXPRS...)
;;   (seq EXPRS...)
;;   (star EXPR)
;;   (plus EXPR)
;;   (opt EXPR)
;;   (not EXPR)
;;   (str "value")
;;   (class "spec")
;;   (ref NAME)

(defun peggy--parse-grammar ()
  "Parse entire grammar, return list of rule nodes."
  (let (rules)
    (while (not (peggy--at 'eof))
      (push (peggy--parse-rule) rules))
    (nreverse rules)))

(defun peggy--parse-rule ()
  "Parse one rule: Ident (display-name)? '=' Expression."
  (let ((name (peggy--tok-value (peggy--eat 'ident))))
    ;; skip optional display name string
    (when (peggy--at 'string)
      (peggy--eat 'string))
    (peggy--eat 'eq)
    (let ((expr (peggy--parse-choice)))
      `(rule ,name ,expr))))

(defun peggy--parse-choice ()
  "Parse: Sequence ( '/' Sequence )*."
  (let ((first (peggy--parse-sequence))
        alts)
    (while (peggy--at 'slash)
      (peggy--eat 'slash)
      (push (peggy--parse-sequence) alts))
    (if alts
        `(choice ,first ,@(nreverse alts))
      first)))

(defun peggy--looking-at-rule-start-p ()
  "Return non-nil if the current token starts a new rule definition.
A rule starts with `Ident =` or `Ident \"display-name\" =`.
We peek ahead in the token stream without consuming."
  (when (peggy--at 'ident)
    ;; Save state
    (let ((saved-pos peggy--pos)
          (saved-tok peggy--current-token))
      (peggy--eat 'ident)
      (let ((is-rule (or (peggy--at 'eq)
                         (and (peggy--at 'string)
                              (let ((saved-pos2 peggy--pos)
                                    (saved-tok2 peggy--current-token))
                                (peggy--eat 'string)
                                (let ((result (peggy--at 'eq)))
                                  ;; Restore inner state
                                  (setq peggy--pos saved-pos2
                                        peggy--current-token saved-tok2)
                                  result))))))
        ;; Restore state
        (setq peggy--pos saved-pos
              peggy--current-token saved-tok)
        is-rule))))

(defun peggy--parse-sequence ()
  "Parse a sequence of postfix expressions."
  (let (items)
    (while (and (memq (peggy--tok-type peggy--current-token)
                      '(string class ident lparen lbrace rbrace bang))
                ;; Don't consume an ident that starts a new rule
                (not (peggy--looking-at-rule-start-p)))
      (push (peggy--parse-postfix) items))
    (if (cdr items)
        `(seq ,@(nreverse items))
      (car (nreverse items)))))

(defun peggy--parse-postfix ()
  "Parse: Atom ('*' | '+' | '?')?."
  (let ((atom (peggy--parse-atom)))
    (cond
     ((peggy--at 'star)  (peggy--eat 'star)  `(star ,atom))
     ((peggy--at 'plus)  (peggy--eat 'plus)  `(plus ,atom))
     ((peggy--at 'qmark) (peggy--eat 'qmark) `(opt  ,atom))
     (t atom))))

(defun peggy--parse-atom ()
  "Parse an atomic expression."
  (cond
   ((peggy--at 'bang)
    (peggy--eat 'bang)
    (let ((inner (peggy--parse-atom)))
      `(not ,inner)))
   ((peggy--at 'string)
    (let ((v (peggy--tok-value (peggy--eat 'string))))
      `(str ,v)))
   ((peggy--at 'class)
    (let ((v (peggy--tok-value (peggy--eat 'class))))
      `(class ,v)))
   ((peggy--at 'ident)
    (let* ((id (peggy--tok-value (peggy--eat 'ident))))
      `(ref ,id)))
   ((peggy--at 'lparen)
    (peggy--eat 'lparen)
    (let ((expr (peggy--parse-choice)))
      (peggy--eat 'rparen)
      expr))
   ((peggy--at 'lbrace)
    (peggy--eat 'lbrace)
    `(str "{"))
   ((peggy--at 'rbrace)
    (peggy--eat 'rbrace)
    `(str "}"))
   (t (error "Unexpected token %s at pos %d"
             (peggy--tok-type peggy--current-token) peggy--pos))))

;; ---------------------------------------------------------------------------
;; Code generator – AST → peg.el S-expressions
;; ---------------------------------------------------------------------------

(defun peggy--rulename-to-symbol (name)
  "Convert PascalCase/camelCase NAME to kebab-case symbol.
Handles transitions like:
  ClassExpression    → class-expression
  ClassIRI           → class-iri
  ObjectPropertyIRI  → object-property-iri
  WS                 → ws
  FullIRI            → full-iri
  PrefixedName       → prefixed-name
  DataRange          → data-range"
  (let ((case-fold-search nil))
    (intern
     (downcase
      ;; Four-pass conversion, case-sensitive (letting case-fold-search = nil):
      ;; Pass 1: Insert hyphen between letter and digit: Expression2 → Expression-2
      ;; Pass 2: Insert hyphen between digit and letter: 2List → 2-List
      ;; Pass 3: Insert hyphen between lowercase/digit and uppercase: fooBar → foo-Bar
      ;; Pass 4: Insert hyphen between uppercase run and uppercase+lowercase: XMLParser → XML-Parser
      (replace-regexp-in-string
       "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1-\\2"
       (replace-regexp-in-string
        "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1-\\2"
        (replace-regexp-in-string
         "\\([0-9]\\)\\([a-zA-Z]\\)" "\\1-\\2"
         (replace-regexp-in-string
          "\\([a-zA-Z]\\)\\([0-9]\\)" "\\1-\\2"
          name nil nil)
         nil nil)
        nil nil)
       nil nil)))))

(defun peggy--format-char (c)
  "Return a printed representation of character C for peg.el output.
Uses Elisp ?-syntax character literals."
  (cond
   ((= c ?\s) "?\\s")
   ((= c ?\t) "?\\t")
   ((= c ?\n) "?\\n")
   ((= c ?\r) "?\\r")
   ((= c ?\") "?\\\"")
   ((= c ?\\) "?\\\\")
   ((= c ?\() "?\\(")
   ((= c ?\)) "?\\)")
   (t (format "?%c" c))))

(defun peggy--char-class-resolve-char (spec pos)
  "Resolve one character from SPEC at POS, handling backslash escapes.
Return (CHAR . NEW-POS)."
  (let ((c (aref spec pos)))
    (if (and (= c ?\\) (< (1+ pos) (length spec)))
        (let ((next (aref spec (1+ pos))))
          (cons (pcase next
                  (?t ?\t)
                  (?n ?\n)
                  (?r ?\r)
                  (?s ?\s)
                  (_  next))
                (+ pos 2)))
      (cons c (1+ pos)))))

(defun peggy--emit-char-class (spec)
  "Convert a character class SPEC string to peg.el form.
E.g. \"a-zA-Z0-9_-\" → (or (range ?a ?z) (range ?A ?Z) (range ?0 ?9) ?_ ?-)
     \"^>\"          → (and (not ?>) (any))
     \" \\t\\n\\r\"     → (or ?\\s ?\\t ?\\n ?\\r)
Single characters are emitted as one-char strings so peg.el can handle them."
  (let ((negated nil)
        (pos 0)
        parts)
    ;; Check for negation
    (when (and (> (length spec) 0) (= (aref spec 0) ?^))
      (setq negated t pos 1))
    ;; Parse ranges and individual chars
    (while (< pos (length spec))
      (let* ((resolved (peggy--char-class-resolve-char spec pos))
             (c (car resolved))
             (next-pos (cdr resolved)))
        ;; Check if this is a range (c-d)
        (if (and (< next-pos (length spec))
                 (= (aref spec next-pos) ?-)
                 (< (1+ next-pos) (length spec)))
            ;; range: c-d
            (let* ((resolved2 (peggy--char-class-resolve-char spec (1+ next-pos)))
                   (d (car resolved2))
                   (end-pos (cdr resolved2)))
              (push (list 'range c d) parts)
              (setq pos end-pos))
          ;; Check for trailing hyphen (not a range, just literal -)
          (if (and (= next-pos (length spec))
                   ;; This shouldn't happen normally
                   nil)
              (progn (push c parts) (setq pos next-pos))
            ;; single char
            (push c parts)
            (setq pos next-pos)))))
    ;; Handle trailing hyphen in class like [a-zA-Z0-9_-]
    ;; The above logic should already handle it since the hyphen at the end
    ;; won't have a following char for a range.
    (setq parts (nreverse parts))
    ;; Convert bare characters to one-char strings for peg.el compatibility.
    ;; peg.el does not accept bare integers as PEG expressions, but it does
    ;; accept single-char strings.  (range ...) forms are already fine.
    (setq parts (mapcar (lambda (p)
                          (if (integerp p)
                              (char-to-string p)
                            p))
                        parts))
    (let ((positive
           (if (= (length parts) 1)
               (car parts)
             `(or ,@parts))))
      (if negated
          ;; [^...] in PEG: match any char that is NOT in the set
          `(and (not ,positive) (any))
        positive))))

(defun peggy--emit-expr (node)
  "Convert an AST NODE to a peg.el S-expression."
  (pcase node
    (`(str ,v)
     ;; peg.el: always use string form — bare integers are not valid PEG
     ;; expressions, so even single characters must be emitted as strings.
     v)
    (`(class ,spec)
     (peggy--emit-char-class spec))
    (`(ref ,name)
     (peggy--rulename-to-symbol name))
    (`(seq . ,items)
     `(and ,@(mapcar #'peggy--emit-expr items)))
    (`(choice . ,alts)
     `(or ,@(mapcar #'peggy--emit-expr alts)))
    (`(star ,inner)
     `(* ,(peggy--emit-expr inner)))
    (`(plus ,inner)
     `(+ ,(peggy--emit-expr inner)))
    (`(opt ,inner)
     `(opt ,(peggy--emit-expr inner)))
    (`(not ,inner)
     `(not ,(peggy--emit-expr inner)))
    (_ (error "Unknown AST node: %S" node))))

(defun peggy--emit-rule (node)
  "Convert a rule AST NODE to a peg.el rule form: (name expr)."
  (pcase node
    (`(rule ,name ,expr)
     (list (peggy--rulename-to-symbol name)
           (peggy--emit-expr expr)))
    (_ (error "Expected rule node: %S" node))))

;; ---------------------------------------------------------------------------
;; Pretty-printer — emit S-expressions with ?-char syntax
;; ---------------------------------------------------------------------------

(defun peggy--pp-sexp (sexp)
  "Pretty-print SEXP to a string, using ?x char syntax for characters."
  (cond
   ;; Character (integer that should print as ?x)
   ((peggy--peg-char-p sexp)
    (peggy--format-char sexp))
   ;; String — ensure control characters are properly escaped
   ((stringp sexp)
    (peggy--pp-string sexp))
   ;; Symbol
   ((symbolp sexp)
    (symbol-name sexp))
   ;; List
   ((listp sexp)
    (concat "(" (mapconcat #'peggy--pp-sexp sexp " ") ")"))
   ;; Fallback
   (t (prin1-to-string sexp))))

(defun peggy--pp-string (s)
  "Pretty-print string S with proper escape sequences for control chars."
  (let ((result "\""))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (setq result
              (concat result
                      (cond
                       ((= c ?\") "\\\"")
                       ((= c ?\\) "\\\\")
                       ((= c ?\t) "\\t")
                       ((= c ?\n) "\\n")
                       ((= c ?\r) "\\r")
                       (t (char-to-string c)))))))
    (concat result "\"")))

(defun peggy--peg-char-p (x)
  "Return non-nil if X is a character value used in peg.el (not a range int)."
  ;; Characters in our grammar are in the printable ASCII range or whitespace.
  ;; We identify them by being integers in a reasonable char range.
  (and (integerp x)
       (or (<= 9 x 13)     ; \t \n \r etc.
           (<= 32 x 126)))) ; printable ASCII

(defun peggy--pp-rule (rule)
  "Pretty-print a single peg.el RULE (name expr) to a string."
  (let ((name (car rule))
        (expr (cadr rule)))
    (format "  (%s () %s)" (symbol-name name) (peggy--pp-sexp expr))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defun peggy-to-peg-convert-string (peggy-source &optional grammar-name)
  "Convert PEGGY-SOURCE string to a `peg-define-grammar' S-expression string.
GRAMMAR-NAME defaults to \"elot-owl-grammar\"."
  (let ((gname (or grammar-name "elot-owl-grammar")))
    (peggy--init-tokenizer peggy-source)
    (let* ((ast   (peggy--parse-grammar))
           (rules (mapcar #'peggy--emit-rule ast)))
      (with-output-to-string
        (princ ";;; elot-owl-grammar.el --- OWL Manchester Syntax grammar for peg.el  -*- lexical-binding: t; -*-\n\n")
        (princ ";;; Generated by peggy-to-peg.el from owl-manchester.peggy\n")
        (princ ";;; Do not edit by hand — edit the .peggy file and regenerate.\n")
        (princ ";;;\n")
        (princ ";;; To regenerate:\n")
        (princ ";;;   emacs --batch -l syntax/peggy-to-peg.el \\\n")
        (princ ";;;     --eval '(peggy-to-peg-convert-file \"syntax/owl-manchester.peggy\" \\\n")
        (princ ";;;                                         \"syntax/elot-owl-grammar.el\")'\n\n")
        (princ "(require 'peg)\n\n")
        (princ (format "(define-peg-ruleset %s\n" gname))
        (dolist (rule rules)
          (princ (peggy--pp-rule rule))
          (princ "\n"))
        (princ ")\n\n")
        (princ (format "(provide '%s)\n\n" gname))
        (princ (format ";;; %s.el ends here\n" gname))))))

(defun peggy-to-peg-convert-file (input-file output-file &optional grammar-name)
  "Read Peggy grammar from INPUT-FILE, write peg.el rules to OUTPUT-FILE."
  (let* ((src (with-temp-buffer
                (insert-file-contents input-file)
                (buffer-string)))
         (result (peggy-to-peg-convert-string src grammar-name)))
    (with-temp-file output-file
      (insert result))
    (message "Wrote %s (%d rules)" output-file
             (with-temp-buffer
               (insert result)
               (goto-char (point-min))
               (let ((n 0))
                 (while (re-search-forward "^  (" nil t) (cl-incf n))
                 n)))))

(provide 'peggy-to-peg)

;;; peggy-to-peg.el ends here
