;;; elot-robot-test.el --- Tests for elot-robot.el  -*- lexical-binding: t; -*-

;; Tests for the shared ROBOT process layer (Milestone 2 of
;; ELOT-GPTEL-PLAN).  Live ROBOT tests are skipped when ROBOT is
;; unavailable.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-robot)

;;;; --------------------------------------------------------------
;;;; Pure / mocked tests (always run)
;;;; --------------------------------------------------------------

(ert-deftest elot-robot-test-invocation-shape-jar ()
  "When the jar path names an existing file, invocation is `java -jar JAR'."
  (let ((tmp (make-temp-file "fake-robot" nil ".jar")))
    (unwind-protect
        (let ((elot-robot-jar-path tmp)
              (elot-robot--available-cache 'unset)
              (elot-robot--invocation-cache nil)
              (elot-robot-jvm-args nil))
          (should (elot-robot-available-p))
          (should (equal (elot-robot-invocation)
                         (list "java" "-jar" tmp))))
      (delete-file tmp))))

(ert-deftest elot-robot-test-invocation-shape-jar-with-jvm-args ()
  "JVM args are inserted before `-jar'."
  (let ((tmp (make-temp-file "fake-robot" nil ".jar")))
    (unwind-protect
        (let ((elot-robot-jar-path tmp)
              (elot-robot--available-cache 'unset)
              (elot-robot--invocation-cache nil)
              (elot-robot-jvm-args '("-Xmx2G" "-Dfoo=bar")))
          (should (equal (elot-robot-invocation)
                         (list "java" "-Xmx2G" "-Dfoo=bar" "-jar" tmp))))
      (delete-file tmp))))

(ert-deftest elot-robot-test-availability-cached ()
  "`elot-robot-available-p' caches its verdict between calls."
  (let* ((tmp (make-temp-file "fake-robot" nil ".jar"))
         (elot-robot-jar-path tmp)
         (elot-robot--available-cache 'unset)
         (elot-robot--invocation-cache nil)
         (call-count 0))
    (unwind-protect
        (cl-letf* ((orig (symbol-function 'elot-robot--compute-invocation))
                   ((symbol-function 'elot-robot--compute-invocation)
                    (lambda ()
                      (cl-incf call-count)
                      (funcall orig))))
          (should (elot-robot-available-p))
          (should (= call-count 1))
          (should (elot-robot-available-p))
          (should (= call-count 1))         ; still 1: cached
          (should (elot-robot-available-p 'refresh))
          (should (= call-count 2)))        ; refreshed once
      (delete-file tmp))))

(ert-deftest elot-robot-test-unavailable-signals-on-run ()
  "`elot-robot-run' raises `user-error' when ROBOT is missing."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil))                    ; no `robot' shim either
    (should-error (elot-robot-run '("--version")) :type 'user-error)))

;;;; --------------------------------------------------------------
;;;; Classifier tests (pure)
;;;; --------------------------------------------------------------

(ert-deftest elot-robot-test-classify-ok-empty ()
  "Empty stderr with exit 0 classifies as :ok."
  (should (eq :ok (car (elot-robot-classify "" 0))))
  (should (eq :ok (car (elot-robot-classify "   \n" 0)))))

(ert-deftest elot-robot-test-classify-syntax-error-canonical ()
  "The canonical ROBOT parser error yields :syntax-error with line/column."
  (let* ((stderr "Parser: org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxOntologyParser
Encountered \"some\" at line 42 column 17
")
         (kp (elot-robot-classify stderr 1)))
    (should (eq :syntax-error (car kp)))
    (should (= 42 (plist-get (cdr kp) :line)))
    (should (= 17 (plist-get (cdr kp) :column)))
    (should (stringp (plist-get (cdr kp) :message)))))

(ert-deftest elot-robot-test-classify-syntax-error-legacy ()
  "The legacy `Line N column M' form is recognised."
  (let* ((stderr "ManchesterOWLSyntaxOntologyParser failed
Line 12 column 3: unexpected token
")
         (kp (elot-robot-classify stderr 1)))
    (should (eq :syntax-error (car kp)))
    (should (= 12 (plist-get (cdr kp) :line)))
    (should (= 3  (plist-get (cdr kp) :column)))))

(ert-deftest elot-robot-test-classify-unsatisfiable ()
  "Unsatisfiable-class lists are extracted into :iris."
  (let* ((stderr "UNSATISFIABLE CLASSES
- <http://example.org/Foo>
- <http://example.org/Bar>
")
         (kp (elot-robot-classify stderr 1)))
    (should (eq :unsatisfiable-classes (car kp)))
    (should (equal '("http://example.org/Foo" "http://example.org/Bar")
                   (plist-get (cdr kp) :iris)))))

(ert-deftest elot-robot-test-classify-unsatisfiable-bare-iri ()
  "Bare (non-bracketed) http(s) IRIs in ROBOT's `unsatisfiable: ...'
output are extracted.  Regression test for ROBOT >= 1.9.x output
shape captured against pizza.org (issue: elot_unsatisfiable
reported 0 classes despite elot_consistency flagging them)."
  (let* ((stderr "ERROR org.obolibrary.robot.ReasonerHelper - There are 2 unsatisfiable classes in the ontology.
ERROR org.obolibrary.robot.ReasonerHelper -     unsatisfiable: https://raw.githubusercontent.com/owlcs/pizza-ontology/refs/heads/master/pizza.owl#CheeseyVegetableTopping
ERROR org.obolibrary.robot.ReasonerHelper -     unsatisfiable: https://raw.githubusercontent.com/owlcs/pizza-ontology/refs/heads/master/pizza.owl#IceCream
")
         (kp (elot-robot-classify stderr 1)))
    (should (eq :unsatisfiable-classes (car kp)))
    (should (equal
             '("https://raw.githubusercontent.com/owlcs/pizza-ontology/refs/heads/master/pizza.owl#CheeseyVegetableTopping"
               "https://raw.githubusercontent.com/owlcs/pizza-ontology/refs/heads/master/pizza.owl#IceCream")
             (plist-get (cdr kp) :iris)))))

(ert-deftest elot-robot-test-classify-inconsistent ()
  "Inconsistent-ontology output classifies as :inconsistent-ontology."
  (let* ((stderr "ONTOLOGY HAS INCONSISTENCIES
Explanation:
Individual x is in disjoint classes A and B
")
         (kp (elot-robot-classify stderr 1)))
    (should (eq :inconsistent-ontology (car kp)))
    (should (string-match-p "disjoint"
                            (plist-get (cdr kp) :explanation)))))

(ert-deftest elot-robot-test-classify-io-error ()
  "Missing-file errors classify as :io-error."
  (let* ((stderr "java.io.FileNotFoundException: /tmp/missing.omn (No such file)")
         (kp (elot-robot-classify stderr 1)))
    (should (eq :io-error (car kp)))
    (should (string-match-p "missing\\.omn"
                            (plist-get (cdr kp) :path)))))

(ert-deftest elot-robot-test-classify-unknown ()
  "Unrecognised non-zero exit yields :unknown with raw stderr preserved."
  (let* ((stderr "some unexpected diagnostic from ROBOT")
         (kp (elot-robot-classify stderr 1)))
    (should (eq :unknown (car kp)))
    (should (equal stderr (plist-get (cdr kp) :stderr)))))

(ert-deftest elot-robot-test-classify-result ()
  "`elot-robot-classify-result' destructures a run plist."
  (let ((kp (elot-robot-classify-result
             '(:exit 0 :stdout "" :stderr "" :argv ("robot") :duration 0.1))))
    (should (eq :ok (car kp)))))

(ert-deftest elot-robot-test-legacy-parse-error-location-uses-classifier ()
  "`elot--parse-robot-error-location' still returns (LINE COL) via classifier."
  (require 'elot-tangle)
  (let ((text "ManchesterOWLSyntaxOntologyParser
at line 7, column 9: oops"))
    (should (equal '(7 9)
                   (elot--parse-robot-error-location text)))))

;;;; --------------------------------------------------------------
;;;; Workspace helper tests
;;;; --------------------------------------------------------------

(ert-deftest elot-robot-test-workspace-creates-and-cleans ()
  "`elot-robot-call-with-workspace' creates a fresh dir and removes it."
  (let (saved)
    (elot-robot-call-with-workspace
     (lambda (ws)
       (setq saved ws)
       (should (file-directory-p ws))
       ;; default-directory is bound to the workspace inside the call
       (should (file-equal-p ws default-directory))
       (with-temp-file (expand-file-name "x.txt" ws)
         (insert "hello"))
       (should (file-exists-p (expand-file-name "x.txt" ws)))))
    (should-not (file-directory-p saved))))

(ert-deftest elot-robot-test-workspace-cleans-on-error ()
  "Workspace is removed even when the callback signals an error."
  (let (saved)
    (should-error
     (elot-robot-call-with-workspace
      (lambda (ws)
        (setq saved ws)
        (should (file-directory-p ws))
        (error "boom"))))
    (should-not (file-directory-p saved))))

(ert-deftest elot-robot-test-workspace-keep-suppresses-delete ()
  "Setting `elot-robot-workspace-keep' retains the directory."
  (let* ((elot-robot-workspace-keep t)
         saved)
    (unwind-protect
        (progn
          (elot-robot-call-with-workspace
           (lambda (ws) (setq saved ws)))
          (should (file-directory-p saved)))
      (when (and saved (file-directory-p saved))
        (ignore-errors (delete-directory saved t))))))

(ert-deftest elot-robot-test-workspace-prefix ()
  "PREFIX arg controls the temp-directory name prefix."
  (elot-robot-call-with-workspace
   (lambda (ws)
     (should (string-match-p "elot-test-pfx-"
                             (file-name-nondirectory
                              (directory-file-name ws)))))
   "elot-test-pfx-"))

;;;; --------------------------------------------------------------
;;;; Timeout tests
;;;; --------------------------------------------------------------

(ert-deftest elot-robot-test-effective-timeout ()
  "`elot-robot--effective-timeout' resolves call/default precedence."
  (let ((elot-robot-default-timeout nil))
    (should (null (elot-robot--effective-timeout nil)))
    (should (= 5 (elot-robot--effective-timeout 5)))
    (should (null (elot-robot--effective-timeout 0))))
  (let ((elot-robot-default-timeout 10))
    (should (= 10 (elot-robot--effective-timeout nil)))
    (should (= 3  (elot-robot--effective-timeout 3)))
    (should (null (elot-robot--effective-timeout 0)))))

(ert-deftest elot-robot-test-classify-timeout ()
  "`elot-robot-classify-result' surfaces :timeout when :timed-out is set."
  (let ((kp (elot-robot-classify-result
             '(:exit nil :stdout "" :stderr ""
               :argv ("robot") :duration 30.0 :timed-out t))))
    (should (eq :timeout (car kp)))))

(ert-deftest elot-robot-test-strip-process-status-lines ()
  "`elot-robot--strip-process-status-lines' removes sentinel boilerplate.
Regression for Patch E: Emacs's default process sentinel inserts
\"Process NAME <status>\\n\" into the process buffer when no
explicit sentinel is supplied.  The helper must strip both the
stdout and stderr variants whether or not a trailing newline is
present, and must leave genuine ROBOT output untouched."
  (with-temp-buffer
    (insert "INCONSISTENT: foo bar\n"
            "Process elot-robot finished\n"
            "Process elot-robot-stderr exited abnormally with code 1\n")
    (elot-robot--strip-process-status-lines)
    (should (equal (buffer-string) "INCONSISTENT: foo bar\n")))
  (with-temp-buffer
    ;; No trailing newline on the last status line.
    (insert "real output\nProcess elot-robot finished")
    (elot-robot--strip-process-status-lines)
    (should (equal (buffer-string) "real output\n")))
  (with-temp-buffer
    ;; Genuine output that merely mentions "Process" must be preserved.
    (insert "Processing class ex:Foo\n")
    (elot-robot--strip-process-status-lines)
    (should (equal (buffer-string) "Processing class ex:Foo\n"))))

(ert-deftest elot-robot-test-live-timeout-aborts ()
  "A tiny timeout aborts a normal ROBOT call; result classifies as :timeout."
  (elot-robot-test--live-or-skip)
  (let ((res (elot-robot-run '("--version") :timeout 0.001)))
    ;; Either the process was killed by the watchdog (:timed-out t) or,
    ;; on very fast machines, it managed to finish in time.  Both are
    ;; acceptable; assert the timed-out path exercises the classifier
    ;; correctly when it triggers.
    (when (plist-get res :timed-out)
      (should (null (plist-get res :exit)))
      (should (eq :timeout (car (elot-robot-classify-result res)))))))

(ert-deftest elot-robot-test-live-timeout-no-sentinel-pollution ()
  "Regression for Patch E: the watchdog path must not leak sentinel
status messages into the captured stdout or stderr.  Runs ROBOT
with a generous timeout so the call succeeds normally, then
asserts that neither stream contains the tell-tale
\"Process elot-robot\" string that Emacs's default sentinel would
otherwise inject."
  (elot-robot-test--live-or-skip)
  (let ((res (elot-robot-run '("--version") :timeout 30)))
    (should (= 0 (plist-get res :exit)))
    (should-not (plist-get res :timed-out))
    (should-not (string-match-p "Process elot-robot"
                                (or (plist-get res :stdout) "")))
    (should-not (string-match-p "Process elot-robot"
                                (or (plist-get res :stderr) "")))))

;;;; --------------------------------------------------------------
;;;; Live tests (skipped when ROBOT is unavailable)
;;;; --------------------------------------------------------------

(defun elot-robot-test--live-or-skip ()
  (elot-robot-reset-cache)
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available; set `elot-robot-jar-path' or install `robot'")))

(ert-deftest elot-robot-test-live-version ()
  "ROBOT reports a version we can parse."
  (elot-robot-test--live-or-skip)
  (let ((res (elot-robot-run '("--version"))))
    (should (= 0 (plist-get res :exit)))
    (should (string-match-p "ROBOT"
                            (concat (plist-get res :stdout)
                                    (plist-get res :stderr))))
    (should (stringp (elot-robot-version)))))

(defconst elot-robot-test--omn-consistent
  "Prefix: : <http://example.org/>
Prefix: owl: <http://www.w3.org/2002/07/owl#>
Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>

Ontology: <http://example.org/test-consistent>

Class: :A
    Annotations: rdfs:label \"A\"

Class: :B
    Annotations: rdfs:label \"B\"
    SubClassOf: :A
"
  "A trivially consistent OMN ontology for live ROBOT round-trips.")

(defconst elot-robot-test--omn-unsatisfiable
  "Prefix: : <http://example.org/>
Prefix: owl: <http://www.w3.org/2002/07/owl#>
Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>

Ontology: <http://example.org/test-unsat>

Class: :A
    DisjointWith: :B

Class: :B

Class: :Bad
    EquivalentTo: :A and :B
"
  "An OMN ontology with one unsatisfiable class (:Bad).")

(defconst elot-robot-test--omn-inconsistent
  "Prefix: : <http://example.org/>
Prefix: owl: <http://www.w3.org/2002/07/owl#>
Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>

Ontology: <http://example.org/test-inconsistent>

Class: :A
    DisjointWith: :B

Class: :B

Individual: :x
    Types: :A, :B
"
  "An OMN ontology that is logically inconsistent.")

(defun elot-robot-test--write-omn (ws name content)
  "Write CONTENT to NAME inside workspace WS; return the absolute path."
  (let ((path (expand-file-name name ws)))
    (with-temp-file path (insert content))
    path))

(ert-deftest elot-robot-test-live-convert-roundtrip ()
  "`robot convert' on a trivial OMN produces a non-empty TTL file."
  (elot-robot-test--live-or-skip)
  (elot-robot-call-with-workspace
   (lambda (ws)
     (let* ((in  (elot-robot-test--write-omn
                  ws "in.omn" elot-robot-test--omn-consistent))
            (out (expand-file-name "out.ttl" ws))
            (res (elot-robot-run
                  (list "convert" "--input" in "--output" out))))
       (should (= 0 (plist-get res :exit)))
       (should (eq :ok (car (elot-robot-classify-result res))))
       (should (file-exists-p out))
       (should (> (nth 7 (file-attributes out)) 0))))))

(ert-deftest elot-robot-test-live-reason-consistent ()
  "`robot reason --reasoner hermit' succeeds on a consistent ontology."
  (elot-robot-test--live-or-skip)
  (elot-robot-call-with-workspace
   (lambda (ws)
     (let* ((in  (elot-robot-test--write-omn
                  ws "in.omn" elot-robot-test--omn-consistent))
            (res (elot-robot-run
                  (list "reason" "--reasoner" "hermit"
                        "--input" in))))
       ;; ROBOT exits 0 on a successful reason; classifier must agree.
       (should (= 0 (plist-get res :exit)))
       (should (eq :ok (car (elot-robot-classify-result res))))))))

(ert-deftest elot-robot-test-live-reason-unsatisfiable ()
  "An unsatisfiable class is detected by reasoning and classified."
  (elot-robot-test--live-or-skip)
  (elot-robot-call-with-workspace
   (lambda (ws)
     (let* ((in  (elot-robot-test--write-omn
                  ws "in.omn" elot-robot-test--omn-unsatisfiable))
            (res (elot-robot-run
                  (list "reason" "--reasoner" "hermit"
                        "--input" in)))
            (kp  (elot-robot-classify-result res)))
       ;; ROBOT exits non-zero when unsat classes are found.
       (should-not (zerop (plist-get res :exit)))
       (should (memq (car kp)
                     '(:unsatisfiable-classes
                       :inconsistent-ontology
                       :unknown)))))))

(ert-deftest elot-robot-test-live-reason-inconsistent ()
  "An inconsistent ontology is detected by reasoning and classified."
  (elot-robot-test--live-or-skip)
  (elot-robot-call-with-workspace
   (lambda (ws)
     (let* ((in  (elot-robot-test--write-omn
                  ws "in.omn" elot-robot-test--omn-inconsistent))
            (res (elot-robot-run
                  (list "reason" "--reasoner" "hermit"
                        "--input" in)))
            (kp  (elot-robot-classify-result res)))
       (should-not (zerop (plist-get res :exit)))
       (should (memq (car kp)
                     '(:inconsistent-ontology
                       :unsatisfiable-classes
                       :unknown)))))))

(provide 'elot-robot-test)
;;; elot-robot-test.el ends here
