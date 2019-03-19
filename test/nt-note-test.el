;;; nt-note-test.el --- Tests -*- lexical-binding: t -*-

;; ~ TEST COVERAGE ~

;; Covered:
;; - Access
;; - Transforms
;; - Sorting
;; - Root-Finding
;; - Deletion (internal-only)

;; Implicitly Covered:
;; - Initiation (note mocks bypass font-lock-mode, not inititation methods)
;; - Insertion

;; Not Covered:
;; - Decomposition

;;; Access
;;;; Fundamentals

(ert-deftest notes:access:fundamentals:pos ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note)
            (nt-test--mock-notes '(("note" "n")))))
      (should* (nt-note<-pos 3)
               (not (nt-note<-pos 10))))))

(ert-deftest notes:access:fundamentals:region ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note)
            (nt-test--mock-notes '(("note" "n")))))
      (should* (nt-notes<-region 3 10)
               (not (nt-notes<-region 8 10))
               (not (nt-notes<-region 0 1))))))

(ert-deftest notes:access:fundamentals:line ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n") ("foo" "n")))))
      (should-equal (nt-notes<-line 1)
                    notes)
      (should-not (nt-notes<-line 2)))))

;;;; Extensions

(ert-deftest notes:access:extensions:lines ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note bar)
            (nt-test--mock-notes '(("note" "n") ("bar" "n")))))
      (should-equal (nt-notes<-lines 1 2)
                    `(,note))
      (should-equal (nt-notes<-lines 1 3)
                    `(,note ,bar))
      (should-equal (nt-notes<-lines 2 3)
                    `(,bar)))))

;;; Transforms
;;;; Misc

(ert-deftest notes:transforms:misc:string ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note)
            (nt-test--mock-notes '(("note" "n")))))
      (should-s= (nt-note->string note)
                 "note"))))

(ert-deftest notes:transforms:misc:width ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n") ("foo" "fo")))))
      (should= (nt-notes->width notes)
               (+ (- 4 1)
                  (- 3 2))))))

(ert-deftest notes:transforms:misc:indent ()
  (nt-test--with-context 'any
      "
(note foo
      6bar)
"
    (-let (((note)
            (nt-test--mock-notes '(("bar" "b")))))
      (should= (nt-note->indent note)
               6))))

(ert-deftest notes:transforms:misc:idx ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note foo bar)
            (nt-test--mock-notes '(("note" "n") ("foo" "f") ("bar" "b")))))
      (should= (nt-note->idx note)
               1)
      (should= (nt-note->idx foo)
               2)
      (should= (nt-note->idx bar)
               3)
      (should= (-> (make-overlay (point-max) (point-max)) nt-note->idx)
               3)
      (should= (-> (make-overlay (point-min) (point-min)) nt-note->idx)
               0))))

;;; Relationships
;;;; Comparisons

(ert-deftest notes:relationships:comparisons:sorting ()
  (nt-test--with-context 'any "
1 (note1 foo
2        bar)
3
4 (note2 foo
5        bar)
6
7 (note1 note2
8        foo
9        bar)
"
    ;; The mock itself sorts for easier destructuring, so dont need to
    ;; explicitly call `nt-notes--sort'
    (-let ((notes
            (nt-test--mock-notes '(("note1" "n") ("note2" "n")))))
      (should-equal notes
                    `(,@(nt-notes<-line 1)
                      ,@(nt-notes<-line 4)
                      ,@(nt-notes<-line 7))))))

;;;; Roots

;; Notice that the CONTEXT does matter for root tests.
;; These cases can be made more granular down the road, but good enough atm.

(ert-deftest notes:relationships:roots:no-children ()
  (nt-test--with-context 'lispy "
(note foo
      bar)

(note foo
      bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    notes))))

(ert-deftest notes:relationships:roots:one-root-with-children ()
  (nt-test--with-context 'lispy "
(note foo
      (note foo
            bar)
      bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    `(,(car notes))))))

(ert-deftest notes:relationships:roots:no-duplicates ()
  ;; Under specific circumstances duplicate roots were present in past
  (nt-test--with-context 'lispy "
(note foo bar)

(note (note foo bar) bar)

(note foo bar)
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    `(,(car notes)
                      ,(cadr notes)
                      ,(cadddr notes))))))

(ert-deftest notes:relationships:roots:full-complexity ()
  (nt-test--with-context 'lispy "
(note foo
      bar)

(note foo (note foo
                bar))

(note foo
      (note foo (note foo bar)
            (note foo
                  foo bar)
            bar)
      (note foo bar))
"
    (-let ((notes
            (nt-test--mock-notes '(("note" "n")))))
      (should-equal (nt-notes->roots notes)
                    `(,(car notes)
                      ,(cadr notes)
                      ,(cadddr notes))))))

;;; Management
;;;; Deletion
;;;;; Internal

(ert-deftest notes:management:deletion:internal ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note foo)
            (nt-test--mock-notes '(("note" "n") ("foo" "fo")))))

      (should-equal nt-notes
                    `(,note ,foo))

      (nt-note--delete-internal foo)
      (should-equal nt-notes
                    `(,note))

      (nt-note--delete-internal foo)  ; Check no error thrown
      (should-equal nt-notes
                    `(,note))

      (nt-note--delete-internal note)
      (should-not nt-notes))))

;;;;; Commands

;; tbd
