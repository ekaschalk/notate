;;; nt-note-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - Access (fully tested)
;; - Transforms (fully tested)

;; Uncovered:
;; -

;; OLD
;; Covered:
;; - overlay methods
;; - width transforms
;; - Initiation and mocking

;; Not Covered:
;; - decompose hook
;; - font lock kwd construction and spec methods

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
      (should (nt-note<-pos 3))
      (should-not (nt-note<-pos 10)))))

(ert-deftest notes:access:fundamentals:region ()
  (nt-test--with-context 'any
      "
(note foo
      bar)
"
    (-let (((note)
            (nt-test--mock-notes '(("note" "n")))))
      (should (nt-notes<-region 3 10))
      (should-not (nt-notes<-region 8 10))
      (should-not (nt-notes<-region 0 1)))))

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

;;; Sorting

(ert-deftest note:sorting ()
  (nt-test--with-context 'simple "
1 (string1 foo
2          bar)
3
4 (string2 foo
5          bar)
6
7 (string1 string2
8          foo
9          bar)
"
    (let ((notes (nt-test--mock-notes
                  '(("string1" "note") ("string2" "note"))))
          (sorted `(,@(nt-notes<-line 1)
                    ,@(nt-notes<-line 4)
                    ,@(nt-notes<-line 7))))
      ;; I chose for notes mock to be sorted for easier destructuring
      ;; So no need to explictly call `nt-notes--sort' here
      (should (equal sorted
                     notes)))))

;;; Roots

(ert-deftest nt:notes:roots:no-overlap ()
  (nt-test--with-context 'lispy "
(string1 foo
         bar)

(string1 foo
         bar)
"
    ;; ((2 3))
    (let* ((notes (nt-test--mock-notes '(("string1" "note"))))
           (roots (nt-notes->roots notes)))
      (should (equal notes
                     roots)))))

(ert-deftest nt:notes:roots:some-overlap ()
  (nt-test--with-context 'lispy "
(string1 foo
         (string1 foo
                  bar)
         bar)
"
    ;; ((2 5) (3 4))
    (let* ((notes (nt-test--mock-notes '(("string1" "note"))))
           (roots (nt-notes->roots notes)))
      (should (equal `(,(car notes))
                     roots)))))

(ert-deftest nt:notes:roots:no-overlap-and-some-overlap ()
  (nt-test--with-context 'lispy "
(string1 foo
         bar)

(string1 foo
         (string1 foo
                  bar)
         bar)
"
    ;; ((2 3) (5 8) (6 7))
    (let* ((notes (nt-test--mock-notes '(("string1" "note"))))
           (roots (nt-notes->roots notes)))
      (should (equal `(,(car notes)
                       ,(cadr notes))
                     roots)))))

;;; Init

(ert-deftest notes:init:simple ()
  (nt-test--with-context 'any "
(string foo bar)
"
    (let ((notes (nt-test--mock-notes '(("string" "note")))))
      (should-size notes 1))))

(ert-deftest notes:init:complex ()
  (nt-test--with-context 'any
      "
1 (string1 string2 string1
2          string1) string2
3
4 (string2 foo
5          bar)
"
    (let ((notes
           (nt-test--mock-notes '(("string1" "note1") ("string2" "note2")))))
      (should-size notes 6))))
