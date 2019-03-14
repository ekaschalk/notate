;;; nt-note-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - overlay methods
;; - width transforms
;; - Initiation and mocking

;; Not Covered:
;; - decompose hook
;; - font lock kwd construction and spec methods

;;; Overlays

(ert-deftest notes:overlays:presence ()
  (nt-test--with-context 'any
      "
1 (string1 foo
2          bar)
3
4 (string2 foo
5          bar)
"
    (nt-test--mock-notes '(("string1" "note1") ("string2" "note2")))

    (should-size (nt-notes<-lines 1 2) 1)
    (should-not  (nt-notes<-line 2))
    (should-size (nt-notes<-lines 3 5) 1)))

(ert-deftest notes:overlays:access-by-line ()
  (nt-test--with-context 'any
      "
1 (string1 string2
2          string1)
3
4 (string2 foo
5          bar)
"
    (nt-test--mock-notes '(("string1" "note1") ("string2" "note2")))

    (should-size (nt-notes<-line 1) 2)
    (should-size (nt-notes<-line 2) 1)
    (should-not  (nt-notes<-line 3))
    (should-size (nt-notes<-line 4) 1)))

;;; Transforms
;;;; Widths

(ert-deftest notes:transforms:width:base-case ()
  (should= (nt-notes->width nil)
           0))

(ert-deftest notes:transforms:width:one-note ()
  (nt-test--with-context 'any "
(string foo bar)
"
    (let ((notes
           (nt-test--mock-notes '(("string" "note")))))
      (should= (nt-notes->width notes)
               (- 6 4)))))

(ert-deftest notes:transforms:width:some-notes ()
  (nt-test--with-context 'any "
(string foo bar)
"
    (let ((notes
           (nt-test--mock-notes '(("string" "note") ("foo" "!")))))
      (should= (nt-notes->width notes)
               (+ (- 6 4)
                  (- 3 1))))))

;;;; Misc

(ert-deftest notes:transforms:string ()
  (nt-test--with-context 'any "
(string foo bar)
"
    (-let (((note _)
            (nt-test--mock-notes '(("string" "note")))))
      (should-s= (nt-note->string note)
                 "string"))))

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
      (should (equal sorted
                     (nt-notes--sort notes))))))

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
