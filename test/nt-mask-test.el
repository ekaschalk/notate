;;; nt-mask-test.el --- Tests -*- lexical-binding: t -*-

;; ~ TEST COVERAGE ~

;; Covered:
;; - Access
;; - Transforms

;; Implicitly Covered:
;; -

;; Not Covered:
;; -


;; OLD
;; Covered:
;; - Notes contributing to masks
;; - Initiation

;; Not Covered:
;; - decompose hook

;;; Access
;;;; Fundamentals

(ert-deftest masks:access:fundamentals:line ()
  (nt-test--with-context 'minimal "
1 foo
2 foo
"
    (should* (not (nt-mask<-line 0))
             (nt-mask<-line 1)
             (nt-mask<-line 2)
             (not (nt-mask<-line 3)))))

(ert-deftest masks:access:fundamentals:lines ()
  (nt-test--with-context 'minimal "
1 foo
2 foo
"
    (should* (not (nt-masks<-lines 0 1))
             (nt-masks<-lines 1 2)
             (nt-masks<-lines 1 3)
             (nt-masks<-lines 0 3)
             (not (nt-masks<-lines 3 4)))))

;;;; Extensions

(ert-deftest masks:access:extensions:region ()
  (nt-test--with-context 'minimal "
123
678
"
    (should-size (nt-masks<-region 1 3)
                 1)
    (should-size (nt-masks<-region 1 6)
                 2)
    (should-size (nt-masks<-region 6 8)
                 1)))

;;; Transforms
;;;; Misc
;;;;; Indent

(ert-deftest masks:transforms:misc:indent ()
  (nt-test--with-context 'minimal "
(foo bar
12345
     note)
"
    (should= (-> 3 nt-mask<-line nt-mask->indent)
             (-> 3 nt-line->indent)
             5)))

;;;;; Widths

(ert-deftest masks:transforms:misc:widths:simplest ()
  (nt-test--with-context 'simple "
(foo bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f")))

    (should* (-> 1 nt-mask<-line nt-mask--empty?)
             (= (-> 2 nt-mask<-line nt-mask->width)
                (- 3 1))
             (-> 3 nt-mask<-line nt-mask--empty?))))

(ert-deftest masks:transforms:misc:widths:simplest-many-lines ()
  (nt-test--with-context 'simple-2 "
(foo bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f")))

    (should* (-> 1 nt-mask<-line nt-mask--empty?)
             (= (-> 2 nt-mask<-line nt-mask->width)
                (-> 3 nt-mask<-line nt-mask->width))
             (- 3 1))))

(ert-deftest masks:transforms:misc:widths:complex ()
  "Multiple notes updating multiple masks"
  (nt-test--with-context 'simple-2 "
(foo bazz
     foo
     bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f") ("bazz" "bro")))

    (let ((note-1-width (- 3 1))
          (note-2-width (- 4 3)))
      (should* (-> 1 nt-mask<-line nt-mask--empty?)
               (= (-> 2 nt-mask<-line nt-mask->width)
                  (+ note-1-width
                     note-2-width))
               (= (-> 3 nt-mask<-line nt-mask->width)
                  (+ note-1-width
                     note-2-width
                     note-1-width))
               (= (-> 4 nt-mask<-line nt-mask->width)
                  note-1-width)
               (-> 5 nt-mask<-line nt-mask--empty?)))))

;;; Init
;;;; Single

(ert-deftest masks:init:single ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (nt-mask--init 2)
    (should-size nt-masks 1)))

;;;; Multiple

(ert-deftest masks:init:buffer ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (nt-masks--init)
    (should-size nt-masks 4)))

(ert-deftest masks:init:ranges:start-only ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (let ((start 2) end)
      (nt-masks--init start end))
    (should-size nt-masks (- 4 1))))

(ert-deftest masks:init:ranges:end-only ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (let (start (end 4))
      (nt-masks--init start end))
    (should-size nt-masks (- 4 1))))

(ert-deftest masks:init:ranges ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-masks)
    (let ((start 2) (end 4))
      (nt-masks--init start end))
    (should-size nt-masks (- 4 1 1))))
