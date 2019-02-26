;;; nt-mask-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - True indent
;; - Notes contributing to masks
;; - Initiation

;; Not Covered:
;; - decompose hook




;;; Transforms
;;;; Indentation

(ert-deftest masks:transforms:true-indent ()
  (nt-test--with-context 'minimal "
(foo bar
12345
     note)
"
    (assert= (-> 3 nt-mask--indent-at)
             5)
    (assert= (-> 3 nt-mask--at nt-mask->indent)
             5)))

;;;; Widths/Notes

(ert-deftest masks:transforms:widths:simple-1 ()
  (nt-test--with-context 'simple "
(foo bar
     bar
     bar)
"
    (should (-> 2 nt-mask--at nt-mask--empty?))
    (should (-> 3 nt-mask--at nt-mask--empty?))

    (nt-test--mock-notes '(("foo" "f")))

    (assert= (-> 2 nt-mask--at nt-mask->width)
             (- 3 1))
    (should (-> 3 nt-mask--at nt-mask--empty?))))

(ert-deftest masks:transforms:widths:simple-2 ()
  (nt-test--with-context 'simple-2 "
(foo bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f")))

    (assert= (-> 2 nt-mask--at nt-mask->width)
             (- 3 1))
    (assert= (-> 3 nt-mask--at nt-mask->width)
             (- 3 1))))

(ert-deftest masks:transforms:widths:complex ()
  "Multiple notes updating multiple masks"
  (nt-test--with-context 'simple-2 "
(foo bazz
     foo
     bar
     bar
     bar)
"
    (nt-test--mock-notes '(("foo" "f") ("bazz" "bro")))

    (assert= (-> 2 nt-mask--at nt-mask->width)
             (+ (- 3 1)
                (- 4 3)))

    (assert= (-> 3 nt-mask--at nt-mask->width)
             (+ (- 3 1)
                (- 4 3)
                (- 3 1)))
    (assert= (-> 4 nt-mask--at nt-mask->width)
             (- 3 1))
    (should (-> 5 nt-mask--at nt-mask--empty?))))

;;; Init
;;;; Single

(ert-deftest masks:init:buffer ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-mask-list)
    (nt-mask--init 2)
    (assert-size nt-mask-list 1)))

;;;; Multiple

(ert-deftest masks:init:buffer ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-mask-list)
    (nt-masks--init)
    (assert-size nt-mask-list 4)))

(ert-deftest masks:init:ranges:start-only ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-mask-list)
    (let ((start 2) end)
      (nt-masks--init start end))
    (assert-size nt-mask-list (- 4 1))))

(ert-deftest masks:init:ranges:end-only ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-mask-list)
    (let (start (end 4))  ; remember RHS open [a b)
      (nt-masks--init start end))
    (assert-size nt-mask-list (- 4 1))))

(ert-deftest masks:init:ranges ()
  (nt-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not nt-mask-list)
    (let ((start 2) (end 4))
      (nt-masks--init start end))
    (assert-size nt-mask-list (- 4 1 1))))
