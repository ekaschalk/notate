;;; aplig-mask-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - True indent
;; - Ligs contributing to masks

;; Not Covered:
;; - decompose hook




;;; Transforms
;;;; Indentation

(ert-deftest masks:transforms:true-indent ()
  (aplig-test--with-context 'minimal "
(foo bar
12345
     lig)
"
    (assert= (-> 3 aplig-mask--indent-at)
             5)
    (assert= (-> 3 aplig-mask--at aplig-mask->indent)
             5)))

;;;; Widths/Ligs

(ert-deftest masks:transforms:widths:simple-1 ()
  (aplig-test--with-context 'simple "
(foo bar
     bar
     bar)
"
    (should (-> 2 aplig-mask--at aplig-mask--empty?))
    (should (-> 3 aplig-mask--at aplig-mask--empty?))

    (aplig-test--mock-ligs '(("foo" "f")))

    (assert= (-> 2 aplig-mask--at aplig-mask->width)
             (- 3 1))
    (should (-> 3 aplig-mask--at aplig-mask--empty?))))

(ert-deftest masks:transforms:widths:simple-2 ()
  (aplig-test--with-context 'simple-2 "
(foo bar
     bar
     bar)
"
    (aplig-test--mock-ligs '(("foo" "f")))

    (assert= (-> 2 aplig-mask--at aplig-mask->width)
             (- 3 1))
    (assert= (-> 3 aplig-mask--at aplig-mask->width)
             (- 3 1))))

(ert-deftest masks:transforms:widths:complex ()
  "Multiple ligs updating multiple masks"
  (aplig-test--with-context 'simple-2 "
(foo bazz
     foo
     bar
     bar
     bar)
"
    (aplig-test--mock-ligs '(("foo" "f") ("bazz" "bro")))

    (assert= (-> 2 aplig-mask--at aplig-mask->width)
             (+ (- 3 1)
                (- 4 3)))

    (assert= (-> 3 aplig-mask--at aplig-mask->width)
             (+ (- 3 1)
                (- 4 3)
                (- 3 1)))
    (assert= (-> 4 aplig-mask--at aplig-mask->width)
             (- 3 1))
    (should (-> 5 aplig-mask--at aplig-mask--empty?))))

;;; Init
;;;; Single

(ert-deftest masks:init:buffer ()
  (aplig-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not aplig-mask-list)
    (aplig-mask--init 2)
    (assert-size aplig-mask-list 1)))

;;;; Multiple

(ert-deftest masks:init:buffer ()
  (aplig-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not aplig-mask-list)
    (aplig-masks--init)
    (assert-size aplig-mask-list 4)))

(ert-deftest masks:init:ranges:start-only ()
  (aplig-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not aplig-mask-list)
    (let ((start 2) end)
      (aplig-masks--init start end))
    (assert-size aplig-mask-list (- 4 1))))

(ert-deftest masks:init:ranges:end-only ()
  (aplig-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not aplig-mask-list)
    (let (start (end 4))  ; remember RHS open [a b)
      (aplig-masks--init start end))
    (assert-size aplig-mask-list (- 4 1))))

(ert-deftest masks:init:ranges ()
  (aplig-test--with-context 'no-setup
      "
1 foo
2 foo
3 foo
4 foo
"
    (should-not aplig-mask-list)
    (let ((start 2) (end 4))
      (aplig-masks--init start end))
    (assert-size aplig-mask-list (- 4 1 1))))
