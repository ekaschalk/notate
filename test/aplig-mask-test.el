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
  "Multiple ligs updating multuple masks"
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
