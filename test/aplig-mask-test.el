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
    (-> 3 aplig-mask--indent-at (assert= 5))
    (-> 3 aplig-mask--at aplig-mask->indent (assert= 5))))

;;;; Widths/Ligs

(ert-deftest masks:transforms:widths:simple-1 ()
  (aplig-test--with-context 'simple "
(foo bar
     bar
     bar)
"
    (-> 2 aplig-mask--at aplig-mask->width (assert= 0))
    (-> 3 aplig-mask--at aplig-mask->width (assert= 0))

    (aplig-test--mock-ligs '(("foo" "f")))

    (-> 2 aplig-mask--at aplig-mask->width (assert= (- 3 1)))
    (-> 3 aplig-mask--at aplig-mask->width (assert= 0))))

(ert-deftest masks:transforms:widths:simple-2 ()
  (aplig-test--with-context 'simple-2 "
(foo bar
     bar
     bar)
"
    (-> 2 aplig-mask--at aplig-mask->width (assert= 0))
    (-> 3 aplig-mask--at aplig-mask->width (assert= 0))

    (aplig-test--mock-ligs '(("foo" "f")))

    (-> 2 aplig-mask--at aplig-mask->width (assert= (- 3 1)))
    (-> 3 aplig-mask--at aplig-mask->width (assert= (- 3 1)))))

(ert-deftest masks:transforms:widths:complex ()
  "Multiple ligs updating multuple masks"
  (aplig-test--with-context 'simple-2 "
(foo bazz
     foo
     bar
     bar
     bar)
"
    (-> 2 aplig-mask--at aplig-mask->width (assert= 0))
    (-> 3 aplig-mask--at aplig-mask->width (assert= 0))

    (aplig-test--mock-ligs '(("foo" "f") ("bazz" "bro")))

    (-> 2 aplig-mask--at aplig-mask->width
       (assert= (+ (- 3 1)
                   (- 4 3))))
    (-> 3 aplig-mask--at aplig-mask->width
       (assert= (+ (- 3 1)
                   (- 4 3)
                   (- 3 1))))
    (-> 4 aplig-mask--at aplig-mask->width
       (assert= (- 3 1)))
    (-> 5 aplig-mask--at aplig-mask->width
       (assert= 0))))
