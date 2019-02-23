;;; aplig-mask-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; -

;; Not Covered:
;; -




;;; Transforms

(ert-deftest masks:transforms:true-indent ()
  (aplig-test--with-context 'minimal "
(foo bar
12345
     lig)
"
    (-> 3 aplig-mask--indent-at
       (assert= 5))
    (-> 3 aplig-mask--at aplig-mask->indent
       (assert= 5))))

(ert-deftest masks:transforms:widths:simple-1 ()
  (aplig-test--with-context 'simple "
(foo bar
12345
     lig)
"
    (-> 2 aplig-mask--at aplig-mask->width
       (assert= 0))
    (-> 3 aplig-mask--at aplig-mask->width
       (assert= 0))

    (aplig-test--mock-ligs '(("foo" "f")))

    (-> 2 aplig-mask--at aplig-mask->width
       (assert= (- 3 1)))
    (-> 3 aplig-mask--at aplig-mask->width
       (assert= 0))))

(ert-deftest masks:transforms:widths:simple-2 ()
  (aplig-test--with-context 'simple-2 "
(foo bar
12345
     lig)
"
    (-> 2 aplig-mask--at aplig-mask->width
       (assert= 0))
    (-> 3 aplig-mask--at aplig-mask->width
       (assert= 0))

    (aplig-test--mock-ligs '(("foo" "f")))

    (-> 2 aplig-mask--at aplig-mask->width
       (assert= (- 3 1)))
    (-> 3 aplig-mask--at aplig-mask->width
       (assert= (- 3 1)))))
