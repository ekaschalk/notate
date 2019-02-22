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
