;;; aplig-lig-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; -

;; Not Covered:
;; -




;;; Transforms
;;;; Widths

(ert-deftest ligs:transforms:width:none ()
  (assert= (aplig-ligs->width nil)
           0))

(ert-deftest ligs:transforms:width:one-lig ()
  (aplig-test--with-context 'minimal "(string foo bar)"
    (assert= (->
              '(("string" "lig"))
              aplig-test--mock-ligs
              aplig-ligs->width)
             (- 5 2))))

(ert-deftest ligs:transforms:width:some-ligs ()
  (aplig-test--with-context 'minimal "(string foo bar)"
    (assert= (->
              '(("string" "lig")
                ("foo" "!"))
              aplig-test--mock-ligs
              aplig-ligs->width)
             (+ (- 5 2)
                (- 3 1)))))
