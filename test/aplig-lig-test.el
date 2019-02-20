;;; aplig-lig-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - width transforms

;; Not Covered:
;; - boundary-fns
;; - overlay methods




;;; Overlays

(ert-deftest ligs:overlays:presence ()
  (aplig-test--with-context 'minimal
      "
2 (string1 foo
3          bar)
4
5 (string2 foo
6          bar)
"
    (aplig-test--mock-ligs '(("string1" "lig1") ("string2" "lig2")))

    (should (aplig-ligs--present?
             (aplig-base--line-start 2)
             (aplig-base--line-end   2)))
    (should-not (aplig-ligs--present?
                 (aplig-base--line-start 3)
                 (aplig-base--line-end   3)))
    (should (aplig-ligs--present?
             (aplig-base--line-start 4)
             (aplig-base--line-end   6)))))



;;; Transforms

(ert-deftest ligs:transforms:width:base-case ()
  (assert= (aplig-ligs->width nil)
           0))

(ert-deftest ligs:transforms:width:one-lig ()
  (aplig-test--with-context 'minimal
      "(string foo bar)"
    (assert= (->
              '(("string" "lig"))
              aplig-test--mock-ligs
              aplig-ligs->width)
             (- 5 2))))

(ert-deftest ligs:transforms:width:some-ligs ()
  (aplig-test--with-context 'minimal
      "(string foo bar)"
    (assert= (->
              '(("string" "lig")
                ("foo" "!"))
              aplig-test--mock-ligs
              aplig-ligs->width)
             (+ (- 5 2)
                (- 3 1)))))
