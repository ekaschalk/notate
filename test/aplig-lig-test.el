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
1 (string1 foo
2          bar)
3
4 (string2 foo
5          bar)
"
    (aplig-test--mock-ligs '(("string1" "lig1") ("string2" "lig2")))

    (should (aplig-ligs--present?
             (aplig-base--line-start 1)
             (aplig-base--line-end   1)))
    (should-not (aplig-ligs--present?
                 (aplig-base--line-start 2)
                 (aplig-base--line-end   2)))
    (should (aplig-ligs--present?
             (aplig-base--line-start 3)
             (aplig-base--line-end   5)))))

(ert-deftest ligs:overlays:access-by-line ()
  (aplig-test--with-context 'minimal
      "
1 (string1 string2
2          string1)
3
4 (string2 foo
5          bar)
"
    (aplig-test--mock-ligs '(("string1" "lig1") ("string2" "lig2")))

    (assert-size (aplig-ligs--at 1) 2)
    (assert-size (aplig-ligs--at 2) 1)
    (assert-size (aplig-ligs--at 3) 0)
    (assert-size (aplig-ligs--at 4) 1)))



;;; Transforms

(ert-deftest ligs:transforms:width:base-case ()
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
