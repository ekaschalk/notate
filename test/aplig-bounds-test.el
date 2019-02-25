;;; aplig-bounds-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - Lisps
;;   - Form opener?

;; Not Covered:
;; -




;;; General

(ert-deftest bounds:general:comments ()
  (aplig-test--with-context 'lispy "
(foo ; bar
     baz)
"
    (-let (((foo bar)
            (aplig-test--mock-ligs '(("foo" "f") ("bar" "b")))))
      (should (aplig-bounds?--in-string-or-comment? bar))
      (should-not (aplig-bounds?--in-string-or-comment? foo)))))

(ert-deftest bounds:general:strings ()
  (aplig-test--with-context 'lispy "
(foo \"bar\"
     baz)
"
    (-let (((foo bar)
            (aplig-test--mock-ligs '(("foo" "f") ("bar" "b")))))
      (should (aplig-bounds?--in-string-or-comment? bar))
      (should-not (aplig-bounds?--in-string-or-comment? foo)))))



;;; Lisps

(ert-deftest bounds:lisps?:form-openers ()
  (aplig-test--with-context 'lispy "
(foo bar
     baz)
"
    (-let (((foo bar)
            (aplig-test--mock-ligs '(("foo" "f") ("bar" "b")))))
      (should (aplig-bounds?--lisps-form-opener? foo))
      (should-not (aplig-bounds?--lisps-form-opener? bar)))))
