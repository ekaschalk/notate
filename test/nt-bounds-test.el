;;; nt-bounds-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - General
;;   - comments/strings
;; - Lisps
;;   - Form opener?

;; Not Covered:
;; - Alot (restricting to simple cases atm until change functions finished)




;;; General

(ert-deftest bounds:general:comments ()
  (nt-test--with-context 'lispy "
(foo ; bar
     baz)
"
    (-let (((foo bar)
            (nt-test--mock-notes '(("foo" "f") ("bar" "b")))))
      (should (nt-bounds?--in-string-or-comment? bar))
      (should-not (nt-bounds?--in-string-or-comment? foo)))))

(ert-deftest bounds:general:strings ()
  (nt-test--with-context 'lispy "
(foo \"bar\"
     baz)
"
    (-let (((foo bar)
            (nt-test--mock-notes '(("foo" "f") ("bar" "b")))))
      (should (nt-bounds?--in-string-or-comment? bar))
      (should-not (nt-bounds?--in-string-or-comment? foo)))))



;;; Lisps

(ert-deftest bounds:lisps?:form-openers ()
  (nt-test--with-context 'lispy "
(foo bar
     baz)
"
    (-let (((foo bar)
            (nt-test--mock-notes '(("foo" "f") ("bar" "b")))))
      (should (nt-bounds?--lisps-form-opener? foo))
      (should-not (nt-bounds?--lisps-form-opener? bar)))))
