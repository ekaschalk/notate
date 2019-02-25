;;; aplig-bounds-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - Lisps
;;   - Form opener?

;; Not Covered:
;; -




;;; Lisps

(ert-deftest bounds:lisps:?:form-openers ()
  (aplig-test--with-context 'no-setup "
(foo bar
     baz)
"
    (let* ((ligs (aplig-test--mock-ligs '(("foo" "f") ("bar" "b"))))
           (foo-lig (-first-item ligs))
           (bar-lig (-second-item ligs)))
      (should (aplig-bounds?--lisps-form-opener? foo-lig))
      (should-not (aplig-bounds?--lisps-form-opener? bar-lig)))))
