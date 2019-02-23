;;; aplig-bounds-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; -

;; Not Covered:
;; -




;;; Lisps

(ert-deftest bounds:lisps ()
  (aplig-test--with-context 'lispy "
(foo bar
     baz)
"
    ;;
    ))
