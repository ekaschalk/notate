;;; nt-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; -

;; Not Covered:
;; -

;;; Unorganized

(ert-deftest nt:xxx ()
  (nt-test--with-context 'simple "
1 (string1 foo
2          bar)
3
4 (string2 foo
5          bar)
6
7 (string1 string2
8          foo
9          bar)
"
    (let ((notes (nt-test--mock-notes '(("string1" "note") ("string2" "note")))))
      (should (equal `(,@(nt-notes--at 1)
                       ,@(nt-notes--at 4)
                       ,@(nt-notes--at 7))
                     notes)))))
