;;; nt-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; -

;; Not Covered:
;; -

;;; Roots

(ert-deftest nt:notes:roots:no-overlap ()
  (nt-test--with-context 'lispy "
(string1 foo
         bar)
"
    (let ((notes (nt-test--mock-notes '(("string1" "note")))))
      ;; ((2 3))

      )))

(ert-deftest nt:notes:roots:some-overlap ()
  (nt-test--with-context 'lispy "
(string1 foo
         (string2 foo
                  bar)
         bar)
"
    (let ((notes (nt-test--mock-notes '(("string1" "note") ("string2" "note")))))
      ;; ((2 5) (3 4))

      )))

(ert-deftest nt:notes:roots:no-overlap-and-some-overlap ()
  (nt-test--with-context 'lispy "
(string1 foo
         bar)

(string1 foo
         (string2 foo
                  bar)
         bar)
"
    (let ((notes (nt-test--mock-notes '(("string1" "note") ("string2" "note")))))
      ;; ((2 3) (5 8) (6 7))

      )))
