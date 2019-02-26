;;; nt-note-test.el --- Tests -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; - overlay methods
;; - width transforms
;; - Initiation and mocking

;; Not Covered:
;; - decompose hook




;;; Overlays

(ert-deftest notes:overlays:presence ()
  (nt-test--with-context 'any
      "
1 (string1 foo
2          bar)
3
4 (string2 foo
5          bar)
"
    (nt-test--mock-notes '(("string1" "note1") ("string2" "note2")))

    (should (nt-notes--at 1))
    (should-not (nt-notes--at 2))
    (should (nt-notes--in 3 5))))

(ert-deftest notes:overlays:access-by-line ()
  (nt-test--with-context 'any
      "
1 (string1 string2
2          string1)
3
4 (string2 foo
5          bar)
"
    (nt-test--mock-notes '(("string1" "note1") ("string2" "note2")))

    (assert-size (nt-notes--at 1) 2)
    (assert-size (nt-notes--at 2) 1)
    (assert-size (nt-notes--at 3) 0)
    (assert-size (nt-notes--at 4) 1)))



;;; Transforms

(ert-deftest notes:transforms:width:base-case ()
  (assert= (nt-notes->width nil) 0))

(ert-deftest notes:transforms:width:one-note ()
  (nt-test--with-context 'any "(string foo bar)"
    (assert= (->
              '(("string" "note"))
              nt-test--mock-notes
              nt-notes->width)
             (- 5 2))))

(ert-deftest notes:transforms:width:some-notes ()
  (nt-test--with-context 'any "(string foo bar)"
    (assert= (->
              '(("string" "note")
                ("foo" "!"))
              nt-test--mock-notes
              nt-notes->width)
             (+ (- 5 2)
                (- 3 1)))))



;;; Init

(ert-deftest notes:init:simple ()
  (nt-test--with-context 'any "(string foo bar)"
    (->
     '(("string" "note"))
     nt-test--mock-notes
     (assert-size 1))))

(ert-deftest notes:init:complex ()
  (nt-test--with-context 'any
      "
1 (string1 string2 string1
2          string1) string2
3
4 (string2 foo
5          bar)
"
    (->
     '(("string1" "note1") ("string2" "note2"))
     nt-test--mock-notes
     (assert-size 6))))
