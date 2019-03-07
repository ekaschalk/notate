;;; nt-tree-test.el --- Note Trees -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; -

;; Not Covered:
;; -

;;; Querying

(ert-deftest tree:querying:items ()
  (nt-test--with-context 'simple
      "
1 (string1 string2
2          bar)
3
4 (string2 foo
5          bar)
"
    (nt-test--mock-tree '(("string1" "note1") ("string2" "note2")))

    (print (nt-tree--line->notes 1))

    (nt-tree-print)

    ))
