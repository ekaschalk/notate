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

    (-let* ((line 1)
            (notes (nt-tree--line->notes line))
            (sorted-notes (-sort #'nt-tree--note< notes))
            ((child root) sorted-notes))
      (message "notes %s" notes)
      (message "sorted %s" sorted-notes)
      (message "roots %s" root)
      (message "child %s" child)
      )

    (print (nt-tree--line->notes 1))
    (print (nt-tree--line->roots 1))

    (nt-tree-print)

    ))
