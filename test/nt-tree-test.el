;;; nt-tree-test.el --- Note Trees -*- lexical-binding: t -*-

;; ~ Testing Status ~

;; Covered:
;; -

;; Not Covered:
;; -

;;; Querying

(ert-deftest tree:querying:items ()
  (nt-test--with-context 'any
      "
1 (string1 foo
2          bar)
3
4 (string2 foo
5          bar)
"
    (nt-test--mock-tree '(("string1" "note1") ("string2" "note2")))

    ;; (nt-tree-visualize)
    ;; (nt-tree-print)
    ;; (print (nt-tree->list))
    ;; (print (nt-tree->roots))

    ))
