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
1 (string1 string2 string1
2          bar)
3
4 (string2 foo
5          bar)
"
    (-let (((note1 note2 note3 note4)
            (nt-test--mock-tree '(("string1" "note1") ("string2" "note2")))))
      ;;;;
      ;; Debug
      ;;;;
      (nt-tree-print)
      ;; (message "foo %s" (nt-tree->roots))

      ;;;;
      ;; Containment
      ;;;;
      (should* (nt-tree--contains? note1)
               (nt-tree--contains? note2)
               (nt-tree--contains? note3))

      ;;;;
      ;; Root Finding (note-based)
      ;;;;
      ;; (should* (eq note1 (nt-tree--note->root note1))
      ;;          (eq note1 (nt-tree--note->root note2))
      ;;          (eq note1 (nt-tree--note->root note3)))

      ;;;;
      ;; Parent Finding (note-based)
      ;;;;
      ;; (should-not (nt-tree--note->parent note1))

      ;; (should* (null (nt-tree--note->root note1))

      ;;          (eq note1 (nt-tree--note->parent note2))
      ;;          (eq note2 (nt-tree--note->parent note3)))
      )))
