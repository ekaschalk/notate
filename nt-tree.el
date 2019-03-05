;;; nt-tree.el --- Note Nodes -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Module will replace current `nt-tree.el' list-based note management with
;; `hierarchy' tree-based management.

;; The intent that it will be easier and faster to write methods operating on
;; regions of batches of notes.

;; See https://github.com/DamienCassou/hierarchy

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)

;;; Configuration
;;;; Managed

(defconst nt-tree (hierarchy-new)
  "Manage NOTE nodes in a `hierarchy' tree.

Roots are non-overlapping line-intervals with P-C relationship defined as:
  A note n_c is a child of note n_p if masks(n_p) contains masks(n_c).

Notes are sorted as follows:
- n_1 < n_2 if n_1 is a child of n_2
- otherwise, n_1 < n_2 if start(n_1) < start(n_2)

So notes aren't line-ordered. Instead:
- Sort roots by buffer position.
- Insert before each root its children ordered by increasing size.")

;;; Aliases

(defun nt-tree->list ()
  "Return list of managed notes by `nt-tree'."
  (hierarchy-items nt-tree))

(defun nt-tree->roots ()
  "Return roots of `nt-tree'."
  (hierarchy-roots nt-tree))

;;; Hierarchy Management

(defun nt-tree--add (note)
  "Place NOTE into the `nt-tree'."
  (hierarchy-add-tree nt-tree note
                      #'nt-tree--parent-fn
                      #'nt-tree--child-fn))

(defun nt-tree--root-for (pos)
  "Get root containing POS, if there is one."
  (-first (-partial #'nt-bounds--pos-captured? pos)
          (nt-tree->roots)))

(defun nt-tree--root-fn (note)
  "Try to get NOTE's largest parent, its root."
  (-first (-partial #'nt-tree--captured? note)
          (nt-tree->roots)))

(defun nt-tree--parent-fn (note)
  "Try to get NOTE's parent."
  (-when-let (root (nt-tree--root-fn note))
    (-if-let (candidates (hierarchy-children nt-tree root))
        ;; Check if first-item (the smallest bound) contains NOTE
        ;; otherwise check next-smallest bound
        ;; If no candidate is acceptable, return root.
        (-first-item candidates)
      root)))

(defun nt-tree--child-fn (note)
  "Try to get NOTE's children."
  (-filter (-when-let (parent (nt-tree--captured? note it)) parent)
           (nt-tree->roots)))

;;; Sorting

(defun nt-tree--captured? (note-1 note-2)
  "Is NOTE-1's boundary captured in NOTE-2's boundary? If so, give NOTE-2."
  (when (nt-bounds--captured? (nt-note->bound note-1) (nt-note->bound note-2))
    note-2))

(defun nt-tree--cmp-uncaptured (note-1 note-2)
  "Return non-nil if NOTE-1 comes before NOTE-2."
  (when (nt-bound--cmp-uncaptured (nt-note->bound note-1) (nt-note->bound note-2))
    note-2))

(defun nt-tree--sort-fn (note-1 note-2)
  "Return non-nil if NOTE-1 is captured in NOTE-2, else compare a_1 <= a_2."
  ;; NOTE go over whether strict equality or not
  (cond (nt-tree--captured? note-1 note-2)
        (nt-tree--cmp-uncaptured note-1 note-2)))

(provide 'nt-tree)

;;; nt-tree.el ends here
