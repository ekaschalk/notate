;;; nt-tree.el --- Note Nodes -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Module will replace current `nt-tree.el' list-based note management with
;; `hierarchy' tree-based management.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)

;;; Hierarchy

(defconst nt-tree-hierarchy (hierarchy-new)
  "Manage NOTE nodes in a `hierarchy'.

Roots are non-overlapping line-intervals with P-C relationship defined as:
  A note n_c is a child of note n_p if masks(n_p) contains masks(n_c).")

(defun nt-tree--roots ()
  "Return roots of `nt-tree-hierarchy'."
  (hierarchy-roots nt-tree-hierarchy))

(defun nt-tree->bound (note)
  "Return NOTE's bound."
  (overlay-get note 'nt-bound))

(defun nt-bound--captured? (bound-1 bound-2)
  "Is BOUND-1 captured in BOUND-2?"
  (-let (((a_1 b_1) bound-1)
         ((a_2 b_2) bound-2))
    (and (<= a_2 a_1)
         (<= b_1 b_2))))

(defun nt-bound--pos-captured? (pos bound)
  "Is POS captured in BOUND?"
  (-let ((a b) bound)
    (and (<= pos a)
         (< pos b))))

(defun nt-bound--cmp-uncaptured (bound-1 bound-2)
  "Return non-nil if BOUND-1 starts before BOUND-2."
  (-let (((a_1 _) bound-1)
         ((a_2 _) bound-2))
    (<= a_2 a_1)))

(defun nt-tree--captured? (note-1 note-2)
  "Is NOTE-1's boundary captured in NOTE-2's boundary? If so, give NOTE-2."
  (when (nt-bound--captured? (nt-tree->bound note-1) (nt-tree->bound note-2))
    note-2))

(defun nt-tree--cmp-uncaptured (note-1 note-2)
  "Return non-nil if NOTE-1 comes before NOTE-2."
  (when (nt-bound--cmp-uncaptured (nt-tree->bound note-1) (nt-tree->bound note-2))
    note-2))

(defun nt-tree--root-for (pos)
  "Get root containing POS, if there is one."
  (-first (-partial #'nt-bound--pos-captured? pos)
          (nt-tree--roots)))

(defun nt-tree--root-fn (note)
  "Try to get NOTE's largest parent, its root."
  (-first (-partial #'nt-tree--captured? note)
          (nt-tree--roots)))

(defun nt-tree--parent-fn (note)
  "Try to get NOTE's parent."
  (-when-let (root (nt-tree--root-fn note))
    (-if-let (candidates (hierarchy-children nt-tree-hierarchy root))
        ;; TODO -first-item assumes a sorting
        (-first-item candidates)
      root)))

(defun nt-tree--child-fn (note)
  "Try to get NOTE's children."
  (-filter (-when-let (parent (nt-tree--captured? note it)) parent)
           (nt-tree--roots)))

(defun nt-tree--sort-fn (note-1 note-2)
  "Return non-nil if NOTE-1 is captured in NOTE-2."
  (cond (nt-tree--captured? note-1 note-2)
        (nt-tree--cmp-uncaptured note-1 note-2)))

(defun nt-tree--add (note)
  "Place NOTE into the `nt-tree-hierarchy'."
  (hierarchy-add-tree nt-tree-hierarchy note
                      #'nt-tree--parent-fn
                      #'nt-tree--child-fn))

(defun nt-tree ()
  "Return list of managed notes by `nt-tree-hierarchy'."
  (hierarchy-items nt-tree-hierarchy))

;; return regions of text with notes present
;; (hierarchy-roots
;;  nt-tree
;;  )
;; https://github.com/DamienCassou/hierarchy


(provide 'nt-tree)

;;; nt-tree.el ends here
