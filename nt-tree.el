;;; nt-tree.el --- Note Nodes -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Module will replace current `nt-tree.el' list-based note management with
;; `hierarchy' tree-based management.

;; See `nt-tree' for how the tree structure is defined.

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
  "Manage notes in a `hierarchy' tree based on interval-containment.

---

Roots are non-overlapping line-intervals with P-C relationship defined as:

  - A note n_c is a child of note n_p iff masks(n_p) contains masks(n_c).

  - If the masks are equal, then the first note by position is defined
    to be the parent.

---

Notes are sorted as follows:

  - n_1 < n_2 if n_1 is a child of n_2

  - otherwise, n_1 < n_2 if start(n_1) < start(n_2)

So the ordering looks like:

  - Sort roots by buffer position.

  - Insert before each root its children ordered by increasing size.")

;;; Aliases

(defun nt-tree->list ()
  "Return list of managed notes by `nt-tree'."
  (hierarchy-items nt-tree))

(defun nt-tree->roots ()
  "Return roots of `nt-tree'."
  (hierarchy-roots nt-tree))

;;; Relationships
;;;; Root-Finding

(defun nt-tree--region->roots (start end)
  "Return roots covering region [START END)."
  (let* ((roots
          (nt-tree->roots))
         (start-idx
          (-find-index (-lambda ((a _) it)
                         (<= start a))
                       roots))
         (end-idx
          (-find-last-index (-lambda ((_ b) it)
                              (<= b end))
                            roots)))
    (-slice roots
            (or start-idx 0)
            (or end-idx (length roots)))))

(defun nt-tree--point->root (pos)
  "Return root containing POS"
  (-first-item (nt-tree--region->roots pos (1+ pos))))

(defun nt-tree--note->root (note)
  "Return root of NOTE, possibly being itself."
  (-> note overlay-start nt-tree--point->root))

;;;; Comparisons

(defun nt-tree--note-is-subset? (note-1 note-2)
  "Is NOTE-1's boundary captured in NOTE-2's boundary?"
  (-let (((a1 b1) (nt-note->bound note-1))
         ((a2 b2) (nt-note->bound note-2)))
    (and (<= a2 a1)
         (<= b1 b2))))

(defun nt-tree--note-start< (note-1 note-2)
  "Compare NOTE-1's start and NOTE-2's start positions."
  (-let (((a1 _) (nt-note->bound note-1))
         ((a2 _) (nt-note->bound note-2)))
    (<= a2 a1)))

(defun nt-tree--note< (note-1 note-2)
  "Compare NOTE-1 and NOTE-2."
  (cond (nt-tree--note-is-subset? note-1 note-2)
        (nt-tree--note-start<     note-1 note-2)))

;;;; Parent-Finding

(defun nt-tree--parent-fn (note)
  "Try to get NOTE's smallest parent."
  ;; Note the -first call assumes children are ordered size-ascending
  (-when-let (root (nt-tree--note->root note))
    (or (-first (-partial #'nt-tree--note-is-subset? note)
                (hierarchy-children nt-tree root))
        root)))

;;; Mutations

(defun nt-tree--add (&rest notes)
  "Place NOTES into the `nt-tree'."
  (hierarchy-add-trees nt-tree notes
                       #'nt-tree--parent-fn
                       #'nt-tree--child-fn))

(defun nt-tree--sort ()
  "Sort `nt-tree' accordding to `nt-tree--note<' comparison fn."
  (hierarchy-sort nt-tree #'nt-tree--note<))

;;; Provide

(provide 'nt-tree)

;;; nt-tree.el ends here
