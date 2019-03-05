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
  (-first-item (nt-tree--region->roots pos pos)))

(defun nt-tree--note->root (note)
  "Return root of NOTE."
  (-some-> note overlay-start nt-tree--point->root))

;;;; Parent-Finding

(defun nt-tree--note-is-captured? (note-1 note-2)
  "Is NOTE-1's boundary captured in NOTE-2's boundary? If so, give NOTE-2."
  (when (nt-bounds--captured? (nt-note->bound note-1) (nt-note->bound note-2))
    note-2))

(defun nt-tree--parent-fn (note)
  "Try to get NOTE's smallest parent."
  (-when-let (root (nt-tree--note->root note))
    ;; Note - assumes children are ordered size-ascending
    (or (-first (-partial #'nt-tree--note-is-captured? note)
                (hierarchy-children nt-tree root))
        root)))

;;;; Child-Finding

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

;;; Management

(defun nt-tree--add (&rest notes)
  "Place NOTES into the `nt-tree'."
  (hierarchy-add-trees nt-tree notes
                       #'nt-tree--parent-fn
                       #'nt-tree--child-fn))

(defun nt-tree--sort ()
  (hierarchy-sort nt-tree #'nt-tree--sort-fn))

(provide 'nt-tree)

;;; nt-tree.el ends here
