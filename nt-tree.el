;;; nt-tree.el --- Note Ov Tracking -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Module will replace current `nt-tree.el' list-based note management with
;; `hierarchy' tree-based management https://github.com/DamienCassou/hierarchy

;; See `nt-tree' for how the tree structure is defined.

;;; Code:
;;;; Requires

(require 'nt-base)

;;; Init

(defun nt-tree--init ()
  "(re)Init `nt-tree'."
  (setq nt-tree (hierarchy-new)))

;;; Querying
;;;; Top-level

(defun nt-tree->list ()
  "Return list of managed notes by `nt-tree'."
  (hierarchy-items nt-tree))

(defun nt-tree->roots ()
  "Return roots of `nt-tree'."
  (hierarchy-roots nt-tree))

(defun nt-tree->string ()
  "Convert hierarchy `nt-tree' to a string."
  ;; TODO Below should be used when I have it working
  ;; (hierarchy-to-string nt-tree)
  (nt-tree--format))

(defun nt-tree->leafs (&optional note)
  "Return the smallest-boundary notes, optionally restricted to NOTE's subtree."
  (hierarchy-leafs nt-tree note))

;;;; Notes

(defun nt-tree--contains? (note)
  "Is NOTE contained in `nt-tree'?"
  (hierarchy-has-item nt-tree note))

(defun nt-tree--note->parent (note)
  "Return parent of NOTE, possibly being itself."
  (hierarchy-parent nt-tree note))

(defun nt-tree--note->root (note)
  "Return root of NOTE, possibly being itself."
  ;; Alternatively could do the natural choice of visiting parents
  (-first (-partial #'nt-tree--note-is-subset? note)
          (nt-tree->roots)))

;;;; Regions

(defun nt-tree--region->notes (start end)
  "Return notes within region [START END)"
  (-filter (-cut #'overlay-get <> 'nt-note)
           (overlays-in start end)))

(defun nt-tree--region->roots (start end)
  "Return roots covering region [START END)."
  (-map #'nt-tree--note->root
        (nt-tree--region->notes start end)))

(defun nt-tree--point->root (pos)
  "Return root containing POS"
  (-first-item (nt-tree--region->roots pos (1+ pos))))

;;;; Visualizations

(defun nt-tree--label-fn (note indent)
  "Format label for NOTE at INDENT level for hierarchy display representations."
  (-> indent (s-repeat " ") (s-join note)))

(defun nt-tree-print ()
  "Print hierarchy `nt-tree'."
  (print (nt-tree->string)))

(defun nt-tree-visualize (&optional table?)
  "Visualize `nt-tree' via `hierarchy-tree-display' and friends."
  (if table?
      (hierarchy-tabulated-display nt-tree #'nt-tree--label-fn)
    (hierarchy-tree-display nt-tree #'nt-tree--label-fn)))

;;; Relationships
;;;; Comparisons

(defun nt-tree--note-is-subset? (note-1 note-2)
  "Is NOTE-1's boundary captured in NOTE-2's boundary? Non-strict."
  (-let (((a1 b1) (nt-note->bound note-1))
         ((a2 b2) (nt-note->bound note-2)))
    (and (<= a2 a1)
         (<= b1 b2))))

(defun nt-tree--note-start< (note-1 note-2)
  "Compare NOTE-1's start and NOTE-2's start positions. Non-strict."
  (-let (((a1 _) (nt-note->bound note-1))
         ((a2 _) (nt-note->bound note-2)))
    (<= a2 a1)))

(defun nt-tree--note< (note-1 note-2)
  "Compare NOTE-1 and NOTE-2. Non-strict. See `nt-tree' for cmp rules."
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

(defun nt-tree--add (note)
  "Place NOTE into the `nt-tree'."
  (hierarchy-add-tree nt-tree note
                      #'nt-tree--parent-fn
                      ;; #'nt-tree--child-fn
                      ))

(defun nt-tree--add* (notes)
  "Place NOTES into the `nt-tree'."
  (hierarchy-add-trees nt-tree notes
                       #'nt-tree--parent-fn
                       ;; #'nt-tree--child-fn
                       ))

(defun nt-tree--sort ()
  "Sort `nt-tree' according to `nt-tree--note<' comparison fn."
  (hierarchy-sort nt-tree #'nt-tree--note<))

;;; Development Utilities

(defun nt-tree--format ()
  "Format `nt-tree' for pprint (will be replaced with visualizers later)."
  (let* ((items (nt-tree->list))
         (roots (nt-tree->roots))
         (leafs (nt-tree->leafs)))
    (format "Tree:
~~
items:
%s
~~
roots:
%s
~~
leafs:
%s
~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~
"
            items roots leafs)))

;;; Provide

(provide 'nt-tree)

;;; nt-tree.el ends here
