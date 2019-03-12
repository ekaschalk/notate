;;; nt-tree.el --- Note Ov Tracking -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Module will replace current `nt-tree.el' list-based note management with
;; `hierarchy' tree-based management https://github.com/DamienCassou/hierarchy

;; NOTE above will not work, actually going to use dash's -tree methods and
;; implement custom.

;; See `nt-tree' for how the tree structure is defined.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)
(require 'nt-note)

;;; Dash-based Implementation Exploring

(defun nt-tree--init ()
  "(re)Init `nt-tree'."
  (setq nt-tree nil))

(defun nt-tree--buildup (notes)
  "Buildup initial `nt-tree' given NOTES."
  (-sort #'nt-note--cmp notes)
  ;; Start position decreases within each node then increases
  ;; Pretty simple way of determining groups

  )

(defun nt-tree--groupby (notes)
  ;; -tree-seq
  )

(defun nt-tree--note->root (note)
  "Return root of NOTE, if it has one."
  (-first (-partial #'nt-note--proper-subset-of? note)
          (nt-tree->roots)))

;;; Init

(defun nt-tree--init ()
  "(re)Init `nt-tree'."
  (setq nt-tree (hierarchy-new)))

(defun nt-tree--buildup (notes)
  "Buildup initial `nt-tree' given NOTES."
  ;; batch initiation must be handled differently than one-at-a-time!
  (nt-tree--add* notes))

;;; Querying
;;;; Primitives

(defun nt-tree--note->root (note)
  "Return root of NOTE, if it has one."
  (-first (-partial #'nt-note--proper-subset-of? note)
          (nt-tree->roots)))

(defun nt-tree--region->notes (start end)
  "Return notes within region [START END)"
  (--filter
   (overlay-get it 'nt-note?)
   (overlays-in start end)))

(defun nt-tree--region->roots (start end)
  "Return roots covering region [START END)."
  (->>
   (nt-tree--region->notes start end)
   (-map #'nt-tree--note->root)
   -non-nil
   -distinct))

(defun nt-tree--region->notes* (region)
  "Apply `nt-tree--region->notes' on REGION."
  (apply #'nt-tree--region->notes region))

(defun nt-tree--region->roots* (region)
  "Apply `nt-tree--region->roots' on REGION."
  (apply #'nt-tree--region->roots region))

;;;; Top-level

(defun nt-tree->list ()
  "Return list of managed notes by `nt-tree'."
  (hierarchy-items nt-tree))

(defun nt-tree->roots ()
  "Return roots of `nt-tree'."
  (hierarchy-roots nt-tree))

(defun nt-tree->string ()
  "Convert hierarchy `nt-tree' to a string."
  (nt-tree--format))  ; NOTE will be later just: (hierarchy-to-string nt-tree)

(defun nt-tree->leafs (&optional note)
  "Return the smallest-boundary notes, optionally restricted to NOTE's subtree."
  (hierarchy-leafs nt-tree note))

;;;; Notes

(defun nt-tree--contains? (note)
  "Is NOTE contained in `nt-tree'?"
  (hierarchy-has-item nt-tree note))

(defun nt-tree--note->parent (note)
  "Return parent of NOTE, if it has a parent."
  (hierarchy-parent nt-tree note))

;;;; Regions

(defun nt-tree--point->root (pos)
  "Return root containing POS"
  (-first-item (nt-tree--region->roots pos (1+ pos))))

(defun nt-tree--line->notes (line)
  "Return notes on LINE."
  (nt-tree--region->notes* (nt-base--line-bounds line)))

(defun nt-tree--line->roots (line)
  "Return roots on LINE."
  (nt-tree--region->roots* (nt-base--line-bounds line)))

(defun nt-tree--lines->notes (start-line end-line)
  "Return notes within lines [START-LINE END-LINE)."
  (nt-tree--region->notes* (nt-base--lines-bounds start-line end-line)))

(defun nt-tree--lines->roots (start-line end-line)
  "Return roots within lines [START-LINE END-LINE)."
  (nt-tree--region->roots* (nt-base--lines-bounds start-line end-line)))

;;;; Relationships

(defun nt-tree--parent-fn (note)
  "Get NOTE's smallest parent, or nil if NOTE should be a root."
  (print note)
  (nt-tree--note->root note))

;;; Formatters

(defun nt-tree--label-fn (note indent)
  "Format label for NOTE at INDENT level for hierarchy display representations."
  (-> indent (s-repeat " ") (s-join note)))

(defun nt-tree--print ()
  "Print hierarchy `nt-tree' according to `nt-tree->string'."
  (print (nt-tree->string)))

(defun nt-tree--visualize (&optional table?)
  "Visualize `nt-tree' via `hierarchy-tree-display' and friends."
  (if table?
      (hierarchy-tabulated-display nt-tree #'nt-tree--label-fn)
    (hierarchy-tree-display nt-tree #'nt-tree--label-fn)))

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
  (hierarchy-sort nt-tree #'nt-note--cmp))

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
