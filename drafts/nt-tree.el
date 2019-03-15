;;; nt-tree.el --- Note Ov Tracking -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; graveyard of code banished from nt

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)
(require 'nt-note)

;; ;;; Dash-based Implementation Exploring

;; ;; (defun nt-tree--init ()
;; ;;   "(re)Init `nt-tree'."
;; ;;   (setq nt-tree nil))

;; ;; (defun nt-tree--buildup (notes)
;; ;;   "Buildup initial `nt-tree' given NOTES."
;; ;;   (-sort #'nt-note--cmp notes)
;; ;;   ;; Start position decreases within each node then increases
;; ;;   ;; Pretty simple way of determining groups
;; ;;   )

;; ;; (defun nt-tree--note->root (note)
;; ;;   "Return root of NOTE, if it has one."
;; ;;   (-first (-partial #'nt-note--proper-subset-of? note)
;; ;;           (nt-tree->roots)))

;; ;; WORKED EXAMPLE

;; '((7 10)  ; root.child1.child1.child2
;;   ;; start increased & end increased -> same-level next (or new subtree)
;;   (15 20) ; root.child1.child2.child2
;;   ;; start decreased & end increased -> parent next
;;   (5 20)  ; root.child1.child1
;;   ;; same
;;   (2 20)  ; root.child1
;;   ;; start increased & end increased -> same-level next (or new subtree)
;;   (50 60) ; root.child2
;;   ;; start decreased & end increased -> parent next
;;   (1 100) ; root

;;   ;; start increased & end increased -> same level next (or new subtree)
;;   (150 200) ; root2.child1
;;   ;; start decreased & end increased -> parent next
;;   (110 200) ; root2
;;   )

;; ;; Determining between same-level or new subtree:
;; ;; The set of largest non-overlapping intervals define the roots

;; ;; What if  flip the ordering:

;; (setq tree
;;       '((1 100) ; root
;;         ;; [1]
;;         (2 20)  ; root.child1
;;         ;; [11]
;;         (2 4)   ; root.child1.child1
;;         (5 20)  ; root.child1.child2
;;         ;; [111
;;         ;;  112]
;;         (7 10)  ; root.child1.child2.child1
;;         (15 20) ; root.child1.child2.child2
;;         ;; [1121
;;         ;;  1122]
;;         (50 60) ; root.child2
;;         ;; [12]

;;         (110 200) ; root2
;;         ;; [2]
;;         (150 200) ; root2.child1
;;         ;; [21]
;;         ))

;; (setq tree-2
;;       '((1 100) ; root
;;         ;; [1]
;;         ((2 20)  ; root.child1
;;          ;; [11]
;;          ((2 4)   ; root.child1.child1
;;           (5 20)  ; root.child1.child2
;;           ;; [111
;;           ;;  112]
;;           ((7 10)  ; root.child1.child2.child1
;;            (15 20))) ; root.child1.child2.child2
;;          ;; [1121
;;          ;;  1122]
;;          (50 60)) ; root.child2
;;         ;; [12]

;;         (110 200) ; root2
;;         (
;;          ;; [2]
;;          (150 200)) ; root2.child1
;;         ;; [21]
;;         ))
;; (setq tree-3
;;       '((1 100 ; root
;;            ;; [1]
;;            ((2 20  ; root.child1
;;                ;; [11]
;;                ((2 4)   ; root.child1.child1
;;                 (5 20  ; root.child1.child2
;;                    ;; [111
;;                    ;;  112]
;;                    ((7 10)  ; root.child1.child2.child1
;;                     (15 20))))) ; root.child1.child2.child2
;;             ;; [1121
;;             ;;  1122]
;;             (50 60))) ; root.child2
;;         ;; [12]

;;         (110 200 ; root2
;;              (
;;               ;; [2]
;;               (150 200))) ; root2.child1
;;         ;; [21]
;;         ))

;; (defun tree--node-contains? (line node)
;;   "Does NODE contain POS?"
;;   (< (car node) line (cadr node)))

;; (defun tree--node->note (node))

;; (defun tree--node->children (node)
;;   "Get NODE's children, should they exist."
;;   (caddr node))

;; (defun tree--notes-for (tree line &optional notes)
;;   "Return notes contributing to LINE in SUBTREE."
;;   (if-let (node (-first (-partial #'tree--node-contains? line)
;;                         tree))
;;       (let* ((note     (tree--node->note node))
;;              (children (tree--node->children node)))
;;         (tree--notes-containing children line (cons note notes)))
;;     notes))

;; ;; refresh mask at line -> (tree--notes-at line)
;; ;; refresh masks in region -> (tree--notes-in region)

;; (defun tree--delete-note (tree note)
;;   (if-let (nodes (-filter (-partial #'tree--node-contains? line)
;;                           tree))
;;       ))

;; ;; (defun tree--notes-containing (tree region &optional ret)
;; ;;   "Return NODES satisfying PRED."
;; ;;   ;; WRONG at most one node in each level will contain point
;; ;;   (let* ((nodes    (-filter (-partial #'tree--node-contains? pos)
;; ;;                             tree))
;; ;;          (notes    (-map #'tree--node->note     nodes))
;; ;;          (children (-map #'tree--node->children nodes)))

;; ;;     (if nodes
;; ;;         (-map (-cut #'tree--notes-containing <> pos <>)
;; ;;               children
;; ;;               (append ret notes))
;; ;;       ret)))

;; ;; (-tree-seq
;; ;;  #'tree--node-contains?
;; ;;  #'tree--children
;; ;;  tree-3)

;; ;; THOUGHTS:
;; ;; 1. Sort, buildup labels
;; ;; 2. turn into a tree:

;; ;; insertion:
;; ;;   1. insert at sorted start pos
;; ;; traversing:
;; ;;   1. first interval containing region is the root

;; ;; 1
;; ;; 11             12
;; ;; 111 112        xxx
;; ;; xxx 1121 1122  xxx
;; ;; 2
;; ;; 21


;; ;; Region -> check which it lives in -> label got
;; ;; -> check down with that label -> check which it lives in
;; ;; if doesn't live in any -> that label is the answer
;; ;; otherwise add to label and continue forward

;; ;; (-tree-seq branch-fn child-fn tree)
;; (-tree-seq (-not #'vectorp) #'identity tree)
;; (-tree-map-nodes 'v)

;; ;; Example: Inserting (4 40) would never happend due to how indentation works
;; ;; that is, it must be (4 x) x<=20

;; ;; Now traversing is (or stayed same on increase/decrease):
;; ;;  start increased & end decreased -> child
;; ;;  start increased & end increased -> same-level or up-level

;; ;; ALG:
;; ;; 1. Sort as already defined.
;; ;;      wait, due to how indentation works, it might be just sort by start pos...
;; ;;      (actually desc. first by end then ascending start to cover (2 4) case)
;; ;; 2. For node n_i
;; ;; 3a. if n_i+1 has end <= n_i
;; ;;      n_i+1 is a child of n_i
;; ;; 3b. if n_i+1 has end > n_i and start >= n_i
;; ;;      From i..0 let n_j be the first note containing n_i+1
;; ;;      n_j is the parent of n_i
;; ;;      if no such j, then n_j is a root

;; ;; Now we can store a mirror list with the depth of each idx
;; ;; or something els..

;; ;; That seems much simpler

;; (defun nt-tree--testing ()
;;   ;; -tree-seq
;;   (let ((tree2))

;;     (-tree-map-nodes
;;      (-andfn #'nt-ov--note?
;;              )
;;      )

;;     ;; (-tree-seq #'nt-ov--note?
;;     ;;            'identity
;;     ;;            tree)
;;     ;; (-tree-seq (lambda (node) (integerp (car node)))
;;     ;;            'identity
;;     ;;            tree)

;;     ;; tree
;;     ))

;; ;;; Init

;; (defun nt-tree--init ()
;;   "(re)Init `nt-tree'."
;;   (setq nt-tree (hierarchy-new)))

;; (defun nt-tree--buildup (notes)
;;   "Buildup initial `nt-tree' given NOTES."
;;   ;; batch initiation must be handled differently than one-at-a-time!
;;   (nt-tree--add* notes))

;; ;;; Querying
;; ;;;; Primitives

;; (defun nt-tree--note->root (note)
;;   "Return root of NOTE, if it has one."
;;   (-first (-partial #'nt-note--proper-subset-of? note)
;;           (nt-tree->roots)))

;; (defun nt-tree--region->notes (start end)
;;   "Return notes within region [START END)"
;;   (--filter
;;    (overlay-get it 'nt-note?)
;;    (overlays-in start end)))

;; (defun nt-tree--region->roots (start end)
;;   "Return roots covering region [START END)."
;;   (->>
;;    (nt-tree--region->notes start end)
;;    (-map #'nt-tree--note->root)
;;    -non-nil
;;    -distinct))

;; (defun nt-tree--region->notes* (region)
;;   "Apply `nt-tree--region->notes' on REGION."
;;   (apply #'nt-tree--region->notes region))

;; (defun nt-tree--region->roots* (region)
;;   "Apply `nt-tree--region->roots' on REGION."
;;   (apply #'nt-tree--region->roots region))

;; ;;;; Top-level

;; (defun nt-tree->list ()
;;   "Return list of managed notes by `nt-tree'."
;;   (hierarchy-items nt-tree))

;; (defun nt-tree->roots ()
;;   "Return roots of `nt-tree'."
;;   (hierarchy-roots nt-tree))

;; (defun nt-tree->string ()
;;   "Convert hierarchy `nt-tree' to a string."
;;   (nt-tree--format))  ; NOTE will be later just: (hierarchy-to-string nt-tree)

;; (defun nt-tree->leafs (&optional note)
;;   "Return the smallest-boundary notes, optionally restricted to NOTE's subtree."
;;   (hierarchy-leafs nt-tree note))

;; ;;;; Notes

;; (defun nt-tree--contains? (note)
;;   "Is NOTE contained in `nt-tree'?"
;;   (hierarchy-has-item nt-tree note))

;; (defun nt-tree--note->parent (note)
;;   "Return parent of NOTE, if it has a parent."
;;   (hierarchy-parent nt-tree note))

;; ;;;; Regions

;; (defun nt-tree--point->root (pos)
;;   "Return root containing POS"
;;   (-first-item (nt-tree--region->roots pos (1+ pos))))

;; (defun nt-tree--line->notes (line)
;;   "Return notes on LINE."
;;   (nt-tree--region->notes* (nt-line->region line)))

;; (defun nt-tree--line->roots (line)
;;   "Return roots on LINE."
;;   (nt-tree--region->roots* (nt-line->region line)))

;; (defun nt-tree--lines->notes (start-line end-line)
;;   "Return notes within lines [START-LINE END-LINE)."
;;   (nt-tree--region->notes* (nt-lines->region start-line end-line)))

;; (defun nt-tree--lines->roots (start-line end-line)
;;   "Return roots within lines [START-LINE END-LINE)."
;;   (nt-tree--region->roots* (nt-lines->region start-line end-line)))

;; ;;;; Relationships

;; (defun nt-tree--parent-fn (note)
;;   "Get NOTE's smallest parent, or nil if NOTE should be a root."
;;   (print note)
;;   (nt-tree--note->root note))

;; ;;; Formatters

;; (defun nt-tree--label-fn (note indent)
;;   "Format label for NOTE at INDENT level for hierarchy display representations."
;;   (-> indent (s-repeat " ") (s-join note)))

;; (defun nt-tree--print ()
;;   "Print hierarchy `nt-tree' according to `nt-tree->string'."
;;   (print (nt-tree->string)))

;; (defun nt-tree--visualize (&optional table?)
;;   "Visualize `nt-tree' via `hierarchy-tree-display' and friends."
;;   (if table?
;;       (hierarchy-tabulated-display nt-tree #'nt-tree--label-fn)
;;     (hierarchy-tree-display nt-tree #'nt-tree--label-fn)))

;; ;;; Mutations

;; (defun nt-tree--add (note)
;;   "Place NOTE into the `nt-tree'."
;;   (hierarchy-add-tree nt-tree note
;;                       #'nt-tree--parent-fn
;;                       ;; #'nt-tree--child-fn
;;                       ))

;; (defun nt-tree--add* (notes)
;;   "Place NOTES into the `nt-tree'."
;;   (hierarchy-add-trees nt-tree notes
;;                        #'nt-tree--parent-fn
;;                        ;; #'nt-tree--child-fn
;;                        ))

;; (defun nt-tree--sort ()
;;   "Sort `nt-tree' according to `nt-tree--note<' comparison fn."
;;   (hierarchy-sort nt-tree #'nt-note--cmp))

;; ;;; Development Utilities

;; (defun nt-tree--format ()
;;   "Format `nt-tree' for pprint (will be replaced with visualizers later)."
;;   (let* ((items (nt-tree->list))
;;          (roots (nt-tree->roots))
;;          (leafs (nt-tree->leafs)))
;;     (format "Tree:
;; ~~
;; items:
;; %s
;; ~~
;; roots:
;; %s
;; ~~
;; leafs:
;; %s
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~
;; "
;;             items roots leafs)))

;; ;;; Provide

;; (provide 'nt-tree)

;; ;;; nt-tree.el ends here

;;; Comparisons

;; (defun nt-note--proper-subset-of? (self other)
;;   "Is NOTE's boundary properly captured in another NOTE's boundary?"
;;   (-let (((a1 b1) (nt-note->bound self))
;;          ((a2 b2) (nt-note->bound other)))
;;     (and (< a2 a1)
;;          (< b1 b2))))

;; (defun nt-note--start< (self other)
;;   "Is NOTE's starting position < another NOTE's start positions?"
;;   ;; Shorthand for:
;;   ;;
;;   (-let (((a1 _) (nt-note->bound self))
;;          ((a2 _) (nt-note->bound other)))
;;     (< a1 a2)))

;; (defun nt-note--start> (self other)
;;   "Is NOTE's starting position > another NOTE's start positions?"
;;   (-let (((a1 _) (nt-note->bound self))
;;          ((a2 _) (nt-note->bound other)))
;;     (> a1 a2)))

;; (defun nt-note--cmp (self other)
;;   "Compare NOTE-1 < NOTE-2. See `nt-tree' for data structure."
;;   (cond ((nt-note--proper-subset-of? self other))
;;         ((nt-note--start<            self other))))

;;; roots

;; (defun nt-notes->roots-1 (notes root roots)
;;   "Internal, see `nt-notes->roots'."
;;   (-let* (((_ root-end)
;;            (nt-note->bound root))
;;           (next
;;            (-drop-while (-compose (-partial #'> root-end)
;;                                   #'car
;;                                   #'nt-note->bound)
;;                         notes)))
;;     (nt-notes->roots next (cons root roots))))

;; (-let (((root . rest) notes))
;;   (cond (rest (nt-notes->roots-1 rest root roots))
;;         (root (nt-notes->roots rest (cons root roots)))
;;         ((reverse roots))))


;; (defvar-local nt-tree nil
;;   "Manage note overlays in a `hierarchy' tree, ordered on interval-containment.

;; ---
;; Roots are non-overlapping line-intervals with P-C relationship defined as:
;;   - A note n_p is a parent of note n_c iff bound(n_p) contains bound(n_c).
;;   - If bound(n_p) == bound(n_c), the first note by buffer position is the parent.

;; ---
;; Notes are sorted as follows (see the cmp fn `nt-tree--note<'):
;;   - A child is smaller than its parents.
;;   - Comparing two separate subtrees: the first occurring note is smaller.

;; ---
;; So the ordering looks like:
;;   - Sort roots by buffer position.
;;   - Insert before each root its children ordered by increasing size.

;; ---
;; There are better tree structures we can use, which will be
;; explored nearing project completion.")
