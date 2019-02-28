;;; nt-alg.el --- Complex Components of Notate -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Unlike other modules that organize methods common in purpose or subject, this
;; module's membership is defined by complexity.

;; Code that isn't easy reading is quarantined here.

;; NOW A ROUGH DRAFT SECTION FOR MORE INVOLVED UPDATES IM EXPLORING



;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)



;;; Note Deletion (list-based)
;;;; Commentary

;; Batch note deletion without recalculating bounds:

;; ROUGH DRAFT:

;; Given notes n_i ordered descending by indent, let m_i be the mask at line(n_i)
;; with the mask's notes denoted n_m_i.

;; Intersect n_m_0 with each n_m_1.. Add n_0 and call it note-chain C_0 maintaining order.
;; Repeat above for next note not contained in C_0 and call it C_1.
;; Repeat until each n_i is a member of some chain.

;; For each chain C_i, let l_i be the line of C_i[0] and then:
;; 1. Delete notes in C_i
;; 2. Goto line 1+l_i
;; 3. Remove any deleted notes from mask at line
;; 4. Forward-line and repeat step 3 until mask at line has no deleted notes

;;;; Implementation (Rough Draft)

;; None of below is tested yet not necessarily correct.
;; Starting point for figuring out batch deletion.

;; notes = n_i,         note = n_0
;; masks-notes = n_m_i, mask-notes = n_m_0
;; chains = C,          chain = C_0

(defun nt-alg--construct-chains-1 (notes notes-masks &optional chains)
  (-if-let* (((note notes-rest)
              notes)
             ((mask-notes masks-notes-rest)
              masks-notes)
             (chain
              (-filter (-partial #'-intersection mask-notes)
                       mask-notes-rest)))
      (-if-let (next_i
                (-find-index (-partial (-not #'-contains?) chain)
                             notes-rest))
          (nt-alg--construct-chains-1
           (-slice notes next_i)
           (-slice masks-notes next_i)
           (cons chain chains))
        chains)
    chains))

(defun nt-alg--construct-chains (notes)
  "Delete NOTES and refresh the masks they contributed to."
  (let* ((notes (-sort (-on #'>= #'nt-note-indent)
                       notes))
         (masks (-map (-compose #'nt-mask--at
                                #'nt-note->line)
                      notes))
         (masks-notes (-map #'nt-mask->notes
                            masks)))
    (nt-alg--construct-chains-1 notes masks-notes)))

(defun nt-alg--clear-masks ()
  (let ((mask (nt-mask--at (line-number-at-pos)))
        (notes (nt-mask->notes mask))
        (continue? (-any? #'nt-ov--deleted? notes)))
    (when continue?
      (nt-ov--clear-deleted ov)
      (forward-line)
      (nt-alg--clear-masks))))

(defun nt-alg--delete-chain (chain)
  (save-excursion
    (nt-ov--goto (-first-item chain))
    (-each #'nt-note--delete chain)
    (nt-alg--clear-masks)))

(defun nt-alg--delete-notes (notes)
  (->> notes
     nt-alg--construct-chains
     (-each #'nt-alg--delete-chain)))

;; Is it *really* worth all this complexity for "efficient" batch deletion?
;; Have to think through this deeper



;;; Note Deletion (alist based)

;; These are non-overlapping by definition
;; (a_0 b_0) (1 2)
;; (a_1 b_1) (5 10)

;; new note: (6 7)
;; a_1=5<=6
;; b_1=10>=7

;; new note: (12 15)
;; a_1=5<=12
;; b_1=10<15

;; new note: (3 10)
;; Still must check if there is another interval afterwards

;; new note: (3 7)
;; should never happen, things mustve gotten desynced somewhere

;; nt-notes-alist has key=bound with val=notes-in-bound

(defun nt-init-note ()
  (let ((start
         end))
    (-when-let* ((notes-idx (-find-index (and (<= a_1 start)
                                              (>= b_1 start))))
                 (notes (nth notes-idx notes-alist)))
      (when (<= (1+ notes-idxs a) end)
        (setcar notes '(start end)))
      (push note (cdr notes)))))

;; Another thought, is it useful to store all bounds or just the maxmost bound?

;;; Note Deletion (hierarchy based)

;; make tree
(setq nt-notes (hierarchy-new))
(hierarchy-add-tree
 nt-notes=hierarchy
 item=(bound mask)
 parent-fn=is-subset+car
 children-fn=is-superset+car
 )

;; sort by bounds
(hierarchy-sort
 nt-notes
 sortfn=less-than+car+car (< a_0 a_1))

;; return regions of text with notes present
(hierarchy-roots
 nt-notes
 )

;; Get all notes modifying a region
(hierarchy-children
 nt-notes
 root
 )
;; https://github.com/DamienCassou/hierarchy



(provide 'nt-alg)



;;; nt-alg.el ends here
