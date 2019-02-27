;;; nt-alg.el --- Complex Components of Notate -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>



;;; Commentary:

;; Unlike other modules that organize methods common in purpose or subject, this
;; module's membership is defined by complexity.

;; Code that isn't easy reading is quarantined here.



;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-bounds)
(require 'nt-mask)
(require 'nt-note)
(require 'nt-ov)



;;; Note Deletion
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



(provide 'nt-alg)



;;; nt-alg.el ends here
