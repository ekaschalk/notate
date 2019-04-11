;;; nt-vis.el --- Visual Columns -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Implements concept of "visual columns" - the column taking into account
;; the width of the /displayed/ characters rather than the true characters.

;; Enables "visual column movement" as a solution to usability issues incurred
;; when true column numbers become unreliable.

;;; Code:
;;;; Requires

(require 'nt-base)

(require 'nt-ov)

;;; Configuration
;;;; Managed

(defvar nt-vis--temporary-goal-column nil
  "A visual-column version of `temporary-goal-column' for masking line mvmnt.

It is implemented in terms of `temporary-goal-column' so it doesn't share
semantics quite the same. Used for tracking goal columns when moving lines
interacts with EOLs.")

;;; Fundamentals

;; TODO Why is it possible to move one char past true column end
;; if the display is shrunk?

(defun nt-vis--pos->vis-col (pos)
  "Get visual column of POS.

No relatation to `visual-line-mode', truncation, etc.

The visual column is the column taking into account 'display overlays.
vis-colum  <= true-column as long as indentation expansions are not allowed."
  (let* ((col (nt-pos->col pos))
         (ovs (nt-ovs<-region (line-beginning-position) pos))
         (hidden-chars (nt-ovs->width ovs)))
    (- col hidden-chars)))

;; TODO This is inefficient, write a smarter solution
(defun nt-vis--goto-vis-col (vis-col)
  "Goto VIS-COL of current line, or line's end if VIS-COL is out of range.

Return the visual column reached."
  (let ((vis-col-limit (nt-vis--pos->vis-col (line-end-position))))
    (if (or (not vis-col-limit)
            (>= vis-col vis-col-limit))  ; Fast-track EOL handling
        (goto-char (line-end-position))

      (goto-char (line-beginning-position))
      (while (< (nt-vis--pos->vis-col (point)) vis-col)
        (forward-char))))
  (nt-vis--pos->vis-col (point)))

;;; Line Movement

(defun nt-vis--mask-line-movement (line-move-fn &rest args)
  "Vis-col based version of line movement masking."
  (-let* (((line-count) args)
          (col (current-column))
          (start-line (line-number-at-pos))
          (end-line (+ start-line line-count))

          (tracking-eol?
           (= most-positive-fixnum temporary-goal-column))
          (vis-col
           (nt-vis--pos->vis-col (point)))
          (goal-vis-col
           (and (not tracking-eol?)
                (or nt-vis--temporary-goal-column vis-col))))
    (apply line-move-fn args)

    (setq goal-vis-col
          (- temporary-goal-column
             (- (nt-vis--pos->vis-col
                 (+ (beginning-of-line) temporary-goal-column))
                vis-col)))

    (let ((vis-col-after-line-move (nt-vis--goto-vis-col goal-vis-col)))
      (when (and vis-col-after-line-move
                 (= vis-col-after-line-move goal-vis-col))
        (setq goal-vis-col nil)
        (setq temporary-goal-column (current-column))))

    (setq nt-vis--temporary-goal-column goal-vis-col)))

;;; Advice

(defun nt-vis--advice-add-line-movement ()
  (setq nt-vis--temporary-goal-column nil)  ; just-in-case

  (advice-add #'next-line :around #'nt-vis--mask-line-movement)
  (advice-add #'previous-line :around #'nt-vis--mask-line-movement))

(defun nt-vis--advice-remove-line-movement ()
  (setq nt-vis--temporary-goal-column nil)  ; just-in-case

  (advice-remove #'next-line #'nt-vis--mask-line-movement)
  (advice-remove #'previous-line #'nt-vis--mask-line-movement))

;;; Notes

;; Bugs:

;; 1. the space before baz -> go down -> go back up should be same spot, instead
;; it is one before but this doesnt occur if point is one further on the "b"

;; goal-vis-col should be the difference of widths of the two lines

;; temporary-goal-column -> temporary-vis-column

;;; Provide

(provide 'nt-vis)

;;; nt-vis.el ends here
