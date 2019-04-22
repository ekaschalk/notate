;;; nt-base.el --- Common requires and utilities -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Collect pkg-wide imports and utils.

;; Members of this file are not required to be prefixed with `nt-base--' as other
;; files are (by convention).

;; Right now provides methods, macros, and others operating on LINES, REGIONS,
;; and POSITIONS.

;;; Code:
;;;; Requires

(require 'cl)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'smartparens)

;;; Lines
;;;; Transforms
;;;;; Positions

(defun nt-line->start (line)
  "Get start pos of LINE."
  (save-excursion (nt-line--goto line) (line-beginning-position)))

(defun nt-line->end (line)
  "Get end pos of LINE."
  (save-excursion (nt-line--goto line) (line-end-position)))

(defun nt-line->region (line)
  "Get start and end points of LINE."
  (save-excursion
    (nt-line--goto line)
    (list (line-beginning-position)
          (line-end-position))))

(defun nt-lines->region (start-line end-line)
  "Get start and end points of [START-LINE END-LINE)."
  (save-excursion
    (nt-line--goto start-line)
    (list (line-beginning-position)
          (progn (nt-line--goto (1- end-line))
                 (line-end-position)))))

(defun nt-lines<-region (start end)
  "Get start-line and end-line containing region in START and END."
  (unless (> start (point-max))
    (list (line-number-at-pos start)
          (1+ (line-number-at-pos end)))))

;;;;; Columns

(defun nt-line->end-col (line)
  "Get end col of LINE."
  (save-excursion (goto-char (nt-line->end line)) (current-column)))

;; Colapse below two functions
(defun nt-line->indent (line)
  "Get indentation col of LINE."
  (save-excursion (nt-line--goto line) (nt-line->indent-col)))

(defun nt-line->indent-col (&optional n)
  "Get indentation col of line forward N-1 times, if N is given."
  (save-excursion (end-of-line n) (current-indentation)))

;;;;; Misc

(defun nt-line->idx (line)
  "Convert 1-idxed LINE to 0-idxed IDX."
  (and line (> line 0)
       (1- line)))

;;;; Predicates

(defun nt-line--empty? (line)
  "Is LINE empty?"
  (apply #'= (nt-line->region line)))

;;;; Methods

(defun nt-line--goto (line)
  "Non-interactive version of `goto-line'."
  ;; TODO Add a check that line isn't too far outside buffer
  (forward-line (- line (line-number-at-pos))))

(defun nt-line-move-visual (&optional count goal-col)
  "Perform `line-move-visual' optionally forcing GOAL-COL.

Returns `temporary-goal-column', not necessarily the same as
`current-column', after moving 1 or COUNT lines."

  ;; `line-move-visual' isn't meant to be called consecutively at lisp level.
  ;; But we have to - so we save and restore the `temporary-goal-column' when
  ;; needed, and the bookkeeping supporting that, namely `last-command'.

  (setq count (or count 1))

  (when goal-col
    (setq temporary-goal-column goal-col)
    (setq last-command (if (> count 1) #'next-line #'previous-line)))

  (line-move-visual count 'noerror)
  temporary-goal-column)

;;;; Macros

(defmacro nt-lines--foreach (start-line end-line &rest body)
  "Execute BODY on each line within optionally bounded [START-LINE END-LINE)."
  (declare (indent 2))
  `(save-excursion
     (nt-line--goto (or ,start-line 1))

     (while (and (not (eobp))
                 (if ,end-line (< (line-number-at-pos) ,end-line) t))
       ,@body
       (forward-line))))

(defvar nt--move-up? nil "When true `nt-line-move-visual-while' will move up.")
(defmacro nt-line-move-visual-while (pred &rest body)
  "Perform `line-move-visual' maintaining goal column while PRED evals true.

If `nt--move-up?' is non-nil, move upwards in buffer instead."
  (declare (indent 1))
  `(let ((orig-goal-col temporary-goal-column)
         (goal-col (nt-line-move-visual)))
     (while (and (not (if nt--move-up? (bobp) (eobp)))
                 ,pred)
       ,@body
       (nt-line-move-visual (if nt--move-up? -1 1) goal-col))

     (setq temporary-goal-column orig-goal-col)))

;;; Points

(defun nt-pos->col (pos)
  "Get column of POS."
  (save-excursion (goto-char pos) (current-column)))

(defun nt--before-indent? ()
  "Is point before `current-indentation'?"
  (< (current-column) (current-indentation)))

;;; Provide

(provide 'nt-base)

;;; nt-base.el ends here
