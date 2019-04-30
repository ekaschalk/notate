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

(defun nt-line--nonempty? (line)
  "Is LINE nonempty?"
  (not (nt-line--empty? line)))

;;;; Methods

(defun nt-line--goto (line)
  "Non-interactive version of `goto-line'."
  ;; TODO Add a check that line isn't too far outside buffer
  (forward-line (- line (line-number-at-pos))))

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

;; Is there a better way to condition the macro than a defvar?
(defvar nt--move-up? nil "When true `nt-line-move-visual-while' will move up.")
(defmacro nt-line-move-visual-while (pred &rest body)
  "Perform `line-move-visual' maintaining `goal-column' while PRED is non-nil.

If `nt--move-up?' is non-nil, move upwards in buffer instead.

`line' is bound to the current line, available to the predicate and body."
  (declare (indent 1))
  `(-let* ((count (if nt--move-up? -1 1))
           ((goal-column)  ; See `goal-column', only need car of the temp col
            (progn (line-move-visual count 'noerror) temporary-goal-column))
           (line (line-number-at-pos)))
     (while (and (not (if nt--move-up? (bobp) (eobp)))
                 ,pred)
       ,@body
       (line-move-visual count 'noerror)
       (if nt--move-up? (cl-decf line) (cl-incf line)))))

;;; Points

(defun nt-pos->col (pos)
  "Get column of POS."
  (save-excursion (goto-char pos) (current-column)))

(defun nt--before-indent? ()
  "Is point before `current-indentation'?"
  (< (current-column) (current-indentation)))

(defun nt--depth-at-point ()
  "Get parenthetical (syntax-table-based) depth at point."
  (car (syntax-ppss)))

;;; Provide

(provide 'nt-base)

;;; nt-base.el ends here
