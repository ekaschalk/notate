;;; nt-base.el --- Common requires and utilities -*- lexical-binding: t; -*-

;; Copyright © 2019 Eric Kaschalk <ekaschalk@gmail.com>

;;; Commentary:

;; Collect pkg-wide imports and utils.

;; Members of this file are not required to be prefixed with `nt-base--' as other
;; files are (by convention).

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
  (list (line-number-at-pos start)
        (1+ (line-number-at-pos end))))

;;;;; Indentation

(defun nt-line->indent (line)
  "Get indentation col of LINE."
  (save-excursion (nt-line--goto line) (nt-line->indent-col)))

(defun nt-line->indent-col (&optional n)
  "Get indentation col of line forward N-1 times, if N is given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

;;;;; Misc

(defun nt-line->idx (line)
  "Convert 1-idxed LINE to 0-idxed IDX."
  (and line (> line 0)
       (1- line)))

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

;;; Provide

(provide 'nt-base)

;;; nt-base.el ends here
