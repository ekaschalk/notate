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

;;;;; Indentation

(defun nt-line->indent (line)
  "Get indentation col of LINE."
  (save-excursion (nt-line--goto line) (nt-line->indent-col)))

(defun nt-line->indent-col (&optional n)
  "Get indentation col of line forward N-1 times, if N is given."
  (save-excursion (end-of-line n) (back-to-indentation) (current-column)))

;;;; Methods

(defun nt-line--goto (line)
  "Non-interactive version of `goto-line'."
  ;; TODO Add a check that line isn't too far outside buffer
  (forward-line (- line (line-number-at-pos))))

;;; Provide

(provide 'nt-base)

;;; nt-base.el ends here
